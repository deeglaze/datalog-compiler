#lang racket
(provide deduplicate-rules
         simplify-formula
         formula-≡?)
(require "nauty.rkt"
         "terms.rkt"
         data/union-find
         (only-in unstable/list check-duplicate)
         ffi/unsafe ffi/cvector)
#|
A datalog language with unordered existential quantification.

Conjunctions may be reordered and de-duplicated without consequence.

Problem: determine α-equivalence of rules modulo head name,
         existential reordering,
         existential lifting, and
         conjunction deduplication and reordering.

Remark: When predicates are shown to be equivalent, then this can be solved
        in a fixed-point computation to consider equivalent predicates.

Remark: Reorderable binding and associative/commutative constructors makes this
        problem graph-isomorphism-complete.

Remark: the usual solution to canonicalizing unordered binders is to bind in
appearance order. This doesn't work here since conjunctions are unordered.
A zipper is a structured path through a formula to a hole that represents a name.
The zipper structure is similarly agnostic to structure.
When we see a formula like
  H(y),H(x)
then the zippers to y and x are conjHole({H(x)}) and conjHole({H(y)}) respectively.

Phase 1: lift existentials.
Phase 2: encode formulae as graphs
Phase 3: Decide

Examples:
The following three rules are equivalent:
p1(x,y) <- H(x,y),F(z),H(x,y),G(w)
p2(y,x) <- G(z),F(w),H(y,x)
p3(y,x) <- G(w),F(z),H(y,x)

The following two rules are equivalent (existential lifting):
p1(x) <- (∃ y. H(x,y)),(∃ z. G(x,z))
p1(x) <- G(x,z),H(x,y)

The following two rules are /not/ equivalent, since disjunctions are ordered:
p1(x) <- H(x),F(z) ‌\/ H(y)
p1(x) <- H(x),H(y) \/ F(z)

|#

;; From paper:
;; frontier_f(t) = { frontier_f(a1) @ ... @ frontier_f(an) if t ≡ f(a1,...,an)
;;                   [t] otherwise
;; However we only have binary constructors: conj and disj.
(define (bin-frontier pred? left right)
  (define (self t)
    (if (pred? t)
        (append (self (left t)) (self (right t)))
        (list t)))
  self)
(define conj-frontier (bin-frontier conj? conj-f0 conj-f1))
(define disj-frontier (bin-frontier disj? disj-f0 disj-f1))

;; Formula (Setof Symbol) -> Formula
;; Given a top-level formula, add existential quantification at either the top level,
;; or immediately under a negation, of free names and explicitly bound existentials.
(define (lift-existentials f [bound ∅])
  (define-values (f* names) (splat-existentials f bound))
  (close-∃ names f*))

;; Formula (Setof Symbol) -> (values Formula (Setof Symbol))
(define (splat-existentials f bound)
  (match f
    [(? pred? p) (values p (set-subtract (term-free p) bound))]
    [(or (and (conj f0 f1) (app (λ _ conj) mk))
         (and (disj f0 f1) (app (λ _ disj) mk)))
     (define-values (f0* names) (splat-existentials f0 bound))
     (define-values (f1* names*) (splat-existentials f1 (set-union bound names)))
     (values (mk f0* f1*) (set-union names names*))]
    ;; negations don't leak any splatted existentials, but any free variables are lifted.
    [(neg F)
     (define names (formula-free F))
     (values (neg (lift-existentials F (set-union bound names))) names)]
    [(∃ (and s (Scope F)))
     (define name (variable-not-in (set-union bound (formula-free F))))
     (define-values (F* names*)
       (splat-existentials (open-scope s (FVar name)) (set-add bound name)))
     (values F* (set-add names* name))]
    [_ (error 'splat-existentials "Bad formula: ~a" f)]))

(define (right-associate bin lst)
  (let self ([L lst])
    (match L
      [(list a) a]
      [(cons a more)
       (bin a (right-associate bin more))]
      ['() (error 'right-associate "Empty list illegal: ~a" bin)])))

(define (simplify-formula f [uf #hasheq()])
  (let self ([f f])
    (match f
      [(pred head ts)
       (match (hash-ref uf head #f)
         ;; Use the equivalence class's representative for a given predicate
         [(? uf-set? s) (pred (uf-find s) ts)]
         [_ f])]
      [(? conj?) ;; remove any equivalent formulae
       (right-associate conj (deduplicate (map self (conj-frontier f))))]
      [(? disj?)
       (right-associate disj (deduplicate (map self (disj-frontier f))))]
      [(neg (neg F)) (self F)] ;; XXX: not sure if DNE is valid in datalog.
      [(neg F) (neg (self F))]
      [(? ∃?)
       (define-values (names f*) (open-existentials f))
       (close-∃ names (self f*))]
      [_ (error 'simplify-formula "Bad formula: ~a" f)])))

(define (simplify-rule r [uf #hasheq()])
  (match-define (rule head arity s) r)
  (define-values (rev-names f) (open-scopes s))
  (define head*
    (match (hash-ref uf head #f)
      [(? uf-set? s) (uf-find s)]
      [_ head]))
  (rule head* arity (abstract-names rev-names (simplify-formula f uf))))

;; This is the top level rule grinder. We find pairs of equivalent predicates,
;; union them, and continue to simplify until all simplifications yield equivalent results.
(define (deduplicate-rules rs)
  (define found-union? #f)
  (define uf-map (make-hasheq))
  ;; Create equivalence classes for predicates
  (for ([r (in-list rs)])
    (define head (rule-head r))
    (when (hash-has-key? uf-map head)
      (error 'deduplicate-rules "Duplicate predicate definition: ~a" head))
    (hash-set! uf-map head (uf-new head)))

  (define (dedup rs)
    (match rs
      ['() '()]
      [(list r) rs]
      [(cons r rs*)
       (define drs (dedup rs*))
       (match (compare-and-reduce r drs)
         [#f (cons (simplify-rule r uf-map) drs)]
         [same
          (uf-union! (hash-ref uf-map (rule-head r))
                     (hash-ref uf-map same))
          (set! found-union? #t)
          (cons (simplify-rule r uf-map) drs)])]))

  ;; Simplify until no more equivalent rules are found.
  (let go ([rs rs])
    (define rs* (dedup rs))
    (if found-union?
        (begin (set! found-union? #f)
               (go rs*))
        rs*)))

;; At most one rule of rs can be equivalent since rs is deduped already.
(define (compare-and-reduce r rs)
  (match-define (rule _ arity f) r)
  (define of (open-scopes f))
  (let search ([rs rs])
   (match rs
     ['() #f]
     [(cons (rule head arity* f*) rs)
      (if (and (= arity arity*)
               (formula-≡? of (open-scopes f*)))
          head
          (search rs))])))

;; Use graph isomorphism to determine identity. Can be optimized to do
;; lighter weight analysis before going to nauty.
(define (deduplicate lst)
  (match lst
    [(list a) lst]
    [(cons a more)
     (define lst* (deduplicate more))
     (if (for/or ([b (in-list lst*)]) (formula-≡? a b))
         lst*
         (cons a lst*))]
    ['() (error 'deduplicate "Empty list")]))

;; Formulae can be interned, and free names canonically chosen,
;; to speed up easy equivalence problems.
(define (formula-≡? f0 f1 [bound ∅])
  (or (equal? f0 f1)
      (let ([f0* (lift-existentials f0 bound)]
            [f1* (lift-existentials f1 bound)])
        (or (equal? f0* f1*)
            (LDGs-isomorphic? (formula->LDG f0*) (formula->LDG f1*))))))

;; Create the labeled, directed graph that we will compare for isomorphism
(struct LDnode (label) #:mutable)
(struct LDedge (label node-pointer) #:mutable)
;; A labeled directed graph is a
;; Hasheq[LDnode,List[LDedge]]
(define (formula->LDG f)
  (define G (make-hasheq))

  (define (mk-LDnode label)
    (define n (LDnode label))
    (hash-set! G n '())
    n)

  ;; find all nodes `n` labeled with (FVar x) where `x` in domain of subst.
  ;; Add a directed edge from subst(x) to `n`, and unlabel `n`.
  (define (link-and-unlabel! root subst)
    (define seen (mutable-seteq))
    (let go ([n root])
      (unless (set-member? seen n)
        (set-add! seen n)
        (match (LDnode-label n)
          [(FVar x) ;; n is a reference node.
           (match (hash-ref subst x #f)
             [#f (void)]
             ;; x has a binder node we must link up.
             [binder-n
              (hash-set! G binder-n
                         (cons (LDedge #f n) (hash-ref G binder-n)))
              ;; Binder edge added, so the reference is closed.
              (set-LDnode-label! n #f)])]
          [_ (void)])
        (for-each (compose go LDedge-node-pointer) (hash-ref G n '())))))

  ;; drop the root node on the ground. We don't need it.
  (let self ([f f])
    ;; Create and link subgraphs of formula and return root node of subgraph.
    (match f
      [(pred head ts)
       (define n (LDnode head))
       (hash-set! G n (for/list ([t (in-list ts)]
                                  [i (in-naturals)])
                        (LDedge i (mk-LDnode t))))
       n]
      ;; Associative and commutative
      [(? conj? f)
       (define n (LDnode 'and))
       (hash-set! G n (for/list ([t (in-list (conj-frontier f))])
                        (LDedge #f (self t))))
       n]
      ;; Associative
      [(? disj? f)
       (define n (LDnode 'or))
       (hash-set! G n (for/list ([t (in-list (disj-frontier f))]
                                 [i (in-naturals)])
                        (LDedge i (self t))))
       n]
      [(neg F)
       (define n (LDnode 'neg))
       (hash-set! G n (list (LDedge 0 (self F))))
       n]
      [(? ∃?)
       ;; Open all existentials for associativity.
       (define-values (names opened) (open-existentials f))
       (define n (LDnode '∃))
       (define name-nodes (for/list ([_ (in-list names)]) (mk-LDnode #f)))
       (define subst (for/hasheq ([name (in-list names)]
                                  [n (in-list name-nodes)])
                       (values name n)))
       (define subgraph (self opened))
       (link-and-unlabel! subgraph subst)
       (hash-set! G n (cons (LDedge #f subgraph)
                            (for/list ([nn (in-list name-nodes)])
                              ;; unordered for commutivity
                              (LDedge #f nn))))
       n]
      [_ (error 'formula->LDG "Bad formula: ~a" f)]))
  G)

;; #f < symbol < (FVar x) < (Const #f) < (Const #t) < (Const number) < (Const string) < (Const symbol)
;; XXX: consider moving this to terms for a more canonical interning strategy.
(define (atom<? a0 a1)
  (match* (a0 a1)
    [((Const a0) (Const a1))
     (cond [(and (symbol? a0) (symbol? a1)) (symbol<? a0 a1)]
           [(and (string? a0) (string? a1)) (string<? a0 a1)]
           [(and (number? a0) (number? a1))
            (cond [(and (real? a0) (real? a1)) (< a0 a1)]
                  [(real? a0) #t]
                  [(real? a1) #f]
                  [else (or (< (real-part a0) (real-part a1))
                            (and (= (real-part a0) (real-part a1))
                                 (< (imag-part a0) (imag-part a1))))])]
           [(and (boolean? a0) (boolean? a1))
            (not (or (equal? a0 a1) a0))]
           [(symbol? a1) (not (symbol? a0))]
           [(string? a1) (or (boolean? a0) (number? a0))]
           [(number? a1) (boolean? a0)]
           [else #f])]
    [((FVar x) (FVar y)) (symbol<? x y)]
    [((? FVar?) (? Const?)) #t]
    [((? symbol?) _)
     (if (symbol? a1)
         (symbol<? a0 a1)
         (or (FVar? a1) (Const? a1)))]
    [(#f (not #f)) #t]
    [(_ _) #f]))

(define (opreal<? or0 or1)
  (if (and (real? or0) (real? or1))
      (< or0 or1)
      (and (not or0) or1)))

(define (same-labels? G0 G1)
  (define (vertex-labels G)
    (map LDnode-label (hash-keys G)))
  (define (edge-labels G)
    (map LDedge-label (append* (hash-values G))))
  (and (equal? (sort (vertex-labels G0) atom<?)
               (sort (vertex-labels G1) atom<?))
       (equal? (sort (edge-labels G0) opreal<?)
               (sort (edge-labels G1) opreal<?))))

;; A directed graph is a Hash[ℕ,[Set[ℕ]]]
;; We assign numbers to LDnode by pointer equality
(define (identify G)
  (for/hasheq ([n (in-hash-keys G)]
               [i (in-naturals)])
    (values n i)))

;; Return Hash[LDnode,ℕ+] counting the number of edges to particular nodes.
(define (edge-multiplicity lst)
  (define M (make-hasheq))
  (for ([e (in-list lst)])
    (match-define (LDedge _ n) e)
    (hash-set! M n (add1 (hash-ref M n 0))))
  M)

#|
 For any two nodes that have multiple edges between them, add breaker nodes.
   ___________              _____._____
  /           \            /           \
* ------------- *   ==>  * ------•------ *
  \___________/            \_____._____/

|#
(define (LDG->DG LDG)
  (define I (identify LDG))
  (define DG (make-hash))
  (for ([i (in-hash-values I)])
    (hash-set! DG i (set)))
  (for* ([(n es) (in-hash LDG)]
         [ni (in-value (hash-ref I n))]
         [(n* mult) (in-hash (edge-multiplicity es))]
         [n*i (in-value (hash-ref I n*))])
    (cond [(> mult 1)
           ;; hash-count is the next node number, so the interval
           ;; [hash-count,hash-count+mult)
           ;; is the next range of node indices to use.
           (define end (hash-count DG))
           (for ([j (in-range mult)]
                 [idx (in-naturals end)])
             (hash-set! DG ni (set-add (hash-ref DG ni) idx))
             (hash-set! DG idx (set-add (hash-ref DG idx) n*i)))]
          [else
           (hash-set! DG ni (set-add (hash-ref DG ni) n*i))]))
  DG)

(define (LDGs-isomorphic? G0 G1)
  (and (same-labels? G0 G1)
       (adjs-isomorphic? (LDG->DG G0) (LDG->DG G1))))

#|
Directed graph to undirected graph widget:
                             . m2
                             |
* ---> *    ==>  * --- * --- * --- *
n      n*        n     m0    m1    n*

This code is only necessary if it turns out nauty digraph canonicalization is buggy.
The file interface to nauty is undirected only, so there might be cause to worry.
It's better to not blow graphs up a factor of 4, so I use directed graphs for now.

|#
(define (DG->UG DG)
  (define UG (make-hash))
  (for ([i (in-hash-keys DG)])
    (hash-set! UG i (set)))
  (define (diedge! i j)
    (hash-set! UG i (set-add (hash-ref UG i (set)) j)))
  (define (edge! i j)
    (diedge! i j)
    (diedge! j i))
  (for* ([(n S) (in-hash DG)]
         [n* (in-set S)])
    (define m0 (hash-count UG))
    (define m1 (+ 1 m0))
    (define m2 (+ 1 m1))
    (edge! n m0)
    (edge! m0 m1)
    (edge! m1 n*)
    (edge! m1 m2))
  UG)

(define (DGs-isomorphic? G0 G1)
  (adjs-isomorphic? (DG->UG G0) (DG->UG G1)))

(module+ test
  (require rackunit)

  #|
  A little parser for an s-expression form of the formulas and rules.
  |#

  (define (sexp->term sexp)
    (match sexp
      [(? symbol? x) (FVar x)]
      [(or `(quote ,(? atom? a))
           (? atom? a)) (Const a)]
      [_ (error 'sexp->term "Bad term: ~a" sexp)]))

  (define (sexp->formula sexp)
    (define ((is-named lst) x) (memq x lst))
    (define (self sexp)
      (match sexp
        [`(,(? (is-named '(and ∧))) ,fs ...)
         (right-associate conj (map self fs))]
        [`(,(? (is-named '(or ∨))) ,fs ...)
         (right-associate disj (map self fs))]
        [`(,(? (is-named '(exists ∃))) (,(? symbol? evar) ...) ,f)
         (define dup (check-duplicate evar))
         (when dup (error 'sexp->formula "Duplicate binder in formula: ~a" dup))
         (close-∃ evar (self f))]
        [`(,(? (is-named '(not ¬))) ,f) (neg (self f))]
        [`(,(? symbol? head) ,terms ...)
         (pred head (map sexp->term terms))]))
    (self sexp))

  (define (sexp->rule sexp)
    (match sexp
      [`((,(? symbol? head) ,(? symbol? formal) ...) <- ,F)
       (define dup (check-duplicate formal))
       (when dup (error 'sexp->rule "Duplicate binder in rule: ~a" dup))
       (rule head
             (length formal)
             (for/fold ([s (sexp->formula F (apply seteq formal))])
                 ([name (in-list formal)])
               (abstract-name name s)))]))

  (check equal?
         ;; Rule (foo x y) <- (H x), (G y), (P y z)
         (lift-existentials (sexp->formula `(and (H x) (G y) (P y z)))
                            (seteq 'x 'y))
         (sexp->formula `(∃ (z) (and (H x) (G y) (P y z)))))

  (check equal?
         ;; Rule (foo x y) <- (H x), (∃ (w) (G y w)), (∃ (w) (P y z w))
         (lift-existentials (sexp->formula
                             `(and (H x)
                                   (∃ (w) (G y w))
                                   (∃ (w) (P y z w))))
                            (seteq 'x 'y))
         (sexp->formula `(∃ (z w0 w1) (and (H x) (G y w0) (P y z w1))))))

;; G is an adjacency mapping.
(define (adj->nauty-graph G num-vertices m)
  (nauty_check WORDSIZE num-vertices m NAUTYVERSIONID)

  (define g (make-cvector setword (* num-vertices m)))

  (EMPTYSET0 g (* num-vertices m))
  (for* ([(node adj) (in-hash G)]
         [neighbor (in-set adj)])
    (ADDONEARC g node neighbor m))

  g)

(define (nauty-canonicalize g num-vertices m)
  (define lab1 (make-cvector _int num-vertices))
  (define ptn (make-cvector _int num-vertices))
  (define orbits (make-cvector _int num-vertices))
  (define cg (make-cvector setword (* num-vertices m)))
  (EMPTYSET0 cg (* num-vertices m)) 

  (liason g lab1 ptn orbits m num-vertices cg)
  cg)

(define (adjs-isomorphic? G0 G1)
  (define num-vertices (hash-count G0))
  (cond
   [(= num-vertices (hash-count G1))
    (define m (SETWORDSNEEDED num-vertices))
    (define NG0 (adj->nauty-graph G0 num-vertices m))
    (define NG1 (adj->nauty-graph G1 num-vertices m))
    (define ngc0 (nauty-canonicalize NG0 num-vertices m))
    (define ngc1 (nauty-canonicalize NG1 num-vertices m))
    (dense_compare ngc0 ngc1 num-vertices m)]
   [else #f]))

(module+ test
  (check formula-≡?
         ;; Rule (foo x y) <- (H x), (∃ (w) (G y w)), (∃ (w) (P y z w))
         (sexp->formula `(∃ (z w0 w1) (and (H x) (G y w0) (P y z w))))
         (sexp->formula `(∃ (w0 z w1) (and (G y w0) (P y z w) (H x))))
        )

  (check formula-≡?
         (simplify-formula
          (sexp->formula `(∃ (w0 z w1) (and (G y w0) (G y w0) (P y z w) (H x) (P y z w)))))
         (sexp->formula `(∃ (w0 w1 z) (and (G y w0) (P y z w) (H x))))))
