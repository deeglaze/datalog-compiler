#lang racket

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

(define intern-table (make-weak-hash))
(struct formula (key) #:transparent
        #:methods gen:equal+hash
        [(define (equal-proc x y rec) (= (formula-key x) (formula-key y)))
         (define (hash-proc x rec) (rec (formula-key x)))
         (define (hash2-proc x rec) (rec (formula-key x)))])

;; The formula constructors are structurally ordered as pred < conj < disj < neg < ∃
(struct pred formula (name terms) #:transparent)
(struct conj formula (Fs) #:transparent)
(struct disj formula (Fs) #:transparent)
(struct neg formula (F) #:transparent)
(struct ∃ formula (s) #:transparent)

;; Intern if unseen. Return canonical representation if seen.
(define-syntax-rule (mk-intern name constructor args ...)
  (define (name args ...)
    (define clause (list 'constructor args ...))
    (cond
     [(hash-has-key? intern-table clause)
      (hash-ref intern-table clause)]
     [else
      (define F (constructor (hash-count intern-table) args ...))
      (hash-set! intern-table clause F)
      F])))

(mk-intern *pred pred name terms) ;; terms is a list.
(mk-intern *conj conj Fs) ;; Fs is a list
(mk-intern *disj disj Fs) ;; Fs is a list
(mk-intern *neg neg F)
(mk-intern *∃ ∃ s)

;; Terms
(struct BVar (db) #:transparent)
(struct FVar (name) #:transparent)

(struct Scope (n f) #:transparent)

(define (atom? x)
  (or (boolean? x) (number? x) (string? x) (symbol? x)))

;; (rule head (Scope n F)) stands for `head(x1,...,xn) <- F.` for some names xi
(struct rule (head s) #:transparent)

(define (lex ord ls0 ls1)
  (define n0 (length ts0))
  (define n1 (length ts1))
  (or (< n0 n1)
      (and (= n0 n1)
           (let lex ([ls0 ls0] [ls1 ls1])
             (match* (ls0 ls1)
               [('() '()) #f]
               [((cons l0 ls0) (cons l1 ls1))
                (or (ord l0 l1)
                    (and (equal? l0 l1)
                         (lex ls0 ls1)))])))))

(define (term<? t0 t1)
  (match* (t0 t1)
    [((pred _ m ts0) (pred _ n ts1))
     (or (symbol<? m n)
         (and (symbol=? m n)
              (lex term<? ts0 ts1)))]
    [((BVar i) (BVar j)) (< i j)]
    [((FVar x) (FVar y)) (symbol<? x y)]
    ;; atom < BVar < FVar < pred
    [(_ (? pred?)) (not (pred? t0))]
    [((? BVar?) (? FVar?)) #t]
    [((? atom? a0) _)
     (if (atom? t1)
         (atom<? a0 t1)
         #t)]
    [(_ _) #f]))

;; pred < ∃ < neg < disj < conj
(define (formula<? f0 f1)
  (match* (f0 f1)
    [((conj _ fs0) (conj _ fs1))
     ;; don't worry, it's well-founded.
     (define fs0* (remove-sorted-duplicates (sort fs0 formula<?)))
     (define fs1* (remove-sorted-duplicates (sort fs1 formula<?)))
     (lex formula<? fs0* fs1*)]
    [((disj _ fs0) (disj _ fs1)) (lex formula<? fs0 fs1)]
    [((neg _ f0) (neg _ f1)) (formula<? f0 f1)]
    [((∃ _ (and s0 (Scope m F0))) (∃ _ (and s1 (Scope n F1))))
     (define names (build-list (min m n) (λ _ (FVar (gensym)))))
     (formula<? (open-scope s0 names) (open-scope s1 names))]
    [((? pred? p0) (? pred? p1)) (term<? p0 p1)]
    ;; injection orderings
    [(_ (? conj?)) #t]
    [((or (? pred?) (? ∃?) (? neg?)) (? disj?)) #t]
    [((or (? pred?) (? ∃?)) (? neg?)) #t]
    [((? pred?) (? ∃?)) #t]
    [(_ _) #f]))

;; #f < #t < number < string < symbol
(define (atom<? a0 a1)
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
        [else #f]))

;; Formula -> Setof Symbol
(define (formula-free f)
  (match f
    [(? pred? p) (term-free p)]
    [(conj _ Fs) (for/union ([F (in-list Fs)]) (formula-free F))]
    [(disj _ Fs) (for/union ([F (in-list Fs)]) (formula-free F))]
    [(neg _ F) (formula-free F)]
    [(∃ _ (Scope _ F)) (formula-free F)]
    [_ (error 'formula-free "Bad formula ~a" f)]))

(define ∅ (seteq))

;; Term -> Setof Symbol
(define (term-free t)
  (match t
    [(pred _ _ ts) (for/union ([t (in-list ts)]) (term-free ts))]
    [(FVar x) (seteq x)]
    [(or (? BVar?) (? atom?)) ∅]
    [_ (error 'term-free "Bad term ~a" t)]))

;; Ideally, we want to canonicalize the set->list operation based on some
;; well-behaved criteria.
(define (close-∃ names f) (*∃ (abstract-names (set->list names) 0 f)))

;; If name in names, replace by (BVar <index in list>+i)
;; Otherwise return #f
(define (abstract-by name names i)
  (match names
    [(cons name* names*)
     (if (equal? name name*)
         (BVar i)
         (abstract-by name names* (add1 i)))]
    [_ #f]))

;; Replace names by deBruijn indices starting at i, offset by position in list.
(define (abstract-names names i f)
  (let abs ([f f] [i i])
    (match f
      [(? pred? p) (abstract-names-in-term names i p)]
      [(conj _ Fs) (*conj (for/list ([F (in-list Fs)]) (abs F i)))]
      [(disj _ Fs) (*disj (for/list ([F (in-list Fs)]) (abs F i)))]
      [(neg _ F) (*neq (abs F i))]
      [(∃ _ (Scope n F)) (*∃ (Scope n (abs F (+ i n))))]
      [_ (error 'abstract-names "Bad formula ~a" f)])))

(define (abstract-names-in-term names i t)
  (let self ([t t])
    (match t
      [(FVar x) (or (abstract-by x names i) t)]
      [(pred _ name ts) (*pred name (map self ts))]
      [(or (? BVar?) (? atom?)) t]
      [_ (error 'abstract-names-in-term "Bad term ~a" t)])))

(define (open-scope S terms)
  (match-define (Scope n F) S)
  (define num (length terms))
  (when (> num n) (error 'open-scope "Scope has ~a binders, but given ~a substitutions" n num))
  ;; It's possible the scope is too big and will never try to instantiate past the size of terms.
  (when (< num n)
    (log-info (format
               "open-scope called with fewer substitutions (~a) than binders (~a)."
               num n)))
  (let open ([f F] [i 0])
    (match f
      [(? pred? p) (subst-indices-in-term terms i p)]
      [(conj _ Fs) (*conj (for/list ([F (in-list Fs)]) (open F i)))]
      [(disj _ Fs) (*disj (for/list ([F (in-list Fs)]) (open F i)))]
      [(neg _ F) (*neq (open F i))]
      [(∃ _ (Scope n F)) (*∃ (Scope n (open F (+ i n))))]
      [_ (error 'open-scope "Bad formula ~a" f)])))

(define (subst-indices-in-term terms i p)
  (let self ([t t])
    (match t
      [(BVar i*)
       (define index (- i* i))
       (if (< index 0)
           t
           (let find ([terms terms] [ind index])
             (match terms
               [(cons t terms) (if (= 0 ind) t (find terms (sub1 ind)))]
               ;; none left
               [_ t])))]
      [(pred _ name ts) (*pred name (map self ts))]
      [(or (? FVar?) (? atom?)) t]
      [_ (error 'abstract-names-in-term "Bad term ~a" t)])))

;; Formula (Setof Symbol) -> Formula
;; Given a top-level formula, add existential quantification at either the top level,
;; or immediately under a negation, of free names and explicitly bound existentials.
(define (lift-existentials f bound)
  (define-values (f* names) (splat-existentials f bound))
  (close-∃ names f*))

;; Formula (Setof Symbol) -> (values Formula (Setof Symbol))
(define (splat-existentials f bound)
  (match f
    [(? pred? p) (values p (set-subtract (term-free p) bound))]
    [(conj _ Fs)
     (define-values (rev-Fs* names)
       (for/fold ([rev-Fs* '()] [names ∅]) ([F (in-list Fs)])
         (define-values (F* names*)
           (splat-existentials F (set-union bound names)))
         (values (cons F* rev-Fs*) (set-union names names*))))
     (values (*conj (reverse rev-Fs*)) names)]
    [(disj _ Fs)
     (define-values (rev-Fs* names)
       (for/fold ([rev-Fs* '()] [names ∅]) ([F (in-list Fs)])
         (define-values (F* names*)
           (splat-existentials F (set-union bound names)))
         (values (cons F* rev-Fs*) (set-union names names*))))
     (values (*disj (reverse Fs*)) names)]
    ;; negations don't leak any splatted existentials, but any free variables are lifted.
    [(neg _ F)
     (define names (formula-free F))
     (values (*neg (lift-existentials F (set-union bound names))) names)]
    [(∃ _ (and s (Scope n F)))
     (define names (make-names n (set-union bound (formula-free F))))
     (define-values (F* names*)
       (splat-existentials (open-scope S names) (set-union bound names)))
     (values F* (set-union names names*))]))
