#lang racket/base
#|
The core representation of terms, formulae and rules.

Name manipulation is provided.
|#
(provide
 ;; Formulae
 (struct-out pred)
 (struct-out conj)
 (struct-out disj)
 (struct-out neg)
 (struct-out ∃)
 ;; Terms
 (struct-out BVar)
 (struct-out FVar)
 (struct-out Const)
 atom?
 ;; LN
 (struct-out Scope)
 ;; Rule
 (struct-out rule)
 ;; Constant
 ∅
 ;; Free variables
 formula-free
 term-free
 ;; LN ops
 abstract-name
 abstract-names-in-term
 open-scope
 subst-indices-in-term
 ;; Helpers
 variable-not-in
 abstract-names
 open-scopes
 num-scopes
 close-∃
 open-existentials)
(require racket/match racket/set)

;; Suggestions for better representation:
;; * Memoize free variable computation
;; * Intern formulae to a more canonical form (right-associate and sort ∧,∨)

;; The formula constructors are structurally ordered as pred < conj < disj < neg < ∃
(struct pred (name terms) #:transparent)
(struct conj (f0 f1) #:transparent) ;; associative and commutative
(struct disj (f0 f1) #:transparent) ;; associative
(struct neg (f) #:transparent)
(struct ∃ (s) #:transparent) ;; commutative

;; Terms
(struct BVar (db) #:transparent)
(struct FVar (name) #:transparent)
(struct Const (c) #:transparent)

;; Locally nameless representation tool.
(struct Scope (f) #:transparent)

;; What are allowable constants in our language?
(define (atom? x)
  (or (boolean? x) (number? x) (string? x) (symbol? x)))

;; (rule head (Scope n F)) stands for `head(x1,...,xn) <- F.` for some names xi
(struct rule (head arity s) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Free variables

(define ∅ (seteq))

;; Formula -> Setof Symbol
(define (formula-free f)
  (match f
    [(? pred? p) (term-free p)]
    [(or (conj f0 f1) (disj f0 f1)) (set-union (formula-free f0) (formula-free f1))]
    [(neg F) (formula-free F)]
    [(∃ (Scope F)) (formula-free F)]
    [(Scope F) (formula-free F)]
    [_ (error 'formula-free "Bad formula ~a" f)]))

;; Term -> Setof Symbol
(define (term-free t)
  (match t
    [(pred _ ts) (for/fold ([S ∅]) ([t (in-list ts)]) (set-union S (term-free t)))]
    [(FVar x) (seteq x)]
    [(or (? BVar?) (? Const?)) ∅]
    [_ (error 'term-free "Bad term ~a" t)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locally nameless operations

;; Replace names by deBruijn indices starting at i, offset by position in list.
(define (abstract-name name f) (abstract-name-aux name 0 f))
(define (abstract-names names f)
  (for/fold ([f f]) ([name (in-list names)])
    (abstract-name name f)))

(define (abstract-name-aux name i f)
  (Scope
   (let abs ([f f] [i i])
     (match f
       [(? pred? p) (abstract-names-in-term name i p)]
       [(conj f0 f1) (conj (abs f0 i) (abs f1 i))]
       [(disj f0 f1) (disj (abs f0 i) (abs f1 i))]
       [(neg F) (neg (abs F i))]
       [(∃ s) (∃ (abs s i))]
       [(Scope F) (Scope (abs F (add1 i)))]
       [_ (error 'abstract-names "Bad formula ~a" f)]))))

(define (abstract-names-in-term name i t)
  (let self ([t t])
    (match t
      [(FVar x) (if (equal? x name) (BVar i) t)]
      [(pred name ts) (pred name (map self ts))]
      [(or (? BVar?) (? Const?)) t]
      [_ (error 'abstract-names-in-term "Bad term ~a" t)])))

(define (open-scope S term) (open-scope-aux S term 0))

(define (open-scope-aux S term i)
  (match-define (Scope F) S)
  (let open ([f F] [i i])
    (match f
      [(? pred? p) (subst-indices-in-term term i p)]
      [(conj f0 f1) (conj (open f0 i) (open f1 i))]
      [(disj f0 f1) (disj (open f0 i) (open f1 i))]
      [(neg F) (neg (open F i))]
      [(∃ s) (∃ (open s i))]
      [(Scope F) (Scope (open F (add1 i)))]
      [_ (error 'open-scope "Bad formula ~a" f)])))

(define (subst-indices-in-term term i p)
  (let self ([t p])
    (match t
      [(BVar i*)
       (if (= i i*)
           term
           t)]
      [(pred name ts) (pred name (map self ts))]
      [(or (? FVar?) (? Const?)) t]
      [_ (error 'abstract-names-in-term "Bad term ~a" t)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpful functions for naming and recursively manipulating scopes.

;; Deterministic name choice.
(define (variable-not-in S)
  (let search ([i 0])
    (define s (string->symbol (format "g~a" i)))
    (if (set-member? S s)
        (search (add1 i))
        s)))

;; Ideally, we want to canonicalize the set->list operation based on some
;; well-behaved criteria.
(define (close-∃ names f)
  (define free (formula-free f))
  (for/fold ([f f]) ([name (in-set names)])
    ;; Don't bind if irrelevant.
    ;; This is necessary to allow (∃ (x y) (H x)) ≡ (∃ (y) (H y))
    (if (set-member? free name)
        (∃ (abstract-name name f))
        f)))

(define (open-existentials f)
  (match f
    [(∃ s)
     (define n (variable-not-in (formula-free f)))
     (define-values (names f*) (open-existentials (open-scope s (FVar n))))
     (values (cons n names) f*)]
    [_ (values '() f)]))

;; like open-existentials, but for the top level formulae.
(define (open-scopes s)
  (let open ([s s] [rev-names '()])
    (cond
     [(Scope? s)
      (define name (variable-not-in (formula-free s)))
      (open (open-scope s (FVar name)) (cons name rev-names))]
     [else
      (values rev-names s)])))

(define (num-scopes s)
  (let count ([s s] [num 0])
    (match s
    [(Scope f) (count f (add1 num))]
    [_ num])))
