#lang racket
(provide (all-defined-out))

;;; A nearly macro-free purely-functional-style version of
;;; microKanren. Vectors of length 1 are used to represent
;;; variables. This is The Curried microKanren.  Refer to:
;;; https://link.springer.com/chapter/10.1007/978-3-031-38938-2_5
;;; ----------- logic variable -----------
(define (var v)
  (vector v))

(define (var? x)
  (vector? x))
;;; ----------- substitution -----------
(define initial-state '())

;; When ext-s is invoked, x is a fresh variable, v can be
;; any term including a variable, but if it is the variable
;; x, it fails.
(define (ext-S x v S)
  (cond
    ((occurs? x v S) #f)
    (else (cons `(,x . ,v) S))))

;; x is fresh, v is any term, x cannot occur inside v or be
;; v.
(define (occurs? x v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (eqv? v x))
      ((pair? v)
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s)))
      (else #f))))
;;; ----------- operations on substitutions -----------
;;; unify : (-> Any Any Substition (∪ Substitution #f))
; u, v are fresh variables or non-variables (e.g.: list,
; number); success of unification returns a substitution;
; failure returns false.

(define (unify u v S)
  (let ((u (walk u S)) (v (walk v S)))
    (cond
      ((eqv? u v) S)
      ((or (and (var? v) (ext-S v u S))
           (and (var? u) (ext-S u v S))))
      ((and (pair? u) (pair? v))
       ;; The name s^ is for clarity.
       (let ((s^ (unify (car u) (car v) S)))
         (and s^
              (unify (cdr u) (cdr v) s^))))
      (else #f))))

;;; walk : (-> Var Substition (∪ Any Var))
(define (walk v S)
  (let ((a (and (var? v) (assv v S))))
    (cond
      ((pair? a) (walk (cdr a) S))
      (else v))))

;;; ----------- Stream -----------
;;; Stream can be one of:
;;; - '()
;;; - (Substitution . Stream)
;;; - (λ () Stream)
(define (thunk? p)
  (zero? (procedure-arity p)))

;;; ----------- Goal -----------
;;; Goal : Substitution -> Stream
(define (== u v)
  (lambda (S)
    (let ((S^ (unify u v S)))
      (if S^ (list S^) (list)))))

(define succeed (== #f #f))

(define fail (== #f #t))

;;; ----------- Ways to introduce new goals -----------
;;; Way 1: introduce a new variable
;;; because var takes a symbol, now we pass the symbol, the
;;; name of a variable to call/fresh
;;; call/fresh : (-> Str (-> Var Goal) Goal)
(define (call/fresh y f)
  (f (var y)))

;;; Way 2: disjunction of a list of goals
(define ((disj . gs) S)
  (cond
    ((null? gs) (fail S))
    (else (D ((car gs) S) (cdr gs) S))))

(define (D S∞ gs S)
  (cond
    ((null? gs) S∞)
    (else
     (append∞ S∞
       (D ((car gs) S) (cdr gs) S)))))

(define (append∞ S∞ T∞)
  (cond
    ((null? S∞) T∞)
    ((pair? S∞)
     (cons (car S∞)
       (append∞ (cdr S∞) T∞)))
    ;; We could use else here to replace (thunk? S-inf) if
    ;; other languages don't provide procedure-arity.
    ((thunk? S∞)
     (lambda ()
       (append∞ T∞ (S∞))))))

;;; Way 3: conjunction of a list of goals
(define ((conj . gs) S)
  (cond
    ((null? gs) (succeed S))
    (else (C (cdr gs) ((car gs) S)))))

(define (C gs S∞)
  (cond
    ((null? gs) S∞)
    (else
     (C (cdr gs)
        (append-map∞ (car gs) S∞)))))

(define (append-map∞ g S∞)
  (cond
    ((null? S∞) '())
    ((pair? S∞)
     ;; g doesn't change.
     (append∞ (g (car S∞))
                 (append-map∞ g (cdr S∞))))
    (else
     (lambda ()
       (append-map∞ g (S∞))))))

;;; Way 4: only the first pair of goals whose car succeeds
;;; can contribute values
(define ((conda . g*) S)
  (cond
    ((null? g*) '())
    (else (A (cdr g*) ((car g*) S) S))))

;; When we transition to A, we know we have at least one
;; goal.
(define (A g* S∞ S)
  (cond
    ((null? g*) S∞)
    ((null? (cdr g*)) (append-map∞ (car g*) S∞))
    (else (ifs∞te S∞ (car g*) (cdr g*) S))))

(define (ifs∞te S∞ g g+ S)
  (cond
    ((null? S∞) (A (cdr g+) ((car g+) S) S))
    ((pair? S∞) (append-map∞ g S∞))
    (else (λ () (ifs∞te (S∞) g g+ S)))))

;;; Way 5: like conda, except that we consider only the
;;; first result of a successful question
(define ((once g) S)
  (O (g S)))

;;; O returns a Stream that can contain at most one
;;; Substitution.
(define (O S∞)
  (cond
    ((null? S∞) '())
    ;; We return a singleton substitution.
    ((pair? S∞) (cons (car S∞) '()))
    (else (λ () (O (S∞))))))

;;; Way 6: project gives us the value associated with the
;;; var.
;;; project : (-> Var (-> Any Goal) Goal)
(define (project x f)
  (λ (S)
    (let ((x (walk* x S)))
      ((f x) S))))

;;; Way 7: a macro to construct a goal
(define-syntax defrel
  (syntax-rules ()
    ((defrel (name x ...) g ...)
     (define (name x ...)
       (lambda (S)
         (lambda ()
           ((conj g ...) S)))))))

;;; ----------- Ways to run a goal -----------
;;; run : (-> (∪ #f Int) (-> Var Goal) (-> (Listof Any) Any) (-> Var Goal))
; Given a nonnegative integer n, a function fq that takes a
; Var and returns a goal, and a function fr that takes a
; list of results, passes the first n results of unifying
; the goal to fq, then returns a function. The returned
; function takes a Var, and returns a goal. When this goal
; is invoked with run again, it gives the results after the
; first n results from the original goal that fq returns.
(define (run n fq fr)
  (call/fresh 'q
    (lambda (q)
      (run-goal n (fq q) q fr))))

;;; run* : (-> (-> Var Goal) (Listof Any))
; Given a function that takes a Var and returns a goal,
; returns all the results of unifying the goal; if there are
; infinite results, this function goes into an infinite
; loop.
(define (run* fq)
  (call/fresh 'q
    (lambda (q)
      (reify∞* ((fq q) initial-state) (reify q)))))

; Takes a value which is either a variable or non variable
; and after walk* is done, every variable has been replaced
; by its walked* value.

; e.g.:
; v = (x (y h) z (j 5 h))
; s = ((x . 2) (y . 3) (z . 4) (j . k))
; ((reify v) s) = (2 (3 h) 4 (k 5 h))
(define (reify v)
  (lambda (S)
    (let ((v (walk* v S)))
      (let ((S-env (reify-S v initial-state)))
        (walk* v S-env)))))

(define (walk* v S)
  (let ((v (walk v S)))
    (cond
      ((var? v) v)
      ((pair? v)
       (cons
         (walk* (car v) S)
         (walk* (cdr v) S)))
      (else v))))

(define (reify-S v S-env)
  (let ((v (walk v S-env)))
    (cond
      ((var? v)
       (let ((n (length S-env)))
         (let ((rn (reify-name n)))
           (cons `(,v . ,rn) S-env))))
      ((pair? v)
       (let ((S-env^ (reify-S (car v) S-env)))
         (reify-S (cdr v) S-env^)))
      (else S-env))))

(define (reify-name n)
  (string->symbol
    (string-append "_" (number->string n))))

(define (run-goal n g q f)
  (reify∞+ n (g initial-state) q f))

(define (product∞ S∞ T∞)
  (cond
    ((null? T∞) '())
    ((pair? T∞)
     (append∞ (merge∞ (car T∞) S∞)
       (product∞ S∞ (cdr T∞))))
    (else (λ () (product∞ S∞ (T∞))))))

(define (merge∞ S S∞)
  (cond
    ((null? S∞) '())
    ((pair? S∞)
     (let ((S^ (product-S S (car S∞))))
       (if S^
           (cons S^
             (merge∞ S (cdr S∞)))
           (merge∞ S (cdr S∞)))))
    (else (λ () (merge∞ S (S∞))))))

(define (product-S S1 S2)
  (cond
    ((eqv? S1 `()) S2)
    (else
     (let ((x (car (car S1)))
           (v (cdr (car S1)))
           (S1^ (cdr S1)))
       (let ((S2^ (unify x v S2)))
         (if S2^ (product-S S1^ S2^) #f))))))

(define (reify∞+ n S∞ q fr)
  (let ((result (reify∞ n S∞ (reify q) '())))
    (let ((_ (fr (car result) n)))
      (λ (q^)
        (let ((g (== q^ q)))
          (λ (S)
            (product∞ (cdr result) (g S))))))))

(define (reify∞ n S∞ T acc)
  (cond
    ((or (zero? n) (null? S∞)) (cons acc S∞))
    ((pair? S∞)
     (reify∞
      (sub1 n) (cdr S∞) T
      (cons (T (car S∞)) acc)))
    (else (reify∞ n (S∞) T acc))))

(define (reify∞* S∞ T)
  (cond
    ((null? S∞) '())
    ((pair? S∞)
     (cons (T (car S∞))
       (reify∞* (cdr S∞) T)))
    (else (reify∞* (S∞) T))))
