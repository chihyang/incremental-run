#lang racket

(require "mk.rkt")

(defrel (appendo l1 s2 o)
  (disj
    (conj (== '() l1) (== s2 o))
    (call/fresh 'a
      (λ (a)
        (call/fresh 'b
          (λ (d)
            (call/fresh 'res
              (λ (res)
                (conj
                  (== `(,a . ,d) l1)
                  (== `(,a . ,res) o)
                  (appendo d s2 res))))))))))

(define (pretty-printer res query-n)
  (newline)
  (let ((res-length (length res)))
    (printf "~a/~a result~a returned~a\n"
            res-length query-n
            (if (> res-length 1) "s" "")
            (if (> res-length 0) ":" "."))
    (for ((r res))
      (printf "==========================\n")
      (printf "Result: ~a\n" r))
    (newline)))

(define (simple-printer res query-n)
  (newline)
  (for ((r res)
        (i (length res)))
    (printf "Result ~a/~a" i query-n)
    (printf ": ~a\n" r))
  (newline))

(define initial-goal
  (lambda (q)
    (call/fresh 'r
      (lambda (r)
        (call/fresh 's
          (lambda (s)
            (conj
              (== `(, r ,s) q)
              (appendo r s '(a b c d e)))))))))

(define next-goal
  (run 4 initial-goal pretty-printer))

(define more-goal
  (run 1000 next-goal simple-printer))

(define next-next-goal
  (run 1000 next-goal pretty-printer))

(define no-more-goal
  (run 1000 more-goal pretty-printer))

(define nested-goal
  (run 1000
      (run 1000
          (run 2 initial-goal pretty-printer)
        simple-printer)
    pretty-printer))

(define compose-goal
  (run 2 (lambda (p)
           (conj
             (next-goal p)
             (== p 'g)))
    pretty-printer))

(define compose-goal-flipped
  (run 2 (lambda (p)
           (conj
             (== p 'g)
             (next-goal p)))
    pretty-printer))
