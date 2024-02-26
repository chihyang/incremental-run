#lang racket

(require "mk-function-run.rkt")

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
            (if (= res-length 1) "" "s")
            (if (> res-length 0) ":" "."))
    (for ((r res))
      (printf "==========================\n")
      (printf "Result: ~a\n" r))
    (newline)))

(define initial-goal
  (lambda (q)
    (call/fresh 'r
      (lambda (r)
        (call/fresh 's
          (lambda (s)
            (conj
              (== `(, r ,s) q)
              (appendo r s '(a b c d e)))))))))

(define next
  (run 4 initial-goal pretty-printer))

(define more
  (next 3))

(define even-more
  (more 1000))
