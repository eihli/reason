#lang racket/base

(require
 first-order-miniKanren/microk-fo
 first-order-miniKanren/tools
 first-order-miniKanren/math)

(run 20 (q)
     (eval-expo q '() q))

(run* (q)
      (*o (build-num 2) (build-num 3) q))

(define-relation (number-betweeno low hi n)
  (fresh (y)
         (<=o low n)
         (<o n hi)
         (pluso low y n)))

(run 5 (body env rand)
     (fresh ()
            ;; (eval-expo
            ;;  `(app (lambda (*o (var ()) (var (s)) ,body)) ,rand)
            ;;  env
            ;;  (build-num 2))
            (eval-expo
             `(app (lambda (*o (var ()) (var (s)) ,body)) ,rand)
             env
             (build-num 3))))

(run* (q)
      (*o (build-num 2) (build-num 3) q))


(run* (q)
      (fresh (n m)
             (*o n m (build-num 6))
             (== `(,n ,m) q)))

(build-num 5)

; 1+
;; (run 10 (q body rand)
;;      (eval-expo
;;       `(app
;;         (lambda ,body)
;;         ,rand)
;;       '()
;;       q))

;; (run 1 (q)
;;      (fresh (body rand env value)
;;             (== env `(,(build-num 1) (build-num 2)))
;;             (== value (build-num 3))
;;             (== q `(app (lambda ,body) ,rand))
;;             (eval-expo q env value)
;;             (eval-expo q `(,(build-num 1) (build-num 1)) (build-num 2))
;;             ))

;; (run 1 (q)
;;      (fresh (body)
;;             (== q `(lambda ,body))
;;             (eval-expo `(app ,q (quote ,(build-num 1)))
;;                        `(,(build-num 2))
;;                        (build-num 2))
;;             (eval-expo `(app ,q (quote ,(build-num 3)))
;;                        `(,(build-num 5))
;;                        (build-num 15))))
