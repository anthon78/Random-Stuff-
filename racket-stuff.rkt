
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
(define ones (lambda () (cons 1 ones)))

;; put your code below

          ;;;     ;;;
;          O       O 
;              >
        ;;           ;;
         ;;         ;;
          ;;       :;
           ;;;;;;;;;


;1 sequence
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))


;2 string-append-map
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))


;3 list-nth-mod 
(define (list-nth-mod xs n)
  (cond ((< n 0) (raise (error "list-nth-mod: negative number")))
        ((null? xs) (raise (error "list-nth-mod: empty list")))
        (#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i))))))


;4 stream-for-n-steps
(define (stream-for-n-steps s n)
  (cond ((= n 0) null)
        (#t (let ([stream-pair (s)])
          (cons (car stream-pair) (stream-for-n-steps (cdr stream-pair) (- n 1)))))))


;5 funny-number-stream
(define funny-number-stream
         (letrec ([f
                   (lambda (x)
                     (cons (if (= (remainder x 5) 0) (* x -1) x) ;negate x if divisible by 5
                           (lambda () (f (+ x 1)))))])
           (lambda () (f 1))))


;6 dan-then-dog
(define dan-then-dog
  (letrec ([f (lambda (x)
                (let ([y (if (eq? x "dan.jpg") "dog.jpg" "dan.jpg")])
                  (cons y (lambda () (f y)))))])
    (lambda () (f "dog.jpg"))))


;7 stream-add-zero
(define (stream-add-zero s)
  (letrec ([stream-pair (s)])
    (lambda () (cons
                (cons 0 (car stream-pair))
                (stream-add-zero (cdr stream-pair))))))


;8 cycle-lists
(define (cycle-lists xs ys)
  (letrec ([get-item (lambda (x xs) (list-nth-mod xs x))]
        [f (lambda (x)
             (letrec ([x-item (get-item x xs)]
                   [y-item (get-item x ys)])
               (cons
                (cons x-item y-item)
                (lambda () (f (+ x 1))))))])
    (lambda () (f 0))))


;9 vector-assoc
(define (vector-assoc v vec)
  (letrec ([find-match (lambda (vec val i)
                         (cond ((>= i (vector-length vec)) #f)
                               ((not (pair? (vector-ref vec i))) (find-match
                                                           vec val (+ i 1)))
                               ((eq? (car (vector-ref vec i)) val) (vector-ref vec i))
                               (#t (find-match vec val (+ i 1)))))])
    (find-match vec v 0)))


;10









                   
                   


























  






