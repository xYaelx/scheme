; Functional and Logical Programming 2017 - Ex.6

; My Id is 

;stream macro
(defmacro stream-cons (value next)
  `(cons ,value (lambda () ,next)))

(define stream-car car)

(define (stream-cdr stream)
  (if (procedure? (cdr stream))
      ((cdr stream))
      (cdr stream)))

(define (stream-to-list s n)
  (if (= n 0)
      ()
      (cons (stream-car s) (stream-to-list (stream-cdr s) (- n 1)))))
;---
; Class macro
(defmacro class (name members . methods)

  ; Translates a single method definition to a ‘define’
  (define (methodToDefine method)
    `(define (,(cadr method) ,@(caddr method))
       ,(cadddr method)))

  ; Create a list of list-ref calls, according to the
  ; number of parameters
  (define (make-list-refs parameters n)
    (if (null? parameters) ()
        (cons `(list-ref params ,n)
              (make-list-refs (cdr parameters)
                              (+ n 1)))))

  ; Translates a single method definition to a cond case
  (define (methodToCond method)
    `((eq? msg (quote ,(cadr method)))
      (,(cadr method)
       ,@(make-list-refs (caddr method) 0))))

  ; Map function that only keeps public methods
  (define (map-public methods)
    (cond ((null? methods) ())
          ((eq? (car (car methods)) 'public)
           (cons (methodToCond (car methods))
                 (map-public (cdr methods))))
          (else (map-public (cdr methods)))))
  
  ; Main macro body
  `(define (,name ,@members)
     ,@(map methodToDefine methods)
     (define (send msg . params)
       (cond ,@(map-public methods)))
     send))
;==================
; Part 1

(class queue (q)
  
  ; The method enqueue receives a single parameter value, and pushes it into the queue.
  ; It will replace the value of q using the set! command. It does not return any value.
  (public enqueue (value)
          (set! q (append q (list value))))
  
  ; The method dequeue returns the value of the next item to dequeue
  ; It will also use the set! command to update our q so the next dequeue will be correct.
  (public dequeue ()
          (if (null? q)
              ()
              (let ((val (car q)))
                (set! q (cdr q))
                val)))
  
  (public is-empty ()
          (if (null? q)
              #t
              #f)))

; Part 2 - streams
;A
; generates the stream of all the positive odd numbers, starting at 0 and up.
(define (generate-even-stream)
  (define (helper val)
    (stream-cons val (helper (+ val 2))))
  (stream-cons 0 (helper (+ 0 2))))
;B
; generates the stream of a Fibonacci series, starting with 1.
(define (generate-fibo)
  (define (fibo-gen a b)
    (stream-cons a (fibo-gen b (+ a b))))
  (fibo-gen 0 1))

;C
; receives: a positive integer
; returns: a stream of bits that represent this number,
; from the least significant bit to the most significant bit.
(define (generate-bit-stream n)
  (if (= n 0)
      ()
      (stream-cons (modulo n 2) (generate-bit-stream (quotient n 2)))))

; Part 3
; A
;  receives a list and returns a stream with a finite length,
; with the same elements and orders as the list lst.
(define (list-to-stream lst)
  (if (null? lst)
      ()
      (stream-cons (car lst) (list-to-stream (cdr lst)))))
; B
; same as A but infinite stream
(define (list-to-infinite-stream lst)
  (stream-cons (list-to-stream lst) (list-to-infinite-stream (list-to-stream lst))))

; Part 4 - Stream comprehensions
(define (stream-comp action baseStream . conditions)
  (define (checkConds carStream conds)
    (if (null? conds)
        #t
        (if ((car conds) carStream)
            (checkConds carStream (cdr conds))
            #f)))
  (define (helper stream)
  (if (checkConds (stream-car stream) conditions)
      (stream-cons (action (stream-car stream))
                   (helper (stream-cdr stream)))
      (helper (stream-cdr stream))))
  (helper baseStream))