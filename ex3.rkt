; Functional and Logical Programming 2017 - Ex.3

; Part 1

; A
; receives: a function pred and a list lst
; returns: the elements of lst for which pred does not return #f.
; However, if an element in lst is a list, pred is not applied on it. Instead,
; the function filter-rec is applied on it recursively and operates on the elements of that list.
(define filter-rec (lambda (pred lst)
                      (cond ((null? lst) ())
                            ((list? (car lst)) (cons (filter-rec pred (car lst))(filter-rec pred (cdr lst))))
                            ((pred (car lst)) (cons (car lst) (filter-rec pred (cdr lst))))
                            (else (filter-rec pred (cdr lst))))))
; B
; receives: a function pred and a list lst
; returns: a list with the results of applying pred on each one of the elements of lst.
; However, if an element in lst is a list, pred is not applied on it. Instead, the function
; map-rec is applied on it recursively and operates on the elements of that list.
(define map-rec (lambda (pred lst)
                      (cond ((null? lst) ())
                            ((list? (car lst)) (cons (map-rec pred (car lst))(map-rec pred (cdr lst))))
                            (else (cons (pred (car lst)) (map-rec pred (cdr lst)))))))

; C
(define (filter-rec-tail pred lst)
  ; recursive helper function
  (define (filter-tail lst result)
          ; if the list is empty, return the result
    (cond ((null? lst) result)
          ; if car of the list is a list itself, apply the function on it recursively
          ; and also continue with the rest of the list
          ((list? (car lst)) (filter-tail (cdr lst) (append result (list (filter-rec-tail pred (car lst))))))
          ; if car of the list matches the pred, add it to result
          ; and also continue with the rest of the list
          ((pred (car lst)) (filter-tail (cdr lst) (append result (list (car lst)))))
          ; otherwise, just continue with the rest of the list
          (else (filter-tail (cdr lst) result))))
  (filter-tail lst ()))


; D
; tail recursion version of map-rec 
(define (map-rec-tail pred lst)
  (define (map-helper pred lst result)
    (cond ((null? lst) result)
          ((list? (car lst))
           (map-helper pred (cdr lst) (append result (list (map-rec-tail pred (car lst))))))
          (else (map-helper pred (cdr lst) (append result (list (pred (car lst))))))))
  (map-helper pred lst ()))

; Part 2 - Multiple Arguments 
; A
; The function receives one or more numbers
; The function returns the result of summing all numbers.
(define (sumAll x . others)
  (define (sum-helper lst res)
    (if (null? lst)
        res
        (sum-helper (cdr lst) (+ res (car lst)))))
  (+ x (sum-helper others 0)))

; B
; That receives a binary function f (that receives exactly 2 input parameters),
; and one or more numbers. The function applies f on all the numbers,
; as follows: first, it applies f on x and the next number. Then, it applies f on the result and the next number, and so on.
(define (doFonAll f x . others)
  (define (doFonAll-helper nums res)
    (if (null? nums)
        res
        (doFonAll-helper (cdr nums) (f res (car nums)))))
  
      (doFonAll-helper others x))

; Part 3 - Multiple Arguments 
; A
; receives: a binary function f
; returns: a function that receives one or more numbers.
; The returned function should apply f on the numbers it receives just like doFonAll

(define makeDoFonAll (lambda (f)
                       (lambda (x . others)
                         (define (doFonAll-helper nums res)
                           (if (null? nums)
                               res
                               (doFonAll-helper (cdr nums) (f res (car nums)))))
                         (doFonAll-helper others x))))

; B
; I
; receives: a binary function f
; returns: a function that receives one or more numbers.
; The returned function should apply f on the numbers it receives just like doFonAll did

(define sumAllF (makeDoFonAll +))

; II
; receives: some numbers
; returns: the sum of all the odd numbers among them
(define sumOdds (makeDoFonAll (lambda (curr next)
                                (if (odd? next)
                                    ; next num is odd
                                    (+ curr next)
                                    ; else - next num is not odd
                                    curr))))
                          