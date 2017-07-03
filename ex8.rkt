; Functional and Logical Programming 2017 - Ex.8

; My Id is 

; Part 1 - backtracking

; @param  books - a list with all volume lengths. The first element is the number
;                 of pages in the first volume, and so on.
; Note: there are no 2 volumes with the exact same length,
;       so the number of pages is also used as an identifier for a volume.
; @return  a list with 2 lists in it: The 1st internal list has all volumes Alice should summarize,
;          and the 2nd list has all volumes Bob should summarize.
;          If there is no even split, the function returns #f.
(define (split-fairly books)
  (define (helper books Alice_stock Bob_stock)
    (cond ((null? books)
           (if (= (apply + Alice_stock) (apply + Bob_stock))
               (list Alice_stock Bob_stock)
               #f))
           (else (or (helper (cdr books) (cons (car books) Alice_stock) Bob_stock)
                     (helper (cdr books) Alice_stock (cons (car books) Bob_stock))))))
    (helper books () ()))

; Part 2 - More backtracking

; @param lst is a list with zero or more real numbers
; @param r is a positive integer. Defines the number of elements from lst, that their sum is exactly 0
; @return The function returns #f if there is no subset of r elements from lst whose sum is 0.
;         If such a subset exists, the function returns its elements in a list.
(define (r-sum lst r)
    (define (helper L x result)
    (cond ((= x 0)
           (if (= (apply + result) 0)
               result
               #f))
          ((null? L) #f)
          (else (or (helper (cdr L) (- x 1) (cons (car L) result))
                    (helper (cdr L) x result)))))
  (helper lst r ()))
  
  
