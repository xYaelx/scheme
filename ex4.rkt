; Functional and Logical Programming 2017 - Ex.4


; Part 1

; A
; When evaluated, the macro calculates the volume of a sphere
; the radius r (= 4/3*π*r*r*r)
(defmacro sphere-volume (r)
  `(* (/ 4 3) (* pi ,r ,r ,r)))
; B
; When evaluated, the macro calculates the maximal value among x and y.
(defmacro max (x y)
  `(let ((x_val ,x)
         (y_val ,y))
     (if (> x_val y_val) 
         x_val 
         y_val)))
; Part 2
; The macro let*-flat works similarly to let*:
; you can use it to define values just as with let,
; and you can use symbols that were defined in the same call to let*-flat when defining additional symbols
(defmacro let*-flat (values body)
  (define (map-items value)
    ; from (((a 1) (b 2) (c (+ a b)) -> to ((define a 1) (define b 2) (define c (+ a b))
    `(define ,(car value) ,(cadr value)))
  ; attaching the body to the mapped symbols
  `(let ()
     ,@(map map-items values)
     ,body))
                            
; Part 3
; A - nor
(defmacro nor (x . allCases)
  (define (expandCase cases)
    (if (null? (cdr cases)) ; got to the last condition
        `(if ,(car cases)
             #f
             #t)
        ;else- go recursively on each case and check if one of them is true
        ;and then the whole expression will be false
        `(if ,(car cases)
             #f
             ,(expandCase (cdr cases)))))
  (expandCase (cons x allCases)))

;B - xor
(defmacro xor (x . allCases)
  ; count how many times a contidion exists(and returns 1)
  `(odd? (+ ,@(map (lambda (y) `(if ,y 1 0)) (cons x allCases)))))