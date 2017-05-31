; Functional and Logical Programming 2017 - Ex.5

; Part 1

; A
; The function creates a Bloom Filter with n bits that are all 0 (zero).
; The filter is represented as a list of binary digits.
; Thus, the function creates a list with n zeros.
; receives: n - defines the size of the filter

(define (bloom-create n)
  (if (eq? n 0) ()
      (cons 0 (bloom-create (- n 1)))))

; B
; receives: bloom - Bloom Filter (list of bits), bit - index of a bit (a number between 0 and n-1).
; returns: a Bloom Filter where the bit at index bit is 1
; (regardless of its previous value) and the rest of the bits remain unchanged.
(define (bloom-set-bit bloom bit)
    (if (= bit 0)
        (cons 1 (cdr bloom))
        (cons (car bloom) (bloom-set-bit (cdr bloom) (- bit 1)))))

; C
; receives: bloom - a Bloom Filter (list of bits), bit - an index of a bit (between 0 and n-1).
; returns: the value of the bit at index bit in bloom (0 or 1).
(define (bloom-get-bit bloom bit)
      (if (= bit 0)
        (car bloom)
        (bloom-get-bit (cdr bloom) (- bit 1))))

; D
; The function inserts the value into the filter bloom as follows:
; For each hash function, the function applies the hash on value,
; and sets the bit in the index returned by the hash to 1.
; the type added to the set and return some index 0 - n-1, where n = Bloom Filter size)

; receives: bloom - a Bloom Filter (a list of bits) that represents some set,
; value - some value that we wish to add to the set,
; hashes - an unknown number (1 or more) of hash functions

(define (bloom-add bloom value . hash)
  (define (helper bloom hash)
    (if (null? (cdr hash))
        (bloom-set-bit bloom (car hash))
        (bloom-set-bit (helper bloom (cdr hash)) (car hash))))
  (helper bloom (map (lambda(x) (x value)) hash)))

;E
; returns: #t - if the value is a member of the set (with high probability),
; or #f if it is definitely not in the set, as follows:
; For each hash function, the function applies the hash on value, and uses the result index
; to check the bit at that index in the filter.
; If all bits that were checked are 1, the function returns #t. If one of them is 0, it returns #f.

; receives: bloom - a Bloom Filter (a list of bits) that represents some set,
; value - some value that we wish to add to the set,
; hashes - an unknown number (1 or more) of hash functions
(define (bloom-query bloom value . hashes)
  (define (query-helper hash)
    (if (null? hash)
        #t
        (let ((findBit ((car hash) value)))
          (if (= (bloom-get-bit bloom findBit) 0)
              #f
              (query-helper (cdr hash))))))
  (query-helper hashes))

