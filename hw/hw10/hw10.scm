(define (accumulate combiner start n term)
  (if (= n 0) start
      (combiner (term n) (accumulate combiner start (- n 1) term)))
)

(define (accumulate-tail combiner start n term)
  (define (accumulate-iter n tail)
    (if (= n 0) tail
    (accumulate-iter (- n 1) (combiner (term n) tail))))
  (accumulate-iter n start)
)

(define (partial-sums stream)
  (define (helper sum stream)
    (if (null? stream) nil
        (cons-stream (+ sum (car stream)) (helper (+ sum (car stream)) (cdr-stream stream)))
    )
  )
  (helper 0 stream)
)

(define (rle s)
  (define (helper prev n stream)
    (cond ((null? stream) (cons-stream (list prev n) nil))
          ((= prev (car stream)) (helper prev (+ n 1) (cdr-stream stream)))
          (else (cons-stream (list prev n) (helper (car stream) 0 stream)))
    )
  )
  (if (null? s) nil
    (helper (car s) 0 s)
  )
)