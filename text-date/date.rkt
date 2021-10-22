#lang racket/base
(provide string->date today-data)
(require racket/date racket/string)

(define (string->date str)
  (define-values (dd mm yyyy) (split-date (string-split str ".")))
  (format-date (seconds->date (get-seconds dd mm yyyy))))

(define (today-data)
  (define today (current-date))
  (values
   (- (+ (date->seconds  (struct-copy date today
                                [second 1]
                                [minute 0]
                                [hour 0]))
         86400)
      (current-seconds))
   (format-date today)))

(define (format-date d)
  (format "~a.~a.~a"
          (with-0 (date-day d))
          (with-0 (date-month d))
          (date-year d)))

(define (with-0 n)
  (define s (number->string n))
  (if (< n 10) (string-append "0" s) s))

(define (split-date parts [dd ""] [mm ""] [yyyy ""] [n 0])
  (if (or (null? parts) (> n 2))
      (values dd mm yyyy)
      (let ([part (car parts)] [rest (cdr parts)])
        (case n
          [(0) (split-date rest part mm yyyy  1)]
          [(1) (split-date rest dd part yyyy  2)]
          [(2) (split-date rest dd mm part  3)]))))

(define (get-seconds dd mm yyyy)
  (cond
    [(and (<= 1 (string-length dd) 2)
          (<= 1 (string-length mm) 2)
          (> (string-length yyyy) 2))
     (with-handlers ([exn:fail? (λ _ (current-seconds))])
       (find-seconds 0 0 0 (string->number dd) (string->number mm) (string->number yyyy)))]
    [(and (<= 3 (string-length dd))
          (string=? mm ""))
     (define l (string-length dd))
     (get-seconds (substring dd 0 2) (substring dd (min l 2) (min l 4)) (substring dd (min l 5) l))]
    [else
     (define today (current-date))
     (define (extract field accessor)
       (if (string=? "" field) (accessor today) (string->number field)))
     (define (pad x)
       (cond
         [(>= x 100) x]
         [else
          (define year-today (date-year today))
          (define in-century (+ x (* 100 (quotient year-today 100))))
          (cond
            [(> in-century (+ year-today 50) (- in-century 100))]
            [(< in-century (- year-today 50) (+ in-century 100))]
            [else in-century])]))
     (define dd* (extract dd date-day))
     (define mm* (extract mm date-month))
     (define yy* ((if (<= 1 (string-length yyyy) 2) pad (λ (x) x)) (extract yyyy date-year)))
     (with-handlers ([exn:fail? (λ _ (current-seconds))])
       (find-seconds 0 0 0 dd* mm* yy*))]))