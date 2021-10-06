#lang racket/base
;;; Based on gui-widget-mixins
;; Copyright (c) 2019, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
;; Copyright (c) 2021 Roman Klochkov <kalimehtar@mail.ru>

(require racket/date racket/gui/base racket/class racket/string)

(define cue-text-style
  (let ((grey-text (new style-delta%)))
    (send grey-text set-delta-foreground "gray")
    grey-text))

(define normal-text-style
  (let ((black-text (new style-delta%)))
    (send black-text set-delta-foreground "black")
    black-text))

(define (text-empty? a-text)
  (define snip (send a-text find-first-snip))
  (or (not snip) (= 0 (send snip get-count))))

(define (with-0 n)
  (define s (number->string n))
  (if (< n 10) (string-append "0" s) s))

(define (format-date d)
  (format "~a.~a.~a"
          (with-0 (date-day d))
          (with-0 (date-month d))
          (date-year d)))

(define date-text-field%
  (class text-field%
    (inherit get-editor)
    (init-field [callback #f])

    (define (update-cue!)
      (define today (current-date))
      (set! cue (format-date today))
      (define begin-of-day (struct-copy date today
                                        [second 1]
                                        [minute 0]
                                        [hour 0]))
      (define secs-to-next-day (- (+ (date->seconds begin-of-day) 86400)
                                  (current-seconds)))
      (new timer%
           [notify-callback (lambda () (update-cue!))]
           [interval (* 1000 secs-to-next-day)]
           [just-once? #t]))
    
    (define (insert-cue-or-filter-chars!)
      (define editor (get-editor))
      (cond
        [(and (not showing-cue?) (text-empty? editor))
         (send* editor
           (change-style cue-text-style 'start 'end #f)
           (insert cue)
           (move-position 'home))
         (set! showing-cue? #t)]
        [else
         (define start (box 0))
         (define end (box 0))
         (send editor get-visible-position-range start end)
         (let loop ([pos (sub1 (unbox end))])
           (when (>= pos (unbox start))
             (define ch (send editor get-character pos))
             (unless (or (char=? ch #\.) (char<=? #\0 ch #\9))
               (send editor delete (add1 pos)))
             (loop (sub1 pos))))]))

    (define (parse-date!)
      (define parts (string-split (get-value) "."))
      (define-values (dd mm yyyy)
        (let loop ([dd ""] [mm ""] [yyyy ""] [parts parts] [n 0])
          (if (or (null? parts) (> n 2))
              (values dd mm yyyy)
              (let ([part (car parts)] [rest (cdr parts)])
                (case n
                  [(0) (loop part mm yyyy rest 1)]
                  [(1) (loop dd part yyyy rest 2)]
                  [(2) (loop dd mm part rest 3)])))))
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
      (set-value
       (format-date
        (seconds->date
         (get-seconds dd mm yyyy)))))

    (define (clear-cue)
      (when showing-cue?
        (send* (get-editor)
          (erase)
          (change-style normal-text-style 'start 'end #f))
        (set! showing-cue? #f)))

    (define/override (on-subwindow-char receiver event)
      (clear-cue)
      (begin0
        (super on-subwindow-char receiver event)
        (queue-callback (lambda () (insert-cue-or-filter-chars!)))))

    (define/override (on-subwindow-event receiver event)
      (begin0
        (super on-subwindow-event receiver event)
        (when showing-cue?
          (queue-callback (lambda () (send (get-editor) move-position 'home))))))

    (define/override (set-value v)
      (clear-cue)
      (super set-value v)
      (insert-cue-or-filter-chars!))

    (define/override (get-value)
      (if showing-cue? "" (super get-value)))
    
    (super-new [callback (lambda (c e)
                           (unless showing-cue?
                             (when (eq? (send e get-event-type) 'text-field-enter)
                               (parse-date!))
                             (when callback (callback c e))))])
    (define showing-cue? #f)
    (define cue "01.01.2021")
    (update-cue!)
    (insert-cue-or-filter-chars!)))

(module+ main
  (define f (new frame% [label "test"]))
  
  (define t (new date-text-field% [parent f] [label "test"]))

  (send f show #t))