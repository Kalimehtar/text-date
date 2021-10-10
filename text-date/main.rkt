#lang racket/base
;;; Based on gui-widget-mixins
;; Copyright (c) 2019, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
;; Copyright (c) 2021 Roman Klochkov <kalimehtar@mail.ru>

(provide date-text-field%)
(require racket/gui/base racket/class "date.rkt")

(define date-text-field%
  (class text-field%
    (inherit get-editor)
    (init-field [callback #f])

    (define (update-cue!)
      (define-values (secs-to-next-day today) (today-data))
      (set! cue today)
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
         (filter-chars editor (λ (ch) (or (char=? ch #\.) (char<=? #\0 ch #\9))))]))

    (define (process-complete-date!)
      (set-value (string->date (get-value))))

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
                               (process-complete-date!))
                             (when callback (callback c e))))])
    (define showing-cue? #f)
    (define cue "01.01.2021")
    (update-cue!)
    (insert-cue-or-filter-chars!)))

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

(define (filter-chars editor filter)
  (define start (box 0))
  (define end (box 0))
  (send editor get-visible-position-range start end)
  (let loop ([pos (sub1 (unbox end))])
    (when (>= pos (unbox start))
      (define ch (send editor get-character pos))
      (unless (filter ch)
        (send editor delete (add1 pos)))
      (loop (sub1 pos)))))

(module+ main
  (define f (new frame% [label "test"]))  
  (new date-text-field% [parent f] [label "test"])
  (send f show #t))