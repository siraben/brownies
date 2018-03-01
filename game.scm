(define *num-brownies* 130000)

(define (2dec n)
  (/ (truncate (* n 100)) 100))

(define (redraw)
  (set-content! "#dashboard"
                (string-append "<h3>" "Brownies: " (number->string (2dec *num-brownies*))
                               "\nBrewers: " (number->string (brewers 'get-amount))
                               "\nGrandmas: " (number->string (grandmas 'get-amount))
                               "\nMines: " (number->string (mines 'get-amount))
                               "\nFactories "(number->string (factories 'get-amount)) "</h3>")))

(define (message! string)
  (set-content! "#message" string))

(define (click)
  (set! *num-brownies* (+ *num-brownies* 1))
  (redraw))

(define (name-button! button new-name)
  (js-invoke button "setAttribute" "value" new-name))

(define (bind-proc-button button proc)
  (add-handler! button "click" proc))

(define (create-button . args)
  (let ((button (js-eval "document.createElement('input')")))
    (js-invoke button "setAttribute" "type" "button")
    (name-button! button (car args))
    (if (= (length args) 2) (add-handler! button "click" (cadr args)))
    (element-append-child! ($ "#buttons") button)
    button))

;; Perhaps a bug in BiwaScheme? I can't seem to dynamically update button names.

(define (make-new-purchase name price cps interest)
  (let ((amount 0)
        (cost price)
        (button
         (create-button (string-append "Buy " name " (" (number->string price) ")"))))

    (js-invoke button "setAttribute" "title" (string-append "Gets " (number->string cps)" clicks per second."))

    (define (buy)
      (if (> cost *num-brownies*)
          (message! (string-append "Insufficient funds! You need " (number->string (2dec cost)) " brownies."))

          (begin 
            (set! *num-brownies* (- *num-brownies* cost))
            (set-timer! click (/ 1 cps))
            (set! amount (+ amount 1))
            (set! cost (* price (expt interest amount)))
            (message! (string-append "Price of " name " now at " (number->string (2dec cost)) " brownies."))
            (refresh))))

    (bind-proc-button button buy)

    (define (refresh)
      (begin (set! cost (* price (expt interest amount)))
             (name-button! button (string-append "Buy " name " (" (number->string (2dec cost)) ")" ))))

    (define (dispatch m)
      (cond ((eq? m 'refresh) refresh)
            ((eq? m 'buy) buy)
            ((eq? m 'get-amount) amount)))
    dispatch))


(define manual-click (create-button "New brownie!" click))
(js-invoke manual-click "setAttribute" "title" "What are you waiting for? Click me!")

(define brewers (make-new-purchase "brewer" 15 0.1 1.15))

(define grandmas (make-new-purchase "grandma" 100 0.8 1.15))

(define mines (make-new-purchase "mine" 1000 5 1.15))

(define factories (make-new-purchase "factory" 130000 260 1.15))

(set-timer! redraw 0.1)
