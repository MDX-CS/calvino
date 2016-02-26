#lang web-server/insta

(require racket/match)
(require racket/sandbox)

(define base-top-eval
  (parameterize ([sandbox-path-permissions
                 '([execute "/bin/sh"] [write "/dev/tty.usbmodem1411"] )
                  ])
    (make-evaluator 'racket/base #:allow-for-require '("AsipMain.rkt"))
    )
  )

(define (start request)
  (cond
    [(exists-binding? 'code (request-bindings request)) (code-page request)]
    [else (main-page request)]
    )
  )

(define (main-page request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Calvino"))
       (body
        (h1 "Evaluator")
        (p "Please enter some Racket code:")
        (form ((action ,(embed/url code-page)))
              (table
               (tr (td (textarea ((rows "10") (cols "80") (name "code")) "")))
               (tr (td (input ((type "submit") (value "Evaluate!")))))
               )
              )
        )
       )
     )
    )
  (send/suspend/dispatch response-generator)
  )

(define handle-error (λ (e) (cons 'error (exn-message e))))

(define (code-page request)
  (define (response-generator embed/url)
    (let*
        ([code (extract-binding/single 'code (request-bindings request))]
         [res (with-handlers ([exn? handle-error]) (cons 'ok (base-top-eval code)))]
         )
      (response/xexpr
       `(html
         (head (title "Calvino"))
         (body
          (h1 "Your code was:")
          (code ,code)
          (h1 "The result")
          ,(match res
             [(cons 'ok s) `(p ,(format "~v" s))]
             [(cons 'error m) `(p ((style "background-color: #ff8080")) ,m)]
             [strange `(p ,(format "Okay, something really weird happened. (evaluation returned ~v)" strange))]
             )
          )
         (p (a ((href ,(embed/url main-page))) "To main page"))
         )
       )
      )
    )
  (send/suspend/dispatch response-generator)
  )
