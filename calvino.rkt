#lang web-server/insta

(require racket/match)
(require racket/sandbox)
(require db)

(define base-top-eval
  (make-evaluator 'racket/base)
)

(define pgc
  (postgresql-connect #:user "jaap1" #:database "calvino")
  )

(define save-exercise-query (prepare pgc "INSERT INTO exercises (description, inputs, code) \
  VALUES ($1, $2, $3)"))

(define (start request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Calvino"))
       (body
        (h1 "Main menu")
        (ul
         (li (a [(href ,(embed/url main-page))] "Try some code"))
         (li (a [(href ,(embed/url add-exercise))] "Add an exercise"))
         )
        )
       )
     )
    )
  (send/suspend/dispatch response-generator)
  )

(define (main-page request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Calvino"))
       (body
        (h1 "Evaluator")
        (p "Please enter some Racket code:")
        (form [(action ,(embed/url code-page))]
              (table
               (tr (td (textarea [(rows "10") (cols "80") (name "code")] "")))
               (tr (td (input [(type "submit") (value "Evaluate!")])))
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
         (p (a [(href ,(embed/url start))] "To main page"))
         )
       )
      )
    )
  (send/suspend/dispatch response-generator)
  )

(define (add-exercise request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Calvino"))
       (body
        (h1 "Add an exercise")
        (form [(action ,(embed/url save-exercise))]
              (table
               (tr (td "The exercise description:"))
               (tr (td (textarea [(rows "5") (cols "80") (name "description")])))
               (tr (td "Sample inputs (separate arguments by a space, options by a newline):"))
               (tr (td (textarea [(rows "10") (cols "80") (name "inputs")])))
               (tr (td "Solution code:"))
               (tr (td (textarea [(rows "20") (cols "80") (name "code")])))
               (tr (td (input [(type "submit") (value "Save!")])))
               )
              )
        )
       )
     )
    )
  (send/suspend/dispatch response-generator)
  )

(define (save-exercise request)
  (define (response-generator embed/url)
  (let
      ([description (extract-binding/single 'description (request-bindings request))]
       [inputs (extract-binding/single 'inputs (request-bindings request))]
       [code (extract-binding/single 'code (request-bindings request))])
    (query-exec pgc save-exercise-query description inputs code)
    (response/xexpr
     `(html
       (head (title "Calvino"))
       (body
        (h1 "Done")
        (p "Exercise saved.")
        (p (a [(href ,(embed/url start))] "Back to main menu"))
        )
       )
     )
    )
    )
  (send/suspend/dispatch response-generator)
  )
      
     