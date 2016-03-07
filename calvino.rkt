#lang web-server/insta

(require racket/match)
(require racket/sandbox)
(require db)

; Utility stuff
(define (random-element l)
  (list-ref l (random (length l)))
  )

; Evaluator stuff

(define base-top-eval
  (make-evaluator 'racket/base)
)

; Database stuff

(define pgc
  (postgresql-connect #:user "jaap1" #:database "calvino")
  )

(define save-exercise-query (prepare pgc "INSERT INTO exercises (description, inputs, code) \
  VALUES ($1, $2, $3)"))
(define get-exercise-ids-query (prepare pgc "SELECT id FROM exercises"))
(define get-exercise-query (prepare pgc "SELECT description, inputs, code FROM exercises WHERE id=$1"))

; Web stuff

(define (start request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Calvino"))
       (body
        (h1 "Main menu")
        (ul
         (li (a [(href ,(embed/url main-page))] "Try an exercise"))
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
    (let*
        ([ex-ids (query-rows pgc get-exercise-ids-query)]
         [ex-id (vector-ref (random-element ex-ids) 0)]
         [ex (query-row pgc get-exercise-query ex-id)]
         [descr (vector-ref ex 0)]
         )
      (response/xexpr
       `(html
         (head (title "Calvino"))
         (body
          (h1 "Exercise")
          (p ,descr)
          (form [(action ,(embed/url code-page))]
                (table
                 (tr (td (textarea [(rows "10") (cols "80") (name "code")] "")))
                 (tr (td (input [(type "hidden") (name "id") (value ,(number->string ex-id))])))
                 (tr (td (input [(type "submit") (value "Evaluate!")])))
                 )
                )
          )
         )
       )
      )
    )
  (send/suspend/dispatch response-generator)
  )

(define (handle-error e) (cons 'error (exn-message e)))

(define (try-code inputs ref-code sub-code)
  (with-handlers ([exn? handle-error]) (cons 'ok (base-top-eval sub-code)))
  )

(define (code-page request)
  (define (response-generator embed/url)
    (let*
        ([exercise-id (extract-binding/single 'id (request-bindings request))]
         [ex (query-row pgc get-exercise-query (string->number exercise-id))]
         [inputs (vector-ref ex 1)]
         [reference-code (vector-ref ex 2)]
         [submitted-code (extract-binding/single 'code (request-bindings request))]
         [res (try-code inputs reference-code submitted-code)]
         )
      (response/xexpr
       `(html
         (head (title "Calvino"))
         (body
          (h1 "Your code was:")
          (code ,submitted-code)
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
      
     