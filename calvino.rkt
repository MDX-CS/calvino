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
  (postgresql-connect #:user "jaapb" #:database "calvino")
  )

(define save-exercise-query (prepare pgc "INSERT INTO exercises (description, code) \
  VALUES ($1, $2) RETURNING id"))
(define save-inputs-query (prepare pgc "INSERT INTO exercise_inputs (exercise_id, input_line) \
  VALUES ($1, $2)"))
(define get-exercise-ids-query (prepare pgc "SELECT id FROM exercises"))
(define get-exercise-query (prepare pgc "SELECT description, code FROM exercises WHERE id=$1"))
(define get-exercise-inputs-query (prepare pgc "SELECT input_line FROM exercise_inputs WHERE exercise_id=$1"))

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

(define (try-code inputs code)
  (with-handlers ([exn? handle-error])
    (base-top-eval code)
    (cons 'ok (for/list [(i inputs)]
      (base-top-eval (format "(main ~a)" i))
                )
          )
    )
  )

(define (code-page request)
  (define (response-generator embed/url)
    (let*
        ([exercise-id (string->number (extract-binding/single 'id (request-bindings request)))]
         [ex (query-row pgc get-exercise-query exercise-id)]
         [reference-code (vector-ref ex 1)]
         [inputs (query-list pgc get-exercise-inputs-query exercise-id)]
         [submitted-code (extract-binding/single 'code (request-bindings request))]
         [res (try-code inputs submitted-code)]
         [ref-res (try-code inputs reference-code)]
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
          (h1 "The reference result")
          ,(match ref-res
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
  (let*
      ([description (extract-binding/single 'description (request-bindings request))]
       [inputs (string-split (extract-binding/single 'inputs (request-bindings request)) "\n")]
       [code (extract-binding/single 'code (request-bindings request))]
       [ex_id (query-value pgc save-exercise-query description code)])
    (for [(i inputs)] (query-exec pgc save-inputs-query ex_id i))
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
      
     