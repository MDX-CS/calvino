#lang web-server/insta

(require racket/match)
(require racket/sandbox)
(require net/ldap)
(require db)
(require "config.rkt")

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
  (postgresql-connect #:user database-user #:database database-name)
  )

(define save-exercise-query (prepare pgc "INSERT INTO exercises (description, code) \
  VALUES ($1, $2) RETURNING id"))
(define save-input-query (prepare pgc "INSERT INTO exercise_inputs (exercise_id, input_line) \
  VALUES ($1, $2)"))
(define get-exercise-ids-query (prepare pgc "SELECT id FROM exercises"))
(define get-exercise-query (prepare pgc "SELECT description, code FROM exercises WHERE id=$1"))
(define get-exercise-inputs-query (prepare pgc "SELECT input_line FROM exercise_inputs WHERE exercise_id=$1"))

; Code handling stuff

(define (handle-error e) (cons 'error (exn-message e)))

(define (try-code inputs code)
  (with-handlers ([exn? handle-error]) (base-top-eval code))
  (for/list [(i inputs)]
    (with-handlers ([exn? handle-error]) (base-top-eval (format "(main ~a)" i)))
    )
  )

(define (update-verdict old new)
  (match (unbox old)
    ['error (set-box! old 'error)]
    ['notok (cond [(equal? new 'error) (set-box! old 'error)] [else (set-box! old 'notok)])]
    ['ok (cond [(equal? new 'ok) (set-box! old 'ok)] [else (set-box! old new)])]
    ['unknown (set-box! old new)]
    )
  )

; Web stuff

(define (start request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Calvino"))
       (body
        (h1 "Login")
        (form ([action ,(embed/url check-credentials)] [method "post"])
              (table
               (tr
                (td "User ID")
                (td (input ([name "username"])))
                )
               (tr
                (td "Password")
                (td (input ([name "password"] [type "password"])))
                )
               (tr
                (td ([colspan "2"]) (input ([type "submit"] [value "Login"])))
                )
              )
              )
        )
       )
     )
    )
  (send/suspend/dispatch response-generator)
  )

(define (check-credentials request)
  (define (response-generator embed/url)
    (let*
        ([username (extract-binding/single 'username (request-bindings request))]
         [password (extract-binding/single 'password (request-bindings request))]
         [ldap-result (ldap-authenticate ldap-server 389 (string-append "Uni\\" username) password)])
      (response/xexpr
       `(html
         (head (title "Calvino"))
         (body
          (h1 "Credentials")
          (ul
           (li "LDAP says: " ,(format "~a" ldap-result))
           )
          )
         )
       )
      )
    )
  (send/suspend/dispatch response-generator)
  )

(define (main-menu request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Calvino"))
       (body
        (h1 "Main menu")
        (ul
         (li (a [(href ,(embed/url do-exercise))] "Try an exercise"))
         (li (a [(href ,(embed/url list-exercises))] "List the available exercises"))
         (li (a [(href ,(embed/url add-exercise))] "Add an exercise"))
         )
        )
       )
     )
    )
  (send/suspend/dispatch response-generator)
  )

(define (do-exercise request)
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
                 (tr (td (textarea ([rows "10"] [cols "80"] [name "code"]) "")))
                 (tr (td (input ([type "hidden"] [name "id"] [value ,(number->string ex-id)]))))
                 (tr (td (input ([type "submit"] [value "Evaluate!"]))))
                 )
                )
          )
         )
       )
      )
    )
  (send/suspend/dispatch response-generator)
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
         [verdict (box 'unknown)]
         )
      (response/xexpr
       `(html
         (head (title "Calvino"))
         (body
          (h1 "Your code was:")
          (code ,submitted-code)
          (h2 "Result")
          (table
           (tr (th "Input") (th "Your result") (th "Expected result"))
          ,@(for/list ([i inputs] [r res] [rr ref-res])
              `(tr
                (td ,i)
                ,(match r
                   [(cons 'error e) (update-verdict verdict 'error) `(td [(style "background-color: red;")] ,e)]
                   [else (update-verdict verdict (cond [(equal? r rr) 'ok] [else 'notok])) `(td ,(format "~v" r))]
                   )
                (td ,(format "~v" rr))
               )
              )
          )
          (h2 "Verdict")
          ,(match (unbox verdict)
             ['unknown `(p (b "UNKNOWN: ") "No inputs were run on the program, this probably means the exercise wasn't set up correctly.")]
             ['error `(p (b "ERROR: ") "Your program produced errors on execution with some or all of the inputs.")]
             ['notok `(p (b "NOT OK: ") "Your program executed without errors, but one or more of the results were incorrect.")]
             ['ok `(p (b "OK: ") "Your program executed without errors and produced the correct results for all test cases.")]
             )
         (p (a [(href ,(embed/url start))] "To main page"))
         )
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
       [results (try-code inputs code)])
    (match results
      [(cons 'error e)
       (response/xexpr
        `(html
          (head (title "Calvino"))
          (body
           (h1 "Error")
           (p "Something went wrong, an error occurred in the definitions.")
           (p e)
           (p (a [(href ,(embed/url start))] "To main page"))
           )
          )
        )
       ]
      [r
       (let
           ([ex-id (query-value pgc save-exercise-query description code)])
            (response/xexpr
             `(html
               (head (title "Calvino"))
               (body
                (h1 "Results")
                (table
                 (tr (th "Inputs") (th "Result"))
                 ,@(for/list ([i inputs] [r results])
                     (match r
                       [(cons 'error e) `(tr (td ,i) (td ,e))]
                       [else
                        (query-exec pgc save-input-query ex-id (string-trim i))
                        `(tr (td ,i) (td ,(format "~v" r)))
                        ]
                       )
                     )
                 )
                (p (a [(href ,(embed/url start))] "To main page"))
                )
               )
             )
         )
       ]
      )
    )
    )
  (send/suspend/dispatch response-generator)
  )

(define (list-exercises request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (head (title "Calvino"))
       (body
        (h1 "Main menu")
        (ul
         (li (a [(href ,(embed/url do-exercise))] "Try an exercise"))
         (li (a [(href ,(embed/url list-exercises))] "List the available exercises"))
         (li (a [(href ,(embed/url add-exercise))] "Add an exercise"))
         )
        )
       )
     )
    )
  (send/suspend/dispatch response-generator)
  )

     