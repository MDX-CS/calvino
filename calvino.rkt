#lang web-server/insta

(require racket/sandbox)

(define base-top-eval
  (make-evaluator '(begin) '(define (f) later)))

(define (start request)
  (cond
    [(exists-binding? 'code (request-bindings request)) (code-page request)]
    [else (main-page request)]
    )
  )

(define (main-page request)
  (response/xexpr
   `(html
     (head (title "Calvino"))
     (body
      (h1 "Your code:")
      (form
       (textarea ((rows "10") (cols "80") (name "code")) "Insert your code")
       (input ((type "submit")))
       )
      )
     )
   )
  )

(define (code-page request)
  (response/xexpr
   `(html
     (head (title "Calvino"))
     (body
      (h1 "This is YOUR code")
      (p
       ,(format "~v" (base-top-eval (extract-binding/single 'code (request-bindings request))))
       )
      )
     )
   )
  )