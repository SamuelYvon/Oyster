; Idea: the macro will replace function calls to incorrect functions (that would be Shell scripts)
; The function can have shucks-knifes, that define how to call the command and how to export
; the result
(define-library
  (oyster)
  (import (gambit)
          (oyster-core))
  (export
    ->>
    define-shell
    with-shell
    partial
    with-sudo
    )
  (begin

   (define-macro
     (define-shell signature body)
     (import oyster-core)
     `(define ,signature ,(oyster-core#dive body)))

   (define-macro
     (->> . functions)
     (import oyster-core)
     (oyster-core#pipe functions))

   (define-macro
     (with-shell . thunks)
     (import oyster-core)
     `(begin
        ,@(map oyster-core#dive thunks)))

   (define (partial f . args)
    (lambda more-args
     (f (append args more-args))))

   (define-macro (with-sudo . thunks)
     (import oyster-core)
     (let ((_ (gensym)))
       `(let ((,_ (shell-command "sudo ls -al >> /dev/null")))
          ,@thunks
        )))
))
