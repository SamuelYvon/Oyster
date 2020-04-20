; Idea: the macro will replace function calls to incorrect functions (that would be Shell scripts)
; The function can have shucks-knifes, that define how to call the command and how to export
; the result
(define-library
  (oyster)
  (import (gambit)
          (oyster-core))
  (export
    ->>
    with-shell
    define-shell
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

   ; Load the config file at the end, so it can
   ; access all of the previously defined routines
   ; including the core functions
   (include "~/.oyster.scm")
  ))
