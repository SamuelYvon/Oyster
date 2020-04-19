; Idea: the macro will replace function calls to incorrect functions (that would be Shell scripts)
; The function can have shucks-knifes, that define how to call the command and how to export
; the result
(define-library
  (oyster)
  (import (gambit))
  (export
    define-shuck-knife ; define a handler for parsing commands
    butter-knife
    with-knife
    with-knifes
    shuck
    define-shell
    )
  (begin

   (define foldl fold)
   (define foldr fold-right)

   (define (zip left right)
    (if (pair? left)
        (cons (cons (car left) (car right))
              (zip (cdr left) (cdr right)))
        '()))

   ; Join a list of 'strs' of string with the 'sep' character
   (define (string-join strs sep)
     (let ((sep (if (char? sep)
                    (string sep)
                    sep)))
       (foldr
         (lambda (e r) (string-append e sep r))
         ""
         strs
         )))

   (define (string-split-pred str pred)
    (filter
       (lambda (e) (not (string=? e "")))
       (let ((str-l (string->list str)))
         (foldr
           (lambda (e r)
             (if (pred e)
                 (cons "" r)
                 (cons (string-append (string e) (car r)) (cdr r))))
           '("")
           str-l
           ))))

   (define (string-split str sep)
    (string-split-pred str (lambda (c) (eq? c sep))))

   (define (lines->list str)
     (string-split str #\newline))

   (define (filter p l)
    (foldr (lambda (e r) (if (p e) (cons e r) r)) '() l))

   ; Table of handlers to function of the form
   ; symbol, (args) -> type
   (define shucking-knifes (make-table))

   ; Default shucking tool, args as string and command as symbol
   (define (butter-knife command . args)
    (let ((all (cons (symbol->string command) args)))
     (cdr (shell-command (string-join all #\space) #t))))

   (define (shuck command . args)
    (apply (table-ref shucking-knifes command butter-knife) command args))

   (define (cold-bath command)
    (lambda args
      (apply shuck (cons command args))))

   (define-macro
     (define-shell signature body)
     (letrec ((dive
                (lambda (body)
                  (if (not (pair? body))
                      body
                      (if (eq? 'ls (car body))
                          (cons 'shuck (cons `(string->symbol "ls") (map dive (cdr body))))
                          (cons (car body) (map (dive (cdr body))))))))
              )
       `(define
          ,signature
          ,(dive body)
          )))

   (define (define-shuck-knife command knife)
    (table-set! shucks-knifes command knife))

   (define (undef-shuck-knife command)
     (table-set! shucks-knifes command))

   ; Replace a knife for a lexical block
   ; (define-macro (with-knife command knife thunk)
   ;  `(let ((shucking-knifes (table-set! ,command ,knife)))
   ;    ,thunk))

   ; ; Replace a list of knife for a lexical block
   ; (define-macro (with-knifes commands knifes thunk)
   ;  `(let* (,@(map (lambda (p)
   ;                   (let ((name (car p))
   ;                         (knife (cdr p)))
   ;                     `(shucking-knifes (table-set! shucking-knifes ,name ,knife))))
   ;                 (zip commands knifes)))
   ;        ,thunk
   ;        ))

   ; (define (load-path)

   ;  )

   ; (define (load-knifes)

   ; )
  ))
