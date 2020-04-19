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
    cold-bath
    add-reef
    edible?
    )
  (begin
   ; ----------------------------------------------------------------------
   ; ----------------------------------------------------------------------
   ;                               Global data
   ; ----------------------------------------------------------------------
   ; ----------------------------------------------------------------------
   (define explored? #f)

   ; Table of handlers to function of the form
   ; symbol, (args) -> type
   (define shucking-knifes (make-table))

   ; List of directories where functions can be found
   (define reefs '())

   ; List of functions that can be called
   (define oysters (make-table))

   (define default-knife butter-knife)

   ; ----------------------------------------------------------------------
   ; ----------------------------------------------------------------------
   ;                                  Utils
   ; ----------------------------------------------------------------------
   ; ----------------------------------------------------------------------
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

   ; ----------------------------------------------------------------------
   ; ----------------------------------------------------------------------
   ;                                  Core
   ; ----------------------------------------------------------------------
   ; ----------------------------------------------------------------------

   ; Default shucking tool, args as string and command as symbol
   (define (butter-knife command . args)
    (let ((all (cons (symbol->string command) args)))
     (lines->list
       (cdr (shell-command (string-join all #\space) #t)))))

   (define (shuck command . args)
    (apply (table-ref shucking-knifes command default-knife) command args))

   (define (cold-bath command)
    (lambda args (apply shuck (cons command args))))

   (define (edible? sym)
    (table-ref oysters sym #f))

   (define (define-shuck-knife command knife)
    (table-set! shucks-knifes command knife))

   (define (undef-shuck-knife command)
     (table-set! shucks-knifes command))

   ; Add a path as a reef, a source of programs that can
   ; and should be translated to calls to shell
   (define (add-reef path)
    (set! reefs (cons path reefs))
    (explore path))

   ; Add all programs to the list
   (define (explore path)
    (for-each
     (lambda (prog)
      (table-set! oysters (string->symbol prog) #t))
     (directory-files path)))

   ; ----------------------------------------------------------------------
   ; ----------------------------------------------------------------------
   ;                            Exported Macros
   ; ----------------------------------------------------------------------
   ; ----------------------------------------------------------------------

   (define-macro
    (define-shell signature body)
    ; Find a way to get the edible? call working...
    ; Macros are not always fun
    (letrec ((dive
               (lambda (body)
                 (if (not (pair? body))
                     (if (edible? body)
                         (list 'cold-bath `(string->symbol ,(symbol->string body)))
                         body)
                     (let ((head (car body))
                           (rest (cdr body)))
                       (if (edible? head)
                           (cons 'shuck (cons `(string->symbol ,(symbol->string head)) (map dive rest)))
                           (cons head (map dive rest))))
                     ))))
      `(define
         ,signature
         ,(dive body)
         )))

   ; Replace a knife for a lexical block
   ; (define-macro (with-knife command knife thunk)
   ;  (let ((r (gensym)))
   ;    `(let ((shucking-knifes (table-set! ,command ,knife)))
   ;       ,thunk)
   ;    )
   ;  )


   ; ; Replace a list of knife for a lexical block
   ; (define-macro (with-knifes commands knifes thunk)
   ;  `(let* (,@(map (lambda (p)
   ;                   (let ((name (car p))
   ;                         (knife (cdr p)))
   ;                     `(shucking-knifes (table-set! shucking-knifes ,name ,knife))))
   ;                 (zip commands knifes)))
   ;        ,thunk
   ;        ))

   ; Load the config file at the end, so it can
   ; access all of the previously defined routines
   (load "~/.oyster")
  ))
