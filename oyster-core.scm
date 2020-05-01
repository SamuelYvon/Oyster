; Idea: the macro will replace function calls to incorrect functions (that would be Shell scripts)
; The function can have shucks-knifes, that define how to call the command and how to export
; the result
(define-library
  (oyster-core)
  (import (gambit))
  (export
    add-reef
    butter-knife
    cold-bath
    eat
    edible?
    prepare
    shuck
    define-shuck-knife ; define a handler for parsing commands
    alias
    )
  (begin
   ; ----------------------------------------------------------------------
   ; ----------------------------------------------------------------------
   ;                               Global data
   ; ----------------------------------------------------------------------
   ; ----------------------------------------------------------------------
   (define explored? #f)

   ; Table of handlers to function of the form
   ; symbol, string -> scheme data
   (define shucking-knifes (make-table))

   ; Table of handler functions to prepare and execute a command call
   ; A function is in the form of
   ; symbol, strings -> string
   (define seasonners (make-table))

   ; List of directories where functions can be found
   (define reefs '())

   ; List of functions that can be called
   (define oysters (make-table))

   ; Assoc list of aliases
   (define aliases '())

   ; So calls to cd work transparently
   (define cd current-directory)

   ; (define default-knife butter-knife)
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
         (lambda (e r)
           (if (string=? r "")
               e
               (string-append e sep r)))
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

   (define (macro-sym-to-rt-sym sym)
     (symbol->string sym))

   ; ----------------------------------------------------------------------
   ; ----------------------------------------------------------------------
   ;                                  Core
   ; ----------------------------------------------------------------------
   ; ----------------------------------------------------------------------

   ; Default shucking tool, simply transform the
   ; string into an array of lines
   (define (butter-knife command args str)
     (lines->list str))

   (define (make-call command args)
     (let ((all (cons
                  command
                  (map (lambda (e)
                         (string-append "\"" e "\""))
                       args))))
       (string-join all #\space)))

   (define (lime command . args)
     (let* ((alias-fn (assoc (string->symbol command) aliases))
            (substitution (if alias-fn
                              (cdr alias-fn)
                              alias-identity)))
       (substitution
         command
         args
         (lambda (command args)
           (cdr (shell-command (make-call command args) #t)))
         )))

   (define default-knife butter-knife)

   (define default-seasonner lime)

   (define (shuck command args str)
     ((table-ref shucking-knifes command default-knife) command args str))

   (define (prepare command . args)
     (apply (table-ref seasonners command default-seasonner) command args))

   (define (eat command . args)
     (shuck command args (apply prepare command args)))

   (define (cold-bath command)
     (lambda args (apply eat command args)))

   (define (edible? sym)
     (or (table-ref oysters sym #f)
         (assoc sym aliases)))

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

   (define (dive body)
     (if (not (pair? body))
         (if (edible? body)
             (list 'oyster-core#cold-bath (macro-sym-to-rt-sym body))
             body)
         (let ((head (car body))
               (rest (cdr body)))
           (cond ((eq? head '->>) ; don't touch the rest
                  body)
                 ((edible? head)
                  (cons 'oyster-core#eat (cons (macro-sym-to-rt-sym head) (map dive rest))))
                 (else
                   (cons head (map dive rest))))
           )))

   (define (do-pipe sym command)
     (cdr (shell-command command #t)))

   (table-set! seasonners 'OYSTER#PIPE do-pipe)

   (define (pipe-calls . functions)
     (let* ((calls (map (lambda (pair)
                          (let ((command (symbol->string (car pair)))
                                (args (cdr pair)))
                            (make-call command args)
                            )) functions))
            (last (caar (reverse functions)))
            (last-args (cdar (reverse functions)))
            (pipe-command (string-join calls #\|)))
       (oyster-core#shuck (macro-sym-to-rt-sym last) (quote last-args) (oyster-core#prepare 'OYSTER#PIPE pipe-command))
       ))

   (define (pipe functions)
     (let* ((calls (map (lambda (pair)
                          (cons 'list (cons `(string->symbol ,(macro-sym-to-rt-sym (car pair))) (cdr pair)))
                          ) functions)))
       `(oyster-core#pipe-calls ,@calls)))

   (define (alias-replace to args)
    (lambda (command other-args c)
     (c (symbol->string to) (append args other-args))))

   (define (alias-identity command other-args c)
     (c command other-args))

   ; Create an alias
   ; what: source symbol
   ; to : target symbol (shell call)
   ; args: list of args to include
   (define (alias what to . args)
     (set! aliases (cons (cons what (alias-replace to args)) aliases)))


   ; Load the config file at the end, so it can
   ; access all of the previously defined routines
   ; including the core functions
   (include "~/.oyster.scm")
   ))
