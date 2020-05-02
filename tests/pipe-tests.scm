(include "../src/oyster.scm")
(include "../src/oyster-core.scm")

(define (ls-wc-test)
  (string->number
   (car (oyster#->>
         (ls "./tests")
         (wc "-l")))))

(define (ls-wc-test-folder folder)
  (string->number
   (car (oyster#->>
         (ls folder)
         (wc "-l"))))
  )

(let ((result (ls-wc-test))
      (truth (length (directory-files "./tests"))))
  (import (_test))
  (check-= result truth))


(let ((result (ls-wc-test-folder "./tests"))
      (truth (length (directory-files "./tests"))))
  (import (_test))
  (check-= result truth))
