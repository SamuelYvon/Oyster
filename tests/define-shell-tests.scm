(include "../src/oyster.scm")
(include "../src/oyster-core.scm")

(define TEST-STRING "this is a test string")

(oyster#define-shell
 (test-echo)
 (let ((s TEST-STRING))
   (echo s)))

(oyster#define-shell
 (test-ls-length)
 (length (ls)))

(let ((first-test (car (test-echo))))
  (import (_test))
  (check-true (string=? first-test TEST-STRING)))

(let ((length-test (test-ls-length)))
  (import (_test))
  (check-true (> length-test 0)))

