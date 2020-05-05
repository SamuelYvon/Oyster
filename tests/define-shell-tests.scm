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

(oyster#define-shell
 (test-sym-arg)
 (length (ls '..)))

(oyster#define-shell
 (ls-al-test)
 (ls "-al"))

(oyster#define-shell
 (test-flags)
 (ls (oyster#flags a l)))

(oyster#define-shell
 (test-litteral-flags)
 (ls '-al))

(let ((first-test (car (test-echo))))
  (import (_test))
  (check-true (string=? first-test TEST-STRING)))

(let ((length-test (test-ls-length)))
  (import (_test))
  (check-true (> length-test 0)))

(let ((sym-test (test-sym-arg)))
  (import (_test))
  (check-true (> sym-test 0)))

(let ((ls-with-flags (test-flags))
      (ls-without-flags (ls-al-test)))
  (import (_test))
  (check-true (equal? ls-with-flags ls-without-flags))
  )

(let ((ls-with-flags (test-litteral-flags))
      (ls-without-flags (ls-al-test)))
  (import (_test))
  (check-true (equal? ls-with-flags ls-without-flags)))
