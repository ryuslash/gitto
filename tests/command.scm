(define-module (test command)
  #:use-module (srfi srfi-64)
  #:use-module (gitto command))

(define exit-status 0)

(define (gitto-test-runner)
  (let ((runner (test-runner-simple)))
    (test-runner-on-test-end! runner
       (lambda (runner)
         (test-on-test-end-simple runner)
         (case (test-result-kind runner)
           ((fail xfail) (set! exit-status 1)))))
    runner))

(test-runner-factory gitto-test-runner)

(test-begin "Command tests")

(test-equal "command? returns #f for unknown commands"
            #f (command? "this-really-isn't-a-command"))

(define-command (test-command)
  "Test command"
  "Test command"
  #f)

(test-equal "command? returns #t for known commands"
            #t (command? "test-command"))

(test-end "Command tests")

(exit exit-status)
