 yatest - yet another testing framework.
=========================================

First step. Define the test like a following form.

    (eval-after-load "yatest"
      '(yatest::define-test my-project my-test-name
         (yatest "simple case of using `my-hoo'."
           (eq 123 (yatest::p "the return value of my-hoo." (my-hoo 321))))))

Next step. Load yatest.el .

    M-: (require 'yatest)

Last step. Run test.

     M-x yatest::run
    -------------------------
     project? :  my-project
    -------------------------
     test? : my-test-name

yatest::run popups the buffer that contains result of the test with fancy faces.


yatest.el provides following forms and commands.

  - yatest::define-test - Defines your test.
  - yatest::p           - Output the value in your test.
  - yatest              - Define a case of the test.
  - yatest::run         - Runs a individual test or all tests of specified project.


