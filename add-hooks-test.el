;;; add-hooks-test.el --- Tests for add-hooks

;;; Code:

(require 'ert)
(require 'cl)
(require 'add-hooks)

(ert-deftest add-hooks-listify ()
  (should (eq (add-hooks-listify nil) nil))
  (should (equal (add-hooks-listify '(a)) '(a)))
  (should (equal (add-hooks-listify 'a) '(a))))

(macrolet ((fixture
            (&rest body)
            "Declare hook variables locally, then eval BODY forms."
            `(let ((hook-a) (hook-b))
               ,@body)))

  (ert-deftest add-hooks-pair-one-to-one ()
    (fixture (add-hooks-pair 'hook-a 'a)
             (should (equal hook-a '(a)))))

  (ert-deftest add-hooks-pair-one-to-many ()
    (fixture (add-hooks-pair 'hook-a '(a b))
             (should (equal hook-a '(b a)))))

  (ert-deftest add-hooks-pair-many-to-one ()
    (fixture (add-hooks-pair '(hook-a hook-b) 'a)
             (should (equal hook-a '(a)))
             (should (equal hook-b '(a)))))

  (ert-deftest add-hooks-pair-many-to-many ()
    (fixture (add-hooks-pair '(hook-a hook-b) '(a b))
             (should (equal hook-a '(b a)))
             (should (equal hook-b '(b a)))))

  (ert-deftest add-hooks-one-to-one ()
    (fixture (add-hooks '((hook-a . a)))
             (should (equal hook-a '(a)))))

  (ert-deftest add-hooks-multiple ()
    (fixture (add-hooks '((hook-a . a) (hook-b . b)))
             (should (equal hook-a '(a)))
             (should (equal hook-b '(b)))))

  (ert-deftest add-hooks-one-to-many ()
    (fixture (add-hooks '((hook-a . (a b))))
             (should (equal hook-a '(b a)))))

  (ert-deftest add-hooks-many-to-one ()
    (fixture (add-hooks '(((hook-a hook-b) . a)))
             (should (equal hook-a '(a)))
             (should (equal hook-b '(a)))))

  (ert-deftest add-hooks-many-to-many ()
    (fixture (add-hooks '(((hook-a hook-b) . (a b))))
             (should (equal hook-a '(b a)))
             (should (equal hook-b '(b a))))))

(provide 'add-hooks-test)
;;; add-hooks-test.el ends here
