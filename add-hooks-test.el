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
            `(let ((a-hook) (b-hook))
               ,@body)))

  (ert-deftest add-hooks-pair-one-to-one ()
    (fixture (add-hooks-pair 'a-hook 'a)
             (should (equal a-hook '(a)))))

  (ert-deftest add-hooks-pair-one-to-many ()
    (fixture (add-hooks-pair 'a-hook '(a b))
             (should (equal a-hook '(b a)))))

  (ert-deftest add-hooks-pair-many-to-one ()
    (fixture (add-hooks-pair '(a-hook b-hook) 'a)
             (should (equal a-hook '(a)))
             (should (equal b-hook '(a)))))

  (ert-deftest add-hooks-pair-many-to-many ()
    (fixture (add-hooks-pair '(a-hook b-hook) '(a b))
             (should (equal a-hook '(b a)))
             (should (equal b-hook '(b a)))))

  (ert-deftest add-hooks-one-to-one ()
    (fixture (add-hooks '((a-hook . a)))
             (should (equal a-hook '(a)))))

  (ert-deftest add-hooks-multiple ()
    (fixture (add-hooks '((a-hook . a) (b-hook . b)))
             (should (equal a-hook '(a)))
             (should (equal b-hook '(b)))))

  (ert-deftest add-hooks-one-to-many ()
    (fixture (add-hooks '((a-hook . (a b))))
             (should (equal a-hook '(b a)))))

  (ert-deftest add-hooks-many-to-one ()
    (fixture (add-hooks '(((a-hook b-hook) . a)))
             (should (equal a-hook '(a)))
             (should (equal b-hook '(a)))))

  (ert-deftest add-hooks-many-to-many ()
    (fixture (add-hooks '(((a-hook b-hook) . (a b))))
             (should (equal a-hook '(b a)))
             (should (equal b-hook '(b a))))))

(provide 'add-hooks-test)
;;; add-hooks-test.el ends here
