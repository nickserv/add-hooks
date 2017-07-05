;;; add-hooks-test.el --- Tests for add-hooks

;;; Code:

(require 'ert)
(require 'cl)
(require 'add-hooks)

(ert-deftest add-hooks-listify ()
  (should (eq (add-hooks-listify nil) nil))
  (should (equal (add-hooks-listify '(a)) '(a)))
  (should (equal (add-hooks-listify 'a) '(a))))

(ert-deftest add-hooks-normalize-hook ()
  (should (equal (add-hooks-normalize-hook (lambda () nil)) (lambda () nil)))
  (should (eq (add-hooks-normalize-hook 'a) 'a-hook))
  (should (eq (add-hooks-normalize-hook 'a-hook) 'a-hook)))

(macrolet ((fixture
            (&rest body)
            "Declare hook variables locally, then eval BODY forms."
            `(let ((a-hook) (b-hook))
               ,@body)))

  (ert-deftest add-hooks-pair ()
    ;; One to one
    (fixture (add-hooks-pair 'a 'a)
             (should (equal a-hook '(a))))
    ;; One to many
    (fixture (add-hooks-pair 'a '(a b))
             (should (equal a-hook '(b a))))
    ;; Many to one
    (fixture (add-hooks-pair '(a b) 'a)
             (should (equal a-hook '(a)))
             (should (equal b-hook '(a))))
    ;; Many to many
    (fixture (add-hooks-pair '(a b) '(a b))
             (should (equal a-hook '(b a)))
             (should (equal b-hook '(b a))))
    ;; Verbose
    (fixture (add-hooks-pair 'a-hook 'a)
             (should (equal a-hook '(a)))))

  (ert-deftest add-hooks ()
    ;; One to one
    (fixture (add-hooks '((a . a)))
             (should (equal a-hook '(a))))
    ;; Multiple
    (fixture (add-hooks '((a . a) (b . b)))
             (should (equal a-hook '(a)))
             (should (equal b-hook '(b))))
    ;; One to many
    (fixture (add-hooks '((a . (a b))))
             (should (equal a-hook '(b a))))
    ;; Many to one
    (fixture (add-hooks '(((a b) . a)))
             (should (equal a-hook '(a)))
             (should (equal b-hook '(a))))
    ;; Many to many
    (fixture (add-hooks '(((a b) . (a b))))
             (should (equal a-hook '(b a)))
             (should (equal b-hook '(b a))))
    ;; Verbose
    (fixture (add-hooks '((a-hook . a)))
             (should (equal a-hook '(a))))))

(provide 'add-hooks-test)
;;; add-hooks-test.el ends here
