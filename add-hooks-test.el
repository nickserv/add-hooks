;;; add-hooks-test.el --- Tests for add-hooks

;;; Code:

(require 'ert)
(require 'add-hooks)

(ert-deftest add-hooks-listify ()
  (should (eq (add-hooks-listify nil) nil))
  (should (equal (add-hooks-listify '(1)) '(1)))
  (should (equal (add-hooks-listify 1) '(1))))

(ert-deftest add-hooks-mapflat ()
  (should (equal (add-hooks-mapflat #'reverse
                                    '((1 2) (3 4)))
                 '(2 1 4 3))))

(ert-deftest add-hooks ()
  (should (equal (macroexpand '(add-hooks (1 . 2)))
                 '(add-hook '1 '2)))
  (should (equal (macroexpand '(add-hooks (1 . 2)
                                          (3 . 4)))
                 '(progn
                    (add-hook '1 '2)
                    (add-hook '3 '4))))
  (should (equal (macroexpand '(add-hooks (1 . (2 3))))
                 '(progn
                    (add-hook '1 '2)
                    (add-hook '1 '3))))
  (should (equal (macroexpand '(add-hooks ((1 2) . 3)))
                 '(progn
                    (add-hook '1 '3)
                    (add-hook '2 '3))))
  (should (equal (macroexpand '(add-hooks ((1 2) . (3 4))))
                 '(progn
                    (add-hook '1 '3)
                    (add-hook '1 '4)
                    (add-hook '2 '3)
                    (add-hook '2 '4)))))

(provide 'add-hooks-test)
;;; add-hooks-test.el ends here
