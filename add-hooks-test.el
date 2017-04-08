;;; add-hooks-test.el --- Tests for add-hooks

;;; Code:

(require 'ert)
(require 'add-hooks)

(ert-deftest add-hooks-listify ()
  (should (eq (add-hooks-listify nil) nil))
  (should (equal (add-hooks-listify '(a)) '(a)))
  (should (equal (add-hooks-listify 'a) '(a))))

(ert-deftest add-hooks ()
  (should (equal (macroexpand '(add-hooks (hook-a . a)))
                 '(add-hook 'hook-a 'a)))
  (should (equal (macroexpand '(add-hooks (hook-a . a)
                                          (hook-b . b)))
                 '(progn
                    (add-hook 'hook-a 'a)
                    (add-hook 'hook-b 'b))))
  (should (equal (macroexpand '(add-hooks (hook-a . (a b))))
                 '(progn
                    (add-hook 'hook-a 'a)
                    (add-hook 'hook-a 'b))))
  (should (equal (macroexpand '(add-hooks ((hook-a hook-b) . a)))
                 '(progn
                    (add-hook 'hook-a 'a)
                    (add-hook 'hook-b 'a))))
  (should (equal (macroexpand '(add-hooks ((hook-a hook-b) . (a b))))
                 '(progn
                    (add-hook 'hook-a 'a)
                    (add-hook 'hook-a 'b)
                    (add-hook 'hook-b 'a)
                    (add-hook 'hook-b 'b)))))

(provide 'add-hooks-test)
;;; add-hooks-test.el ends here
