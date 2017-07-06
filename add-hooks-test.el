;;; add-hooks-test.el --- Tests for add-hooks

;;; Code:

(require 'ert)
(require 'cl)
(require 'add-hooks)

;; Example lambda
(setq function (lambda () (message "Hello, world!")))

(defmacro fixture (&rest body)
  "Declare hook variables locally, then eval BODY forms."
  `(let ((a-hook) (b-hook))
     ,@body))

(ert-deftest add-hooks-listify ()
  ;; Nil
  (should (eq (add-hooks-listify nil) nil))
  ;; List
  (should (equal (add-hooks-listify '(a)) '(a)))
  ;; Atom
  (should (equal (add-hooks-listify 'a) '(a)))
  ;; Lambda
  (should (equal (add-hooks-listify function) (list function))))

(ert-deftest add-hooks-normalize-hook ()
  ;; Symbol
  (should (eq (add-hooks-normalize-hook 'a) 'a-hook))
  ;; Verbose symbol
  (should (eq (add-hooks-normalize-hook 'a-hook) 'a-hook)))

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
           (should (equal a-hook '(a))))
  ;; Lambda
  (fixture (add-hooks-pair 'a function)
           (should (equal a-hook (list function)))))

(ert-deftest add-hooks ()
  ;; Multiple
  (fixture (add-hooks '((a . a) (b . b)))
           (should (equal a-hook '(a)))
           (should (equal b-hook '(b))))
  ;; One to one
  (fixture (add-hooks '((a . a)))
           (should (equal a-hook '(a))))
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
           (should (equal a-hook '(a))))
  ;; Lambda
  (fixture (add-hooks `((a ,function)))
           (should (equal a-hook (list function)))))

(provide 'add-hooks-test)
;;; add-hooks-test.el ends here
