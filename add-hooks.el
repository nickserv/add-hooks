;;; add-hooks.el --- Macro for setting multiple hooks

;; Copyright (C) 2017 Nick McCurdy

;; Author: Nick McCurdy <nick@nickmccurdy.com>
;; Created: 22 Jan 2017
;; Version: 1.0.0
;; Keywords: lisp
;; Homepage: https://github.com/nickmccurdy/add-hooks
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the gnu general public license as
;; published by the free software foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  see the gnu
;; general public license for more details.

;; You should have received a copy of the gnu general public license
;; along with gnu emacs; see the file copying.  if not, write to the
;; free software foundation, inc., 59 temple place - suite 330,
;; boston, ma 02111-1307, usa.

;;; Commentary:

;; Typically, you would need to call `add-hook' multiple times with
;; similar arguments to declare multiple functions for one hook, or
;; vice versa.  The `add-hooks' macro tidies up duplicate hook and
;; function names, with syntax inspired by `bind-key'.
;;
;; Please see README.md from the same repository for documentation.

;;; Code:

(defun add-hooks-listify (object)
  "If OBJECT is a list, return it, else wrap it in a list."
  (if (listp object) object (list object)))

(defun add-hooks-mapflat (function sequence)
  "Apply FUNCTION to each element of SEQUENCE, and make a list of the results.
Unlike `mapcar', the list is flattened nondestructively before it is returned."
  (apply #'append (mapcar function sequence)))

;;;###autoload
(defmacro add-hooks (&rest args)
  "Call `add-hook' on each cons pair in ARGS.
Each pair has a `car' for setting hooks and a `cdr' for setting
functions to add to those hooks.  Either side of the pair can be
a single symbol or a list of symbols, in which case a function
can be added to multiple hooks and/or multiple functions can be
added to a hook."
  (macroexp-progn
   (add-hooks-mapflat
    (lambda (arg)
      (let ((hooks (add-hooks-listify (car arg)))
            (functions (add-hooks-listify (cdr arg))))
        (add-hooks-mapflat
         (lambda (hook)
           (mapcar (lambda (function) `(add-hook ',hook ',function)) functions))
         hooks)))
    args)))

(provide 'add-hooks)
;;; add-hooks.el ends here
