(defun listify (object)
  "If OBJECT is a list, return it, else wrap it in a list."
  (if (listp object) object (list object)))

(defun mapflat (function sequence)
  "Apply FUNCTION to each element of SEQUENCE, and make a list of the results.
Unlike `mapcar', the list is flattened nondestructively before it is returned."
  (apply #'append (mapcar function sequence)))

(defmacro add-hooks (&rest args)
  "Call `add-hook' on each cons pair in ARGS.
Each pair has a `car' for setting hooks and a `cdr' for setting
functions to add to those hooks.  Either side of the pair can be
a single symbol or a list of symbols, in which case a function
can be added to multiple hooks and/or multiple functions can be
added to a hook."
  (macroexp-progn
   (mapflat (lambda (arg)
              (let ((hooks (listify (car arg)))
                    (functions (listify (cdr arg))))
                (mapflat (lambda (hook)
                           (mapcar (lambda (function)
                                     `(add-hook ',hook ',function))
                                   functions))
                         hooks)))
            args)))