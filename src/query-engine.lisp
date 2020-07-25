(in-package #:bufferswap.shdr)

;; SQL WHERE-like higher order function.
(defun qwhere (func-designator func-compare value)
  (lambda (tm)
    (funcall func-compare (funcall func-designator tm) value)))

(defun qand (&rest funcs)
  (lambda (tm) (every (lambda (a) (funcall a tm)) funcs)))

(defun qor (&rest funcs)
  (lambda (tm) (some (lambda (a) (funcall a tm)) funcs)))

(defun qnot (func)
  (complement func))

(defun qselect (from where)
  (let ((results nil)
        (table (ecase from
                 (version-table *version-table*)
                 (type-table *type-table*)
		 (constant-table *constant-table*))))
    (loop :for entry :across (db table)
          :do (when (funcall where entry)
                (push entry results)))
    results))
