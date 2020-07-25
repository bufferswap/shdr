(in-package :bufferswap.shdr)

(defclass generic-type-table (primitive-table) ())

(defclass primitive/glsl-generic-type ()
  ((%symbol :reader lisp-name
            :initarg :lisp-name)
   (%types :reader types
	   :initarg :types)
   (%versions :reader versions
	      :initarg :versions)))

(u:define-printer (primitive/glsl-generic-type strm :type nil)
  (format strm "~(~A~)" (lisp-name primitive/glsl-generic-type)))

(defun make-generic-type-table (raw-data)
  (flet ((gen-primitive/glsl-generic-type (spec)
           (destructuring-bind (lisp-name . attrs)
               spec
             (apply #'make-instance 'primitive/glsl-generic-type
                    :lisp-name lisp-name
                    attrs))))
    (let ((gtt (make-instance 'generic-type-table
                             :db (make-array (length raw-data)))))
      (loop :for entry :in raw-data
            :for idx :from 0
            :for inst = (gen-primitive/glsl-generic-type entry)
            :do (setf (aref (db gtt) idx) inst))
      gtt)))

(defun load-generic-type-table (filename)
  (let ((raw-data (read-spec filename)))
    (make-generic-type-table raw-data)))

(defun test-generic-type-table (&optional (file "generic-types"))
  (setf *generic-type-table* (load-generic-type-table file))

  (format t "Generic types defined in :460 and later:~%  ~A~%"
	  (qselect 'generic-type-table
		   (qand (qnot (qwhere 'versions 'intersection '(:450)))
			 (qwhere 'versions 'intersection '(:460)))))
  )
