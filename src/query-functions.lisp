(in-package #:bufferswap.shdr)

(defclass function-table (primitive-table) ())

(defclass primitive/glsl-function ()
  ((%lisp-name :reader lisp-name
               :initarg :lisp-name)
   (%name :reader name
          :initarg :name)
   (%parameters :reader parameters
                :initarg :parameters)
   (%return :reader function-return
            :initarg :return)
   (%stages :reader stages
            :initarg :stages)
   (%versions :reader versions
              :initarg :versions)
   (%vulkan :reader vulkan
            :initarg :vulkan)))

(u:define-printer (primitive/glsl-function strm :type nil)
  ;; TODO: Make it print out like GLSL's spec.
  (format strm "function: ~(~A ~A ~A~)"
          (function-return primitive/glsl-function)
          (lisp-name primitive/glsl-function)
          (parameters primitive/glsl-function)))

(defun make-function-table (raw-data)
  (flet ((gen-primitive/glsl-function (spec)
           (destructuring-bind (lisp-name . attrs)
               spec
             (apply #'make-instance 'primitive/glsl-function
                    :lisp-name lisp-name
                    attrs))))
    (let ((ft (make-instance 'function-table
                             :db (make-array (length raw-data)))))
      (loop :for entry :in raw-data
            :for idx :from 0
            :for inst = (gen-primitive/glsl-function entry)
            :do (setf (aref (db ft) idx) inst))
      ft)))

(defun load-function-table (filename)
  (let ((raw-data (read-spec filename)))
    (make-function-table raw-data)))

(defun test-function-table (&optional (file "functions"))
  (setf *function-table* (load-function-table file))

  #++(format t "Functions defined in :460 and later:~%~{  ~A~%~}~%"
             (qselect 'function-table
                      (qand (qnot (qwhere 'versions 'intersection '(:450)))
                            (qwhere 'versions 'intersection '(:460)))))

  (format t "Functions with more than one parameter:~%~{  ~A~%~}~%"
          (qselect 'function-table
                   (qwhere 'parameters
                           (lambda (x y) (> (length x) y))
                           1)))

  (format t "Functions not usable in the fragment stage:~%~{  ~A~%~}~%"
          (qselect 'function-table
                   (qand (qnot (qwhere 'stages 'eq 'meta::all))
                         (qnot (qwhere 'stages
                                       (lambda (column-value user-value)
                                         (find user-value column-value))
                                       'meta::fragment)))))

  (format t "Function that mutate their arguments:~%~{  ~A~%~}~%"
          (qselect 'function-table
                   (qwhere 'parameters
                           (lambda (column-value user-value)
                             (some (lambda (z)
                                     (intersection z user-value))
                                   column-value))
                           '(meta::out meta::inout))))

  ;; TODO: All functions that have an overload. Needs GROUP BY.

  (format t "All overloads for the abs function:~%~{  ~A~%~}~%"
          (qselect 'function-table
                   (qwhere 'lisp-name 'eq 'meta::abs)))

  )
