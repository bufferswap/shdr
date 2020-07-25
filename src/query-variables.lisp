(in-package #:bufferswap.shdr)

(defclass variable-table (primitive-table) ())

(defclass primitive/glsl-variable ()
  ((%lisp-name :reader lisp-name
               :initarg :lisp-name)
   (%name :reader name
          :initarg :name)
   (%stage :reader stage
           :initarg :stage)
   (%qualifier :reader qualifier
               :initarg :qualifier)
   (%type :reader variable-type
          :initarg :type)
   (%block :reader variable-block
           :initarg :block)
   (%instance :reader instance
              :initarg :instance)
   (%versions :reader versions
              :initarg :versions)
   (%vulkan :reader vulkan
            :initarg :vulkan)))

(u:define-printer (primitive/glsl-variable strm :type nil)
  (format strm "variable: ~(~{~A~^ ~} ~A~) ~A ~((~A~))"
          (qualifier primitive/glsl-variable)
          (variable-type primitive/glsl-variable)
          (name primitive/glsl-variable)
          (stage primitive/glsl-variable)))

;; TODO: Make higher order which takes a function that converts a spec to
;; an instance of the right kind. Then the db array creation can be refactored
;; into a single function.
(defun make-variable-table (raw-data)
  (flet ((gen-primitive/glsl-variable (spec)
           (destructuring-bind (lisp-name &rest attrs
                                &key qualifier &allow-other-keys)
               spec
             (apply #'make-instance 'primitive/glsl-variable
                    :lisp-name lisp-name
                    :qualifier (u:ensure-list qualifier)
                    attrs))))
    ;; TODO: Convert into (map 'vector ...) and refactor so others use it too.
    (let ((vt (make-instance 'variable-table
                             :db (make-array (length raw-data)))))
      (loop :for entry :in raw-data
            :for idx :from 0
            :for inst = (gen-primitive/glsl-variable entry)
            :do (setf (aref (db vt) idx) inst))
      vt)))

(defun load-variable-table (filename)
  (let ((raw-data (read-spec filename)))
    (make-variable-table raw-data)))

(defun test-variable-table (&optional (file "variables"))
  (setf *variable-table* (load-variable-table file))

  (format t "Find all variables usable in the compute stage:~%~{  ~A~%~}~%"
          (qselect 'variable-table
                   (qwhere 'stage 'eq 'meta::compute)))

  (format t "Find all out variables in the geometry stage:~%~{  ~A~%~}~%"
          (qselect 'variable-table
                   (qand (qwhere 'qualifier 'intersection '(meta::out))
                         (qwhere 'stage 'eq 'meta::geometry))))

  (format t "Find all variables that ends with \"Color\" in its name:~%~{  ~A~%~}~%"
          (qselect 'variable-table
                   (qwhere 'name 'u:string-ends-with-p "Color")))

  )
