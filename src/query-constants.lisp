(in-package #:bufferswap.shdr)

(defclass constant-table (primitive-table) ())

(defclass primitive/glsl-constant ()
  ((%lisp-name :reader lisp-name
               :initarg :lisp-name)
   (%name :reader name
          :initarg :name)
   (%qualifier :reader qualifier
               :initarg :qualifier)
   (%type :reader constant-type
          :initarg :type)
   (%min :reader constant-min
         :initarg :min)
   (%versions :reader versions
              :initarg :versions)
   (%vulkan :reader vulkan
            :initarg :vulkan)))

(u:define-printer (primitive/glsl-constant strm :type nil)
  (format strm "constant: ~(~A ~{~A ~}~)~A"
          (constant-type primitive/glsl-constant)
          (u:ensure-list (qualifier primitive/glsl-constant))
          (name primitive/glsl-constant)))

(defun make-constant-table (raw-data)
  (flet ((gen-primitive/glsl-constant (spec)
           (destructuring-bind (lisp-name &rest attrs
                                &key qualifier &allow-other-keys)
               spec
             (apply #'make-instance 'primitive/glsl-constant
                    :lisp-name lisp-name
                    :qualifier (u:ensure-list qualifier)
                    attrs))))
    (let ((ct (make-instance 'constant-table
                             :db (make-array (length raw-data)))))
      (loop :for entry :in raw-data
            :for idx :from 0
            :for inst = (gen-primitive/glsl-constant entry)
            :do (setf (aref (db ct) idx) inst))
      ct)))

(defun load-constant-table (filename)
  (let ((raw-version-data (read-spec filename)))
    (make-constant-table raw-version-data)))

(defun test-constant-table (&optional (file "constants"))
  (setf *constant-table* (load-constant-table file))

  (format t "All constants that are ONLY in vulkan:~%  ~A~%"
          (qselect 'constant-table (qwhere 'vulkan 'eq 'meta::only)))

  (format t "Constants defined in :450 and later:~%  ~A~%"
          (qselect 'constant-table
                   (qand (qnot (qwhere 'versions 'intersection '(:440)))
                         (qwhere 'versions 'intersection '(:460)))))

  (format t "All constants that have highp qualifier:~%  ~A~%"
          (qselect 'constant-table
                   (qand (qwhere 'qualifier 'intersection '(meta::const))
                         (qwhere 'qualifier 'intersection '(meta::highp)))))

  )
