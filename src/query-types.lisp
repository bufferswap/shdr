(in-package #:bufferswap.shdr)

(defclass type-table (primitive-table) ())

(defclass primitive/glsl-type ()
  ((%symbol :reader lisp-name
            :initarg :lisp-name)
   (%glsl-name :reader glsl-name
               :initarg :name)
   (%opaque :reader opaque
            :initarg :opaque)
   (%composite :reader composite
               :initarg :composite)
   (%aliases :reader aliases
             :initarg :aliases)
   (%dimensions :reader dimensions
                :initarg :dimensions)
   (%versions :reader versions
              :initarg :versions)
   (%vulkan :reader vulkan
            :initarg :vulkan)))

(u:define-printer (primitive/glsl-type strm :type nil)
  (format strm "Type: ~a~(~@[ opaque: ~a~]~@[ composite: ~a ~a~]~)"
          (glsl-name primitive/glsl-type)
          (opaque primitive/glsl-type)
          (composite primitive/glsl-type)
          (dimensions primitive/glsl-type)))

(defun make-type-table (raw-data)
  (flet ((gen-primitive/glsl-type (spec)
           (destructuring-bind (lisp-name &rest attrs
                                &key dimensions &allow-other-keys)
               spec
             (apply #'make-instance 'primitive/glsl-type
                    :lisp-name lisp-name
                    :dimensions (u:ensure-list dimensions)
                    attrs))))
    (let ((tt (make-instance 'type-table
                             :db (make-array (length raw-data)))))
      (loop :for entry :in raw-data
            :for idx :from 0
            :for inst = (gen-primitive/glsl-type entry)
            :do (setf (aref (db tt) idx) inst))
      tt)))

(defun load-type-table (filename)
  (let ((raw-data (read-spec filename)))
    (make-type-table raw-data)))

(defun test-type-table (&optional (file "types"))
  (setf *type-table* (load-type-table file))

  (flet ((a-short-query (x)
           (qwhere 'versions 'intersection x)))

    (format t "Types defined in :460 and later:~%  ~A~%"
	    (qselect 'type-table
		     (qand (qnot (qwhere 'versions 'intersection '(:450)))
			   (qwhere 'versions 'intersection '(:460)))))
    (format t "Types defined in :120 whose dimension is 2:~%  ~A~%"
	    (qselect 'type-table (qand (a-short-query '(:120))
				       (qwhere 'dimensions 'equal '(2)))))
    (format t "Types in :460 whose dimension is 2:~%  ~A~%"
	    (qselect 'type-table (qand (a-short-query '(:460))
				       (qwhere 'dimensions 'equal '(2)))))
    (format t "Types in :460 whose composite is double:~%  ~A~%"
	    (qselect 'type-table
		     (qand (a-short-query '(:460))
			   (qwhere 'composite 'equal 'meta::double))))

    (format t "Types which have alias dmat2x2:~%  ~A~%"
	    (qselect 'type-table
		     (qwhere 'aliases 'intersection '(meta::dmat2x2)))))

    )
