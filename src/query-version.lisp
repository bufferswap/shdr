(in-package #:bufferswap.shdr)

;; base storage class for all primitive data
(defclass primitive-table ()
  ((%db :reader db
        :initarg :db)))

(defclass version-table (primitive-table) ())

;; type of a single primitive version instance
(defclass primitive/glsl-version ()
  (;; The lisp version keyword symbol
   (%sym :reader sym
         :initarg :sym)
   ;; The integer version number
   (%version :reader version
             :initarg :version)
   ;; Chronological release number compats are after their cores.
   (%release :reader release
             :initarg :release)
   ;; Is this version a compaibility version.
   (%compat-p :reader compat-p
              :initarg :compat-p)))

(u:define-printer (primitive/glsl-version strm :type nil)
  (format strm "~(~S~)" (sym primitive/glsl-version)))

(defun make-version-table (raw-data)
  (let ((vt (make-instance 'version-table
                           :db (make-array (length raw-data)))))

    (loop :for entry :in raw-data
          :for entry-string = (symbol-name entry)
          :for idx :from 0
          :for release :from 0
          :for version-number = (parse-integer entry-string :junk-allowed t)
          :do (setf (aref (db vt) idx)
                    (make-instance 'primitive/glsl-version
                                   :sym entry
                                   :version version-number
                                   :release release
                                   :compat-p (when (search "-COMPAT"
                                                           entry-string
                                                           :test #'string=)
                                               t))))
    vt))

(defun load-version-table (filename)
  (let ((raw-version-data (read-spec filename)))
    (make-version-table raw-version-data)))

(defun test-version-table (&optional (file "versions"))
  (setf *version-table* (load-version-table file))

  (format t ":110 -> ~S~%"
	  (qselect 'version-table (qwhere 'sym 'eq :110)))
  (format t ":foobar -> ~S~%"
	  (qselect 'version-table (qwhere 'sym 'eq :foobar)))
  (format t ":450-compat -> ~S~%"
	  (qselect 'version-table (qand (qwhere 'version '= 450)
					(qwhere 'compat-p 'eq t))))
  )



;; repo shdr. package and system bufferswap.shdr
