(in-package #:bufferswap.shdr)

(defparameter *version-table* nil)
(defparameter *type-table* nil)
(defparameter *constant-table* nil)
(defparameter *generic-type-table* nil)
(defparameter *function-table* nil)
(defparameter *variable-table* nil)


(defun spec-name->path (name)
  (let* ((spec-file (format nil "~(~a~).lisp" name))
         (path (asdf:system-relative-pathname
                :net.mfiano.lisp.glsl-metadata
                (uiop:merge-pathnames*
                 spec-file
                 (uiop:ensure-directory-pathname "src/spec")))))
    (if (uiop:file-exists-p path)
        path
        (error "Spec file ~s does not exist." path))))

(defun read-spec (name &optional (interning-package :bufferswap.shdr.meta))
  (u:safe-read-file-form (spec-name->path name)
                         :package interning-package))
