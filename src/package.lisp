(in-package #:cl-user)

(defpackage #:bufferswap.shdr.meta
  (:use #:cl)
  (:export))

(defpackage #:bufferswap.shdr
  (:local-nicknames
   (#:u #:net.mfiano.lisp.golden-utils)
   (#:meta #:bufferswap.shdr.meta))
  (:use #:cl)
  (:export))
