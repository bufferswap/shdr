(asdf:defsystem #:bufferswap.shdr
  :description ""
  :author ("Peter Keller <psilord@cs.wisc.edu>"
           "Bart Botta <00003b@gmail.com>"
           "Jack Ladwig <jack@ladwig.dev>"
           "Michael Fiano <mail@mfiano.net>")
  :license "MIT"
  :homepage "https://github.com/bufferswap/shdr"
  :source-control (:git "https://github.com/bufferswap/shdr")
  :bug-tracker "https://github.com/bufferswap/shdr/issues"
  :encoding :utf-8
  :depends-on (:net.mfiano.lisp.golden-utils
               :net.mfiano.lisp.glsl-metadata)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "query-engine")
   (:file "query-version")
   (:file "query-types")
   (:file "query-constants")
   (:file "query-generic-types")))
