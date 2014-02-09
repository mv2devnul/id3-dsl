(in-package #:cl-user)

(defpackage #:utils
  (:export #:*standard-optimize-settings*
           #:aif
           #:awhen
           #:dbg
           #:dbg-helper
           #:defconstant*
           #:dump-data
           #:get-bitfield
           #:it
           #:make-keyword
           #:make-octets
           #:memoize
           #:mkstr
           #:mksym
           #:octet
           #:octets
           #:printable-array
           #:redirect
           #:timings
           #:upto-null
           #:warn-user *break-on-warn-user*
           #:while
           #:with-gensyms)
  (:use #:common-lisp))

(defpackage #:id3-dsl
  (:use #:common-lisp
    #:utils
    #:zlib
    #:com.gigamonkeys.binary-data
    #:com.gigamonkeys.binary-data.common-datatypes))
