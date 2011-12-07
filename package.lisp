;;;; package.lisp

(defpackage #:cl-scribd
  (:use #:cl)
  (:export #:*api-key*
           #:*api-secret*
           #:build-api-call))

