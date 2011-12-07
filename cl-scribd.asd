;;;; cl-scribd.asd

(asdf:defsystem #:cl-scribd
  :serial t
  :description "Commong Lisp Client for the Scribd API."
  :author "José Pereira <jsmpereira@gmail.com>"
  :depends-on (#:drakma
               #:cxml
               #:ironclad)
  :components ((:file "package")
               (:file "cl-scribd")))

