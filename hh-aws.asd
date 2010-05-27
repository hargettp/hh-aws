;;;; Created on 2009-02-02 20:09:23

(defpackage #:hh-aws-asd
  (:use :cl :asdf))

(in-package :hh-aws-asd)

(defsystem hh-aws
  :name "hh-aws"
  :version "0.1"
  :components (
               (:file "package-hh-aws")
               (:file "constants" :depends-on ("package-hh-aws"))
               (:file "conditions" :depends-on ("package-hh-aws"))
               (:file "xml" :depends-on ("package-hh-aws"))
               (:file "common" :depends-on ("constants" "conditions" "xml"))
               (:file "simpledb" :depends-on ("common"))
               (:file "s3" :depends-on ("common"))
               (:file "sqs" :depends-on ("common"))
	       
               )
  :depends-on (
	       ;; external packages
	       "drakma" 
	       "puri"
	       "cl-base64"
	       "ironclad"
	       "s-xml"
               
	       ;; local packages
	       "hh-utils"
	       )
  )

(defsystem hh-aws-tests
  :name "hh-aws-tests"
  :version "0.1"
  :components (
               (:file "tests")
	       )
  :depends-on (
	       ;; external packages
               "lisp-unit"

	       ;; local packages
	       "hh-aws"
	       )
  )
