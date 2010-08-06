;; Copyright (c) 2010 Haphazard House LLC

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(defpackage #:hh-aws-asd
  (:use :cl :asdf))

(in-package :hh-aws-asd)

(defsystem hh-aws
  :name "hh-aws"
  :version "0.01"
  :components (
               (:file "package-hh-aws")
	       (:file "utils" :depends-on ("package-hh-aws"))
               (:file "constants" :depends-on ("package-hh-aws" "utils"))
               (:file "conditions" :depends-on ("package-hh-aws" "utils"))
               (:file "xml" :depends-on ("package-hh-aws" "utils"))
               (:file "common" :depends-on ("constants" "conditions" "xml"))
               (:file "sdb" :depends-on ("common"))
               (:file "s3" :depends-on ("common"))
               (:file "sqs" :depends-on ("common"))
               (:file "ec2" :depends-on ("common"))
	       
               )
  :depends-on (
	       ;; external packages
	       "drakma" 
	       "puri"
	       "cl-base64"
	       "ironclad"
	       "s-xml"
               
	       ;; local packages
	       )
  )

(defsystem hh-aws-tests
  :name "hh-aws-tests"
  :version "0.01"
  :components (
               (:file "tests")
	       )
  :depends-on (
	       ;; external packages
               "lisp-unit"
	       "uuid"

	       ;; local packages
	       "hh-aws"
	       )
  )
