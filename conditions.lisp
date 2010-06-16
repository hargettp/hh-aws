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

(in-package :hh-aws)

(define-condition aws-error (error)
  (
   (code
    :initarg :code
    :accessor error-code
    )
   (message
    :initarg :message
    :accessor error-message
    )
   (response
    :initarg :response
    :initform nil
    :accessor error-response
    )
   )
  ( :report (lambda (aws-condition stream)
              (format stream "Amazon Web Services reported an error : ~a - ~a"
                      (error-code aws-condition)
                      (error-message aws-condition) 
                      )
              )
   )
  )

(define-condition invalid-client-token-id (aws-error)
  ()
  )

(define-condition service-unavailable (aws-error)
  ()
  )

(define-condition no-such-domain (aws-error)
  ()
  )

(define-condition signature-does-not-match (aws-error)
  ()
  )

(define-condition invalid-query-expression (aws-error)
  ()
  )

(define-condition no-such-bucket (aws-error)
  ()
  )

(define-condition bad-digest (aws-error)
  ()
  )

(define-condition bucket-not-empty (aws-error)
  ()
  )

(defmacro raise-aws-condition (code message response)
  `(macrolet ( 
              (condition-option (condition-code condition-type)
                `(if (equal code ,condition-code)
                     (setq condition-class (quote ,condition-type))
                     )
                )
              )
     (let ( (condition-class 'aws-error) )
       (condition-option "InvalidClientTokenId" invalid-client-token-id)
       (condition-option "ServiceUnavailable" service-unavailable)
       (condition-option "NoSuchDomain" no-such-domain)
       (condition-option "SignatureDoesNotMatch" signature-does-not-match)
       (condition-option "InvalidQueryExpression" invalid-query-expression)
       (condition-option "NoSuchBucket" no-such-bucket)
       (condition-option "BadDigest" bad-digest)
       (condition-option "BucketNotEmpty" bucket-not-empty)
       
       (error condition-class 
              :code ,code 
              :message ,message 
              :response ,response
              )
       )
     )
  )
