;;;; Created on 2009-02-07 08:57:18

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
