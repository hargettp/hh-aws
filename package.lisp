;; Copyright (c) 2010-11 Phil Hargett

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

(defpackage :hh-aws
  (:nicknames :aws)
  (:use :cl
	:drakma
	:cl-base64)
  (:export
  
   ;; Common
   #:*credentials*
   #:access-key-id
   #:secret-key

   ;; Elastic Compute Cloud (EC2)

   #:ec2-describe-regions
   #:ec2-describe-availability-zones
   #:ec2-describe-instances
   #:ec2-describe-security-groups
   #:ec2-authorize-security-group-ingress
   #:ec2-revoke-security-group-ingress
   

   ;; Simple Storage Service (S3)

   #:s3-list-buckets
   #:s3-create-bucket
   #:s3-delete-bucket
   
   #:s3-list-bucket-objects
   #:s3-put-bucket-object
   #:s3-get-bucket-object
   #:s3-delete-bucket-object

   ;; SimpleDB

   #:db-list-domains
   #:db-create-domain
   #:db-delete-domain
   #:db-domain-metadata
   
   #:db-put-attributes
   #:db-get-attributes
   #:db-delete-attributes
   
   #:db-query
   #:db-query-with-attributes
   #:db-select

   ;; Simple Queue Service

   #:sqs-create-queue
   #:sqs-list-queues
   #:sqs-delete-queue
   #:sqs-send-message
   #:sqs-receive-message
   #:sqs-delete-message
   #:sqs-get-queue-attributes
   #:sqs-set-queue-attributes

   ))

