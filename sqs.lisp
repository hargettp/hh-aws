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

(in-package :hh-aws)

(export
 (list
  
  'sqs-create-queue
  'sqs-list-queues
  'sqs-delete-queue
  'sqs-send-message
  'sqs-receive-message
  'sqs-delete-message
  'sqs-get-queue-attributes
  'sqs-set-queue-attributes
  
  )
 )

(defservice simple-queue-service
  :endpoint ( 
             (string "queue.amazonaws.com") 
             )
  :version ( 
            (string "2008-01-01") 
            )
  :request (
	    sqs-request
	    :slots (
		    (queue-name
		     :initarg :queue
		     :initform nil
		     :accessor queue-for
		     )
		    )
	    :init (progn
		    (add-parameter some-request "Action" (default-action some-request))
		    (add-parameter some-request "AWSAccessKeyId" (access-key-id *credentials*))
		    (add-parameter some-request "SignatureMethod" "HmacSHA256")
		    (add-parameter some-request "SignatureVersion" "2")
		    (add-parameter some-request "Version" (version-of (service-of some-request)))
		    (add-parameter some-request "Timestamp" (aws-timestamp))
		    )
	    :uri (progn
		   (if (queue-for some-request)
		       (format-string "https://~a/~a"
				      (endpoint-of some-request)
				      (queue-for some-request)
				      )
		       (format-string "https://~a"
				      (endpoint-of some-request)
				      )
		       )
		   )
	    :signed-parameters (progn
				 (cons (cons "Signature" (request-signature some-request))
				       (sorted-parameters some-request)
				       )
				 )
	    )
  )

(defrequest sqs-create-queue
  
  :documentation "Create a new queue
                 "
  :bases (sqs-request)
  :service simple-queue-service
  :action (
           (string "CreateQueue")
           )
  :parameters (
               ("QueueName" . queue-name)
               )
  :result-format (
                  `(
                    "QueueUrl"
                    )
                  )
  :result (
           (mapcar (lambda (e) 
                     (car e) 
                     )
                   (call-next-method)
                   )
           )
  )

(defrequest sqs-list-queues
  
  :documentation "List queues
                 "
  :bases (sqs-request)
  :service simple-queue-service
  :action (
           (string "ListQueues")
           )
  :parameters (
               ("QueuePrefix" . queue-name)
               )
  :result-format (
                  `(
                    "QueueUrl"
                    )
                  )  
  :result (
           (mapcar (lambda (e) 
                     (car e) 
                     )
                   (call-next-method)
                   )
           )
  )

(defrequest sqs-delete-queue
  
  :documentation "Delete queue
                 "
  :bases (sqs-request)
  :service simple-queue-service
  :action (
           (string "DeleteQueue")
           )
  :args (
         queue-name
         )
  :call (
         (setf (queue-for some-request) queue-name)
         (setf (uri-of some-request) (format-string "/~a" queue-name))
         (call-next-method)
         )
  :result (
           t
           )
  )

(defrequest sqs-send-message
  
  :documentation "Send a message to the specified queue
                 "
  :bases (sqs-request)
  :service simple-queue-service
  :action (
           (string "SendMessage")
           )
  :args (
         queue-name
         message-body
         )
  :call (
         (setf (queue-for some-request) queue-name)
         (setf (uri-of some-request) (format-string "/~a" queue-name))
         
         (add-parameter some-request "MessageBody" message-body)
         
         (call-next-method)
         )
  :result (
           t
           )
  )

(defrequest sqs-receive-message
  
  :documentation "Receive a message from the queue
                 "
  :bases (sqs-request)
  :service simple-queue-service
  :action (
           (string "ReceiveMessage")
           )
  :args (
         queue-name
         )
  :call (
         (setf (queue-for some-request) queue-name)
         (setf (uri-of some-request) (format-string "/~a" queue-name))
         (call-next-method)
         )
  :result-format (
                  `(
                    "MessageId"
                    "ReceiptHandle"
                    "MD5OfBody"
                    "Body"
                    )
                  )  
  )

(defrequest sqs-delete-message
  
  :documentation "Delete a message from a queue
                 "
  :bases (sqs-request)
  :service simple-queue-service
  :action (
           (string "DeleteMessage")
           )
  :args (
         queue-name
         receipt-handle
         )
  :call (
         (setf (queue-for some-request) queue-name)
         (setf (uri-of some-request) (format-string "/~a" queue-name))
         
         (add-parameter some-request "ReceiptHandle" receipt-handle)
         
         (call-next-method)
         )
  :result (
           t
           )
  )

(defrequest sqs-get-queue-attributes
  
  :documentation "Get queue attributes
                 "
  :bases (sqs-request)
  :service simple-queue-service
  :action (
           (string "GetQueueAttributes")
           )
  :args (
         queue-name
         )
  :call (
         (setf (queue-for some-request) queue-name)
         (setf (uri-of some-request) (format-string "/~a" queue-name))
         (add-parameter some-request "AttributeName" "All")
         (call-next-method)
         )
  :result-format (
                  `(
                    "Name"
                    "Value"
                    )
                  )  
    )

(defrequest sqs-set-queue-attributes
  
  :documentation "Set queue attributes
                 "
  :bases (sqs-request)
  :service simple-queue-service
  :action (
           (string "SetQueueAttributes")
           )
  :args (
         queue-name
         visibility-timeout
         )
  :call (
         (setf (queue-for some-request) queue-name)
         (setf (uri-of some-request) (format-string "/~a" queue-name))
         (add-parameter some-request "Attribute.Name" "VisibilityTimeout")
         (add-parameter some-request "Attribute.Value" (format-string "~a" visibility-timeout))
         (call-next-method)
         )
  :result (
           t
           )  
    )
