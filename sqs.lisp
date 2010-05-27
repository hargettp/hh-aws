;;;; Created on 2009-02-26 21:44:31

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
