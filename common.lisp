;;;; Created on 2009-02-02 20:10:44

(in-package :hh-aws)

(export
 (list
  
  )
 )

(defclass credentials ()
  (
   )
  (:documentation 
    "Encapsulates credentials used for authenticating requests to AWS"
    )
  )

(defclass empty-credentials (credentials)
  (
   )
  )

(defclass explicit-credentials (credentials)
  (
   (access-key-id
    :initarg :access-key-id
    )
   (secret-key
    :initarg :secret-key
    )
   )
  (:documentation
    "Credentials explicitly initialized to specific values
    "
    )
  )

(defclass user-private-credentials (credentials)
  (
   )
  (:documentation
    "Credentials are stored in a pair of files in ~/.aws; 1 file
     for the access key id, and one for the secret key.
    "
    )
  )

(defvar *credentials* (make-instance 'user-private-credentials)
  "Credentials for authenticating against Amazon Web Services"
  )

(defgeneric access-key-id (some-credentials)
  (:documentation 
    "Return the access key associated with the credentials,
    to be sent with requests
    "
    )
  (:method ((some-credentials empty-credentials))
    (string "<access-key-id>")
    )
  (:method ((some-credentials explicit-credentials))
    (slot-value some-credentials 'access-key-id)
    )
  (:method ((some-credentials user-private-credentials))
    (string-trim 
     '(#\Space #\Newline #\Tab)
     (with-open-file 
         (is (merge-pathnames ".aws/access-key-id" (home-dir)))
       (read-line is)
       )
     )
    )
  )

(defgeneric secret-key (some-credentials)
  (:documentation
    "Return the secret key used to sign requests
    "
    )
  (:method ((some-credentials empty-credentials))
    (string "<secret-key>")
    )
  (:method ((some-credentials explicit-credentials))
    (slot-value some-credentials 'secret-key)
    )
  (:method ((some-credentials user-private-credentials))
    (string-trim 
     '(#\Space #\Newline #\Tab)
     (with-open-file 
         (is (merge-pathnames ".aws/secret-key" (home-dir)))
       (read-line is)
       )
     )
    )  
  )

(defclass service ()
  (
   
   )
  (:documentation
   "Encapsulates a specific functional AWS (e.g., S3, SimpleDB, etc.)"
   )
  )

(defgeneric endpoint-of (a-service)
  (:documentation 
    "Hostname of AWS service endpoint (e.g., s3.amazon.com)
    "
    )  
  )

(defgeneric version-of (a-service)
  (:documentation 
    "Return the expected version of the service
    "
    )
  )

(defun aws-timestamp (&optional (time (get-universal-time)))
  "Return a HTTP-style date string.  Borrowed from Zach Bean's ZS3:
      http://www.xach.com/lisp/zs3/ (BSD-license)
  "
  (multiple-value-bind (second minute hour day month year day-of-week
                               daylight zone)
    (decode-universal-time time 0)
    (declare (ignore day-of-week)
             (ignore daylight)
             (ignore zone)
             )
    (with-output-to-string (os)
      (format os "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
              year month day hour minute second
              )
      )
    )
  )

(defun aws-date (&optional (time (get-universal-time)))
  ;; Tue, 27 Mar 2007 19:36:42 +0000
  (let ( 
        (days-of-week '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
        (months-of-year '("Jan" "Feb" "Mar" "Apr" "May" "Jun" 
                          "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"
                                )
                        ) 
        )
    (multiple-value-bind (second minute hour day month year day-of-week
                                 daylight zone)
      (decode-universal-time time 0)
      (declare (ignore daylight)
               (ignore zone)
               )
      (with-output-to-string (os)
        (format os "~a, ~a ~a ~a ~2,'0D:~2,'0D:~2,'0D +0000"
                (elt days-of-week day-of-week) 
                day 
                (elt months-of-year (1- month)) 
                year hour minute second
                )
        )
      )
    )
  )

(defclass message ()
  (
   (
    service
    :initarg :service
    :accessor service-of
    :documentation "Service source or destination of the message"
    )
   (uri
    :initarg :uri
    :accessor uri-of
    :initform "/"
    :documentation "URI that is the source or destinationof the message"
    )
   )
  )

(defclass request (message)
  (
   (parameters
    :initform (make-hash-table :test 'equal)
    :accessor parameters-of
    :documentation "Parameters accompanying the message as name-value pairs"
    )
   (headers
    :initform (make-hash-table :test 'equal)
    :accessor headers-of
    :documentation "HTTP headers associated with the request"
    )
   )
  (:documentation
   "Encapsulates a single request sent to a service instance of AWS"
   )
  )

(defclass db-request ()
  ()
  (:documentation
    "Base for requests targeting SimpleDB"
    )
  )

(defclass s3-request ()
  (
   (bucket
    :initform nil
    :initarg :bucket
    :accessor bucket-for
    )
   (bucket-object
    :initform nil
    :initarg :object
    :accessor bucket-object-for
    )
   (object-content
    :initform nil
    :initarg :content
    :accessor object-content-for
    )
   )
  (:documentation
    "Base for requests targeting S3"
    )
  )

(defclass sqs-request ()
  (
   (queue-name
    :initarg :queue
    :initform nil
    :accessor queue-for
    )
   )
  )

(defgeneric make-request (a-service a-request-type)
  (:documentation
    "Create a new request for the service
    "
    )
  (:method ((a-service t) (a-request-type symbol))
    (make-instance a-request-type
                         :service a-service
                         )
    )
  )

(defgeneric signature-digest-algorithm (some-request)
  (:documentation
    "Return the cryptographic diegest algorithm to use 
     when signing the request
    "
    )
  (:method ((some-request request))
    'ironclad:sha256
    )
  (:method ((some-request s3-request))
    'ironclad:sha1
    )
  )

(defgeneric add-parameter (some-request parameter-name parameter-value)
  (:documentation
    "Add a parameter to the request
    "
    )
  (:method ((some-request request) parameter-name parameter-value)
    (setf 
     (gethash parameter-name (parameters-of some-request)) 
     parameter-value
     )    
    )
  )

(defgeneric add-header (some-request header-name header-value)
  (:documentation
    "Add a header (usually an amz header) to the request
    "
    )
  (:method ((some-request s3-request) header-name header-value)
    (setf 
     (gethash header-name (headers-of some-request)) 
     header-value
     )    
    )
  )

(defun url-encode (a-string)
  "URL encode a string
  "
  (flet ( 
         (unreserved-p (c)
           (find c 
"ABCDEFGHIJKLMNOPQRTSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.~" 
                 :test #'equal
                 )
           )
         (url-encoded-char (c)
           (with-output-to-string (os)
             (format os "%~X" (char-code c))
             )
           ) 
         )
    (with-output-to-string (os)
      (dotimes (i (length a-string))
        (let ( (c (elt a-string i)) )
          (if (unreserved-p c)
              (format os "~a" c)
              (format os "~a" (url-encoded-char c))
              )   
          )
        )
      )
    )

  )

(defun url-decode (a-string)
  
  )

(defgeneric default-action (some-request)
  (:documentation
    "Return the default action associated with this request
    "
    )
  )

(defmethod initialize-instance ((some-request db-request) &key )
  (call-next-method)
  (add-parameter some-request "Action" (default-action some-request))
  (add-parameter some-request "AWSAccessKeyId" (access-key-id *credentials*))
  (add-parameter some-request "SignatureMethod" "HmacSHA256")
  (add-parameter some-request "SignatureVersion" "2")
  (add-parameter some-request "Version" (version-of (service-of some-request)))
  (add-parameter some-request "Timestamp" (aws-timestamp))
  )

(defmethod initialize-instance ((some-request sqs-request) &key )
  (call-next-method)
  (add-parameter some-request "Action" (default-action some-request))
  (add-parameter some-request "AWSAccessKeyId" (access-key-id *credentials*))
  (add-parameter some-request "SignatureMethod" "HmacSHA256")
  (add-parameter some-request "SignatureVersion" "2")
  (add-parameter some-request "Version" (version-of (service-of some-request)))
  (add-parameter some-request "Timestamp" (aws-timestamp))
  )

(defmethod initialize-instance ((some-request s3-request) &key )
  (call-next-method)
  (add-header some-request "Date" (aws-date))
 )

(defgeneric method-of (some-request)
  (:method ((some-request request))
    :get
    )
  )

(defmethod endpoint-of ((some-request request))
  (endpoint-of (service-of some-request))  
  )

(defmethod endpoint-of ((some-request s3-request))
  (if (bucket-for some-request)
      (format-string "~a.~a"
                     (bucket-for some-request)
                     (call-next-method) 
                     )
      (call-next-method)
      )  
  )

(defgeneric http-host-header (some-request)
  (:method ((some-request request))
    (endpoint-of some-request)
    )
  )

(defgeneric http-request-uri (some-request)
  (:method ((some-request request))
    (uri-of some-request)
    )
  )

(defgeneric http-verb (some-request)
  (:method ((some-request request))
    (symbol-name (method-of some-request))
    )
  )

(defgeneric sorted-parameters (some-request)
  (:documentation
    "Sort the parameters of the request according to byte-ordering
     of names, and return the sorted list
    "
    )
  (:method ((some-request request))
    (sort-hashtable (parameters-of some-request) 
          (lambda (p1 p2)
            (string< (car p1) (car p2))
            )
          )
    )
  )

(defgeneric sorted-headers (some-request)
  (:documentation
    "Sort the headers of the request according to byte-ordering
     of names, and return the sorted list
    "
    )
  (:method ((some-request request))
    (sort-hashtable (headers-of some-request) 
          (lambda (p1 p2)
            (string< (car p1) (car p2))
            )
          )
    )
  )

(defgeneric canonical-query-string (some-request)
  (:method ((some-request db-request))
    ( let* ( 
            (sorted-parameters (sorted-parameters some-request))
            (joined-pairs (map-name-value-pairs 
                           sorted-parameters
                           (lambda (n v) 
                             (with-output-to-string (os)
                               (format os "~a=~a" n (url-encode v))
                               )
                             )
                           )
                          )
            )
     (detokenize joined-pairs "&")
     )
    )
  (:method ((some-request sqs-request))
    ( let* ( 
            (sorted-parameters (sorted-parameters some-request))
            (joined-pairs (map-name-value-pairs 
                           sorted-parameters
                           (lambda (n v) 
                             (with-output-to-string (os)
                               (format os "~a=~a" n (url-encode v))
                               )
                             )
                           )
                          )
            )
     (detokenize joined-pairs "&")
     )
    )
  )

(defgeneric md5-digest (some-content)
  (:documentation
    "Create an MD5 digest of the provided content
    "
    )
  (:method ((some-content string))
    (let* (
           (sequence-bytes (string-to-bytes some-content)) 
           (the-digest (ironclad:make-digest 'ironclad:md5)) 
           )
      (ironclad:update-digest the-digest sequence-bytes)
      (cl-base64:string-to-base64-string 
       (bytes-to-string (ironclad:produce-digest the-digest))
       )
      )
    )
  )

(defgeneric content-md5-of (some-request)
  (:method ((some-request s3-request))
    (if (object-content-for some-request)
        (md5-digest (object-content-for some-request))
        (string "")
        )
    )
  )

(defgeneric content-type-of (some-request)
  (:method ((some-request s3-request))
    (if (object-content-for some-request)
        (string "text/plain")
        (string "")
        )
    )
  )

(defgeneric content-length-of (some-request)
  (:method ((some-request s3-request))
    (if (object-content-for some-request)
        (length (object-content-for some-request))
        0
        )
    )
  )

(defgeneric date-header-of (some-request)
  (:method ((some-request request))
    (gethash "Date" (headers-of some-request))
    )
  )

(defgeneric has-amz-headers-p (some-request)
  (:method ((some-request request))
    (some (lambda (h)
            (string-starts-with (car h) "x-amz-")
            )
          (sorted-parameters some-request)
          )
    ) 
  )

(defgeneric canonicalized-amz-headers-of (some-request)
  (:documentation
    "Return a canonical string representation of amz headers
    "
    )
  (:method ((some-request s3-request))
    (with-output-to-string (os)
      (dolist (header (sorted-headers some-request))
        (if (string-starts-with (car header) "x-amz")
            (format os "~a: ~a\n" (car header) (cdr header))
            )
        )
      )
    )
  )

(defgeneric canonicalized-resource-of (some-request)
  (:method ((some-request s3-request))
    (if (bucket-for some-request)
        (if (bucket-object-for some-request)
            (format-string "/~a/~a"
                           (bucket-for some-request)
                           (bucket-object-for some-request) 
                           )
            (format-string "/~a/" (bucket-for some-request))
            )
        "/"
        )
    
    )
  )

(defgeneric request-string-to-sign (some-request)
  (:documentation
    "String representation of request prepared for signing
    "
    )
  (:method ((some-request db-request))
    (format-string "~a~%~a~%~a~%~a"
                   (http-verb some-request)
                   (http-host-header some-request)
                   (uri-of some-request)
                   (canonical-query-string some-request)
                   )
    )
  (:method ((some-request sqs-request))
    (format-string "~a~%~a~%~a~%~a"
                   (http-verb some-request)
                   (http-host-header some-request)
                   (uri-of some-request)
                   (canonical-query-string some-request)
                   )
    )
    (:method ((some-request s3-request))
    (if (has-amz-headers-p some-request)
        (format-string "~a~%~a~%~a~%~a~%~a~%~a"
                       (http-verb some-request)
                       (content-md5-of some-request)
                       (content-type-of some-request)
                       (date-header-of some-request)
                       (canonicalized-amz-headers-of some-request)
                       (canonicalized-resource-of some-request)
                       )
        (format-string "~a~%~a~%~a~%~a~%~a"
                       (http-verb some-request)
                       (content-md5-of some-request)
                       (content-type-of some-request)
                       (date-header-of some-request)
                       (canonicalized-resource-of some-request)
                       )
        )
    )
  )

(defgeneric request-signature (some-request)
  (:documentation 
    "Compute the signature of the request and return
     it's base64 representation (note: using HMAC256)
     "
    )
  (:method ((some-request request))
    (let* (
           (key-bytes (string-to-bytes (secret-key *credentials*)))
           (sequence-bytes (string-to-bytes (request-string-to-sign some-request))) 
           (the-hmac (ironclad:make-hmac 
                      key-bytes 
                      (signature-digest-algorithm some-request)
                      )
                     ) 
           )
      (ironclad:update-hmac the-hmac sequence-bytes)
      (cl-base64:string-to-base64-string 
       (bytes-to-string (ironclad:hmac-digest the-hmac))
       )
      )
    )
  )

(defgeneric signed-parameters-of (some-request)
  (:documentation 
    "Return the signed form of parameters for the request
    (means the addition of the signature parameter); at the moment,
    only SimpleDB requires this method.
    "
    )
  (:method ((some-request request))
    (sorted-parameters some-request)
    )
  (:method ((some-request db-request))
    (cons (cons "Signature" (request-signature some-request))
          (sorted-parameters some-request)
          )
    )
  (:method ((some-request sqs-request))
    (cons (cons "Signature" (request-signature some-request))
          (sorted-parameters some-request)
          )
    )
  )

(defgeneric additional-headers-of (some-request)
  (:documentation
    "Additional headers for the request; for s3, this includes the
     authorization header
    "
    )
  (:method ((some-request request))
    nil
    )
  (:method ((some-request s3-request))
    (let ( 
          (authorization-header (cons "Authorization" 
                                      (format-string "AWS ~a:~a"
                                                     (access-key-id *credentials*)
                                                     (request-signature some-request)
                                                     )
                                      )
                                )
          )
      (if (object-content-for some-request)
          (append (list
                   authorization-header
                   (cons "Content-MD5" 
                         (content-md5-of some-request)
                         )
                   )
                  (sorted-headers some-request)
                  )
          (cons authorization-header
                (sorted-headers some-request)
                )
          )
      )
    )
  )

(defclass response (message)
  (
   (body
    :initarg :body
    :initform nil
    :accessor response-body
    :documentation "Body of the response (if any)"
    )
   (status-code
    :initarg :status-code
    :accessor response-status-code
    :documentation "Status code returned by the server"
    )
   (headers
    :initarg :headers
    :accessor response-headers
    )
   ;; uri is already part of the message base class
   (stream
    :initarg :stream
    :accessor response-stream
    )
   (must-close
    :initarg :must-close
    :accessor response-must-close
    )
   (reason-phrase
    :initarg :reason-phrase
    :accessor response-reason-phrase
    )
   )
  (:documentation
    "Encapsulates a response from a service instance of AWS"
   )
  )

(defgeneric call-succeeded (some-response)
  (:documentation
    "Return true if the response indicates success, false otherwise
    ")
  (:method ((some-response response))
    (or
     (equal (response-status-code some-response) +http-ok+)
     (equal (response-status-code some-response) +http-no-content+)
     )
    )
  )

(defun make-response (
                      a-service 
                      body 
                      status-code 
                      headers 
                      uri 
                      stream 
                      must-close 
                      reason-phrase
                      )
  (make-instance 'response
                 :service a-service
                 :body body
                 :status-code status-code
                 :headers headers
                 :uri uri
                 :stream stream
                 :must-close must-close
                 :reason-phrase reason-phrase
                 )
  )

(defmacro response-values (some-response)
  "Expand a response into a multiple return values"
  `(values
    (response-body ,some-response) 
    (response-status-code ,some-response)
    (response-headers ,some-response)
    (uri-of ,some-response)
    (response-stream ,some-response)
    (response-must-close ,some-response)
    (response-reason-phrase ,some-response)
    )
  )

(defgeneric http-uri (some-request)
  (:documentation
    "The HTTP URI of a request, including scheme, host, and path
    "
    )
  
  (:method ((some-request request))
    (with-output-to-string (os)
      (format os "https://~a~a"
              (endpoint-of some-request)
              (uri-of some-request)
              )
      )
    )
  (:method ((some-request sqs-request))
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
  (:method ((some-request s3-request))
    (if (bucket-object-for some-request)
        (format-string "https://~a/~a"
                       (endpoint-of some-request)
                       (bucket-object-for some-request)
                       )
        (format-string "https://~a"
                       (endpoint-of some-request)
                       )
        )
    )
  )

(defgeneric send-http-request (some-request)
  (:documentation
    "Send the HTTP request
    "
    )
  (:method ((some-request request))
    (http-request (http-uri some-request)
                  :method (method-of some-request)
                  :parameters (signed-parameters-of some-request)
                  )
    )
  (:method ((some-request s3-request))
    (http-request (http-uri some-request)
                  :method (method-of some-request)
                  :parameters (signed-parameters-of some-request)
                  :additional-headers (additional-headers-of some-request)
                  )
    )
  )

(defgeneric send-request (some-service some-request &optional parameters)
  (:documentation
    "Send a request to its endpoint, and return any supplied
     response.
    "
    )
  (:method (
            (some-service service) 
            (some-request request) 
            &optional 
            (parameters nil)
            )
    (if parameters
        (dolist (pair parameters)
          (add-parameter some-request (car pair) (cdr pair))
          )
        )
    (multiple-value-bind 
      (body status-code headers uri stream must-close reason-phrase)
      (send-http-request some-request)
      (make-response some-service
                     body status-code headers uri stream must-close reason-phrase
                     )
      )
    )
  )

(defgeneric result-format (some-request)
  (:documentation
    "Return the format that should be used to extract the result
     from the response's body
    ")
  (:method ((some-request request))
    nil
    )
  )

(defgeneric extract-result (some-service some-request some-response)
  (:documentation
    "
    ")
  (:method (
            (some-service service) 
            (some-request db-request) 
            some-response
            ) 
    (let ( 
          (some-format (result-format some-request))
          )
      (if some-format
          (find-responses (response-body some-response) 
                    some-format
                    )
          (response-values some-response)
          )
      )
    )
  (:method (
            (some-service service) 
            (some-request sqs-request) 
            some-response
            ) 
    (let ( 
          (some-format (result-format some-request))
          )
      (if some-format
          (find-responses (response-body some-response) 
                    some-format
                    )
          (response-values some-response)
          )
      )
    )
  (:method (
            (some-service service) 
            (some-request s3-request) 
            some-response
            )
    (bytes-to-string (response-body some-response))
    )
  )

(defgeneric error-format (some-request)
  (:documentation 
    "
    ")
  (:method ((some-request request))
    '("Code" "Message")
    )
  )

(defgeneric extract-errors (some-service some-request some-response)
  (:documentation
    "
    ")
  (:method (
            (some-service service) 
            (some-request request) 
            some-response
            )
    (find-responses
     ; need bytes-to-string because there is a different encoding for errors 
     (bytes-to-string (response-body some-response))
     (error-format some-request)
     )
    )
  )

(defgeneric call-service (request &optional parameters arguments)
  (:documentation
    "
    ")
  (:method ((some-request request) &optional (parameters nil) (arguments nil))
    (declare (ignorable arguments))
    (let* (
           (the-service (service-of some-request)) 
           (the-response (send-request 
                          the-service 
                          some-request 
                          parameters
                          )
                         ) 
           )
      (if (call-succeeded the-response)
          (extract-result the-service some-request the-response)
          (destructuring-bind
            (code message)
            (car (extract-errors the-service some-request the-response))
            
            (raise-aws-condition code message 
                                 the-response
                                 )
            )
          )
      ) 
    )
  )

(defmacro defservice (
                      name 
                      &key 
                      (documentation "")
                      (bases nil) 
                      (slots nil) 
                      endpoint 
                      version
                      )
  
  `(progn
     
     (defclass ,name (,@bases service)
       (,@slots)
       (:documentation ,documentation)
       )
     
     (defmethod endpoint-of ((a-service ,name))
       ,@endpoint
       )
     
     (defmethod version-of ((a-service ,name))
       ,@version
       )
     
     )
  
  )

(defmacro defrequest (
                      name 
                      &key 
                      (documentation "")
                      (bases nil) 
                      (slots nil)
                      service
                      action
                      ((:method method-of-method) nil)
                      (parameters nil)
                      (args nil)
                      (result-format nil)
                      ((:result result-method) nil)
                      (error-format nil)
                      ((:error error-method) nil)
                      ((:call call-service-method) nil)
                      ((:send send-request-method) nil)
                      )
  
  (let ( 
        (request-class (use-symbol name :postfix "-REQUEST"))
        (parameter-vars (mapcar
                    (lambda (e) (cdr e)) 
                    parameters
                    )
                  )
        (parameter-list (mapcar
                    (lambda (e) 
                      `(cons ,(car e) ,(cdr e))
                      ) 
                    parameters
                    )
                  )
        )
    `(progn
       (defclass ,request-class (,@bases request)
         (,@slots)
         )
       
       ,(if action
            `(defmethod default-action ((some-request ,request-class))
               ,@action
               )
            )
       
       ,(if method-of-method
            `(defmethod method-of ((some-request ,request-class))
               ,@method-of-method 
               )
            )
       
       ,(if result-format
            `(defmethod result-format (( some-request ,request-class))
               ,@result-format
               )
            )
       
       ,(if result-method
            `(defmethod extract-result (
                                        (some-service service) 
                                        (some-request ,request-class) 
                                        some-response
                                        )
               ,@result-method
               )
            )
       
       ,(if error-format
            `(defmethod error-format (( some-request ,request-class))
               ,@error-format
               )
            )
       
       ,(if error-method
            `(defmethod extract-errors (
                                        (some-service service) 
                                        (some-request ,request-class) 
                                        some-response
                                        )
               ,@error-method
               )
            )
       
       ,(if call-service-method
            `(defmethod call-service ((some-request ,request-class) &optional (parameters nil) (arguments nil))
               (declare (ignorable parameters))
               (destructuring-bind (,@args)
                 arguments
                 ,@call-service-method
                 )
               )
            )
       
       ,(if send-request-method
            `(defmethod send-http-request ((some-request ,request-class))
               ,@send-request-method
               )
            )

       (defun ,name (,@parameter-vars ,@args)
         ,documentation
         (let* (
                (aws-service (make-instance (quote ,service))) 
                (aws-request 
                 (make-request aws-service (quote ,request-class))
                 ) 
                )
           (call-service aws-request (list ,@parameter-list) (list ,@args))
           )
         )       
       
       )
    )
  )