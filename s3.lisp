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

(defgeneric content-md5-of (some-request)
  )

(defgeneric content-type-of (some-request)
  )

(defgeneric content-length-of (some-request)
  )

(defgeneric canonicalized-amz-headers-of (some-request)
  (:documentation
    "Return a canonical string representation of amz headers
    "
    )
  )

(defgeneric canonicalized-resource-of (some-request)
  )

(defservice s3
  :endpoint ( 
             (string "s3.amazonaws.com") 
             )
  :version ( 
            (string "2006-03-01") 
            )
  :request (
	    s3-request
	    :slots (
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
	    :documentation "Base for requests targeting S3"
	    :init (progn
		    (add-header some-request "Date" (aws-date))
		    )
	    :endpoint (progn
			(if (bucket-for some-request)
			    (format-string "~a.~a"
					   (bucket-for some-request)
					   (call-next-method) 
					   )
			    (call-next-method)
			    )  
			)
	    :uri (progn
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
	    :string (progn
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
	    :digest ironclad:sha1
	    :additional-headers (progn
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
	    :send (progn
		    (http-request (http-uri some-request)
				  :method (method-of some-request)
				  :parameters (signed-parameters-of some-request)
				  :additional-headers (additional-headers-of some-request)
				  )
		    )
	    :result (progn
		       (bytes-to-string (response-body some-response))
		       )
	    )
  )

(defmethod content-md5-of ((some-request s3-request))
  (if (object-content-for some-request)
      (md5-digest (object-content-for some-request))
      (string "")
      )
  )

(defmethod content-type-of ((some-request s3-request))
    (if (object-content-for some-request)
        (string "text/plain")
        (string "")
        )
    )

(defmethod content-length-of ((some-request s3-request))
    (if (object-content-for some-request)
        (length (object-content-for some-request))
        0
        )
    )

(defmethod canonicalized-amz-headers-of ((some-request s3-request))
    (with-output-to-string (os)
      (dolist (header (sorted-headers some-request))
        (if (string-starts-with (car header) "x-amz")
            (format os "~a: ~a\n" (car header) (cdr header))
            )
        )
      )
    )

(defmethod canonicalized-resource-of ((some-request s3-request))
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

(defclass list-buckets-builder (builder)
  ()
  )

(defxmlparser list-buckets-parser list-buckets-builder
  :enter (
          (push (symbol-name name) *current-elements*)
          )
  :text (
         (if (path-p '("Name" "Bucket"))
             (progn
               (putend text-string (current-of handler) )
               )
             )
         (call-next-method)
         )
  :exit (
         (progn
           (if (current-of handler)
               (progn
                 (putend (current-of handler) (results-of handler) )
                 )
               )
           (setf (current-of handler) nil)
           )
         (pop *current-elements*)
         )
  :finish (
           (mapcar #'car
                   (results-of handler)
                   )
           )
  )

(defrequest s3-list-buckets
  :documentation "Return list of all buckets in S3"
  :bases (s3-request)
  :service s3    
  :result (
           (with-input-from-string 
               (is (bytes-to-string (response-body some-response)))
             (list-buckets-parser is)
             )
           )
  )

(defrequest s3-create-bucket
  :documentation "Create a new bucket"
  :bases (s3-request)
  :service s3
  :method (
           :put
           )
  :args (
         bucket-name
         )
  :call (
         (setf (bucket-for some-request) bucket-name)
         (add-header some-request "Content-Length" 0)
         (call-next-method)
         )
  :result (
           t
           )
  )

(defrequest s3-delete-bucket
  :documentation "Delete a bucket"
  :bases (s3-request)
  :service s3
  :method (
           :delete
           )
  :args (
         bucket-name
         )
  :call (
         (setf (bucket-for some-request) bucket-name)
         (call-next-method)
         )
  :result (
           t
           )
  )

(defclass bucket-contents-builder (builder)
  (
   (all-attributes
    :initform nil
    :accessor attributes-of
    )
   (current-attribute
    :initform nil
    :accessor current-attribute-of
    )
   )
  )

(defxmlparser bucket-contents-parser bucket-contents-builder
  :enter (
	  (push (symbol-name name) *current-elements*)
          )
  :text (
         (if (path-p '("Key" "Contents"))
             (progn
               (putend text-string (current-of handler) )
               )
             )
         (if (path-p '("LastModified" "Contents"))
             (progn
               (putend text-string (current-of handler) )
               )
             )
         (if (path-p '("ETag" "Contents"))
             (progn
               (putend text-string (current-of handler) )
               )
             )
         (if (path-p '("Size" "Contents"))
             (progn
               (putend text-string (current-of handler) )
               )
             )
         (if (path-p '("ID" "Owner" "Contents"))
             (progn
               (putend text-string (current-of handler) )
               )
             )
         (if (path-p '("DisplayName" "Owner" "Contents"))
             (progn
               (putend text-string (current-of handler) )
               )
             )
         (call-next-method)
         )
  :exit (
         (if (path-p '("Contents"))
             (progn
               (putend (current-of handler) (results-of handler) )
               (setf (current-of handler) nil)
               )
             )
	 (pop *current-elements*)
         )
  :finish (
           (results-of handler)
           )
  )

(defrequest s3-list-bucket-objects
  :documentation "List objects in a bucket"
  :bases (s3-request)
  :service s3
  :method (
           :get
           )
  :parameters (
               ("prefix" . prefix)
               )
  :args (
         bucket-name
         )
  :call (
         (setf (bucket-for some-request) bucket-name)
         (call-next-method)
         )
  :result (
           (with-input-from-string 
               (is (bytes-to-string (response-body some-response)))
             (bucket-contents-parser is)
             )
           )
  )

(defrequest s3-put-bucket-object
  :documentation "Either create a new bucket object for the content,
                  or update an existing one.
                 "
  :bases (s3-request)
  :service s3
  :method (
           :put
           )
  :args (
         bucket-name
         object-name
         content
         )
  :send (
         (http-request (http-uri some-request)
                       :method (method-of some-request)
                       :parameters (signed-parameters-of some-request)
                       :additional-headers (additional-headers-of some-request)
                       :content-type (content-type-of some-request)
                       :content-length (content-length-of some-request)
                       :content (object-content-for some-request)
                       )
         )
  :call (
         (setf (bucket-for some-request) bucket-name)
         (setf (bucket-object-for some-request) object-name)
         (setf (object-content-for some-request) content)
         
         (handler-bind 
          (
           (aws-error #'(lambda (e)
                          (cout "Response is ~a~%"
                                (bytes-to-string 
                                 (response-body (error-response e))
                                 )
                                )
                          ) 
                      )
           )
          (call-next-method)
          )
         )
  :result (
           t
           )
  )

(defrequest s3-get-bucket-object
  :documentation "Return the contents of the indicated bucket
                 "
  :bases (s3-request)
  :service s3
  :method (
           :get
           )
  :args (
         bucket-name
         object-name
         )
  :call (
         (setf (bucket-for some-request) bucket-name)
         (setf (bucket-object-for some-request) object-name)
         
	 (call-next-method)
         )
  :result (
           (response-body some-response)
           )
  )

(defrequest s3-delete-bucket-object
  :documentation "Return the contents of the indicated bucket
                 "
  :bases (s3-request)
  :service s3
  :method (
           :delete
           )
  :args (
         bucket-name
         object-name
         )
  :call (
         (setf (bucket-for some-request) bucket-name)
         (setf (bucket-object-for some-request) object-name)
         
          (call-next-method)
         )
  :result (
           t
           )
  )