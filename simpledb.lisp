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

(export
 (list
  
  'db-list-domains
  'db-create-domain
  'db-delete-domain
  'db-domain-metadata
  
  'db-put-attributes
  'db-get-attributes
  'db-delete-attributes
  
  'db-query
  'db-query-with-attributes
  'db-select
  
  )
 )

(defgeneric add-attribute-parameters (some-request attributes)
  (:documentation 
    "Add the indicated parameters as attribute name / value
     pairs to the request
    "
    )
  (:method (some-request attributes)
    (let ( (index 0) )
      (dolist (attribute attributes)
        (let ( 
              (attribute-name-parameter 
               (format-string "Attribute.~a.Name" index)
               )
              (attribute-value-parameter 
               (format-string "Attribute.~a.Value" index)
               )
              (attribute-replace-parameter
               (format-string "Attribute.~a.Replace" index)
               )
              )
          (add-parameter some-request 
                         attribute-name-parameter 
                         (car attribute)
                         )
          (add-parameter some-request 
                         attribute-value-parameter 
                         (cadr attribute)
                         )
          (if (caddr attribute)
              (add-parameter some-request 
                         attribute-replace-parameter 
                         "true"
                         )
              )
          )
        (incf index)
        )
      )
    )
  )

(defgeneric add-attribute-name-parameters (some-request attributes)
  (:documentation 
    "Add the indicated parameters as attribute name / value
     pairs to the request
    "
    )
  (:method (some-request attributes)
    (let ( (index 0) )
      (dolist (attribute attributes)
        (let ( 
              (attribute-name-parameter 
               (with-output-to-string (os)
                 (format os "AttributeName.~a" index)
                 )
               )
              )
          (add-parameter some-request 
                         attribute-name-parameter 
                         attribute
                         )
          )
        (incf index)
        )
      )
    )
  )

(defservice simpledb
  :endpoint ( 
             (string "sdb.amazonaws.com") 
             )
  :version ( 
            (string "2007-11-07") 
            )
  )

(defrequest db-list-domains
  :documentation "List all domains"
  :bases (db-request)
  :service simpledb
  :action ( 
           (string "ListDomains")
           )
  :result-format (
                  '("DomainName")
                  )
  :result (
           (mapcar (lambda (e) 
                     (car e) 
                     )
                   (call-next-method)
                   )
           )
  )

(defrequest db-create-domain
  :documentation "Create a domain with the specified name"
  :bases (db-request)
  :service simpledb
  :action (
           (string "CreateDomain")
           )
  :parameters (
               ("DomainName" . domain-name)      
               )
  :result (
           t
           )
  )

(defrequest db-delete-domain
  :documentation "Delete the specified domain"
  :bases (db-request)
  :service simpledb
  :action (
           (string "DeleteDomain")
           )
  :parameters ( 
               ("DomainName" . domain-name)
               )
  :result (
           t
           )
  )

(defrequest db-domain-metadata
  :documentation "Return metadata about the specified domain"
  :bases (db-request)
  :service simpledb
  :action (
           (string "DomainMetadata")
           )
  :parameters ( 
         ("DomainName" . domain-name)
         )
  :result-format (
                  '(
                    "ItemCount"	 
                    "ItemNamesSizeBytes"
                    "AttributeNameCount"
                    "AttributeNamesSizeBytes"
                    "AttributeValueCount"
                    "AttributeValuesSizeBytes"
                    "Timestamp"
                    )
                  )
  
  )

(defrequest db-get-attributes
  :documentation "Return the attributes for the specified item in the domain"
  :bases (db-request)
  :service simpledb
  :action (
           (string "GetAttributes")
           )
  :parameters (
               ("DomainName" . domain-name)
               ("ItemName" . item-name)
               )
  :result-format (
                  '("Name" "Value")
                  )
  )

(defrequest db-put-attributes
  :documentation "Set the attributes on the specified item in the domain"
  :bases (db-request)
  :service simpledb
  :action (
           (string "PutAttributes")
           )
  :parameters (
               ("DomainName" . domain-name)
               ("ItemName" . item-name)
               )
  :args (
         attributes
         )
  :call (
         (add-attribute-parameters some-request attributes)
         (call-next-method)
         )
  :result (
           t
           )
  )

(defrequest db-delete-attributes
  :documentation "Delete attributes from the specified item, and if all
                 attributes are gone, delete the item itself"
  :bases (db-request)
  :service simpledb
  :action (
           (string "DeleteAttributes")
           )
  :parameters (
               ("DomainName" . domain-name)
               ("ItemName" . item-name)
               )
  :args (
         attributes
         )
  :call (
         (add-attribute-name-parameters some-request attributes)
         (call-next-method)
         )
  :result (
           t
           )
  )

(defrequest db-query
  :documentation "Return a list of item names matching the query expression
                 "
  :bases (db-request)
  :service simpledb
  :action (
           (string "Query")
           ) 
  :parameters (
               ("DomainName" . domain-name)
               ("QueryExpression" . query-expression)
               )  
  :result-format (
                  `(
                    "ItemName"
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

(defclass items-builder (builder)
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

(defxmlparser items-parser items-builder
  :enter (
          (call-next-method)
          )
  :text (
         (if (path-p '("Name" "Item"))
             (progn
	       (putend text-string (current-of handler) )
               )
             )
         (if (path-p '("Name" "Attribute"))
             (progn
	       (putend text-string (current-attribute-of handler) )
               )
             )
         (if (path-p '("Value" "Attribute"))
             (progn
               (putend text-string (current-attribute-of handler)  )
               )
             )
         (call-next-method)
         )
  :exit (
         (if (path-p '("Attribute"))
             (progn
               (putend  (current-attribute-of handler)
			  (attributes-of handler) 
			  )
               (setf (current-attribute-of handler) nil)
               )
             )
         (if (path-p '("Item"))
             (progn
               (setf (current-of handler)
                     (append (current-of handler)
                             (attributes-of handler)            
                             )
                     )
               (putend (current-of handler) (results-of handler) )
               (setf (current-attribute-of handler) nil)
               (setf (attributes-of handler) nil)
               (setf (current-of handler) nil)
               )
             )
         (call-next-method)
         )
  :finish (
           (results-of handler)
           )
  )

(defrequest db-query-with-attributes
  
  :documentation "Return the indicated attributes of items 
                  matching the query expression
                 "
  :bases (db-request)
  :service simpledb
  :action (
           (string "QueryWithAttributes")
           ) 
  :parameters (
               ("DomainName" . domain-name)
               ("QueryExpression" . query-expression)
               )  
  :args (
         attributes
         )
  :call (
         (add-attribute-name-parameters some-request attributes)
         (call-next-method)
         )
  :result (
           (with-input-from-string (is (response-body some-response))
             (items-parser is)
             )
           )
  )

(defrequest db-select
  
  :documentation "Return the results of the provided select statement
                 "
  :bases (db-request)
  :service simpledb
  :action (
           (string "Select")
           )
  :parameters (
               ("SelectExpression" . select-expression)
               )
  :result (
           (with-input-from-string (is (response-body some-response))
             (items-parser is)
             )
           )
  )