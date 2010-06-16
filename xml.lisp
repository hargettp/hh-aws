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

(defvar *current-handlers* (list t)
  "The list of active handlers for XML parsing, with the current
  as the head of the list"
  )

(defun current-handler ()
  (car *current-handlers*)
  )

(defun push-handler (handler)
  (push handler *current-handlers*)
  )

(defun pop-handler ()
  (pop *current-handlers*)
  )

(defgeneric enter-element (handler name attributes)
  (:method ((handler t) name attributes)
    t
    )
  )

(defgeneric capture-text (handler text-string)
  (:method ((handler t) text-string)
    t
    )
  )

(defgeneric exit-element (handler name attributes)
  (:method ((handler t) name attributes)
    t
    )
  )

(defgeneric finish-parse (handler)
  (:method ((handler t))
    t
    )
  )

(defmacro defxmlparser (
                        name 
                        specifier 
                        &key 
                        ((:initargs init-args) nil)
                        ((:enter enter-method) nil) 
                        ((:text text-method) nil) 
                        ((:exit exit-method) nil)
                        ((:finish finish-method nil))
                        )
  (declare (ignorable name))
  `(progn
     ,(if enter-method
          `(defmethod enter-element ((handler ,specifier) name attributes)
             ,@enter-method
             )
          )
     
     ,(if text-method
          `(defmethod capture-text ((handler ,specifier) text-string)
             ,@text-method
             )
          )
     
     ,(if exit-method
          `(defmethod exit-element ((handler ,specifier) name attributes)
             ,@exit-method
             )
          )
     
     ,(if finish-method
          `(defmethod finish-parse ((handler ,specifier))
             ,@finish-method
             )
          )
     
     (defun ,name (input-stream &rest more-args)
       (declare (ignorable more-args))
       (let* (
              (handler ,(if (listp specifier) 
                            (cadr specifier) ; (eql 'some-symbol)
                            `(make-instance (quote ,specifier) ,@init-args) ; specifier is a class
                            )
                       )
             (*current-handlers* (cons handler *current-handlers*))
             )
         
         (s-xml:start-parse-xml input-stream
                                (make-instance 's-xml:xml-parser-state
                                               :seed (cons 0 0)
                                               :new-element-hook #'on-start-element
                                               :finish-element-hook #'on-finish-element
                                               :text-hook #'on-text
                                               )
                                )
         ,(if finish-method
              `(finish-parse handler)
              )
         )
       )
     )
  )

(defvar *elements-to-find* nil
  "A list of strings, each naming an element whose text we're interested in"
  )

(defvar *current-elements-to-find* nil
  "The first element is the current element to find; the rest of
  the list is the elements remaining to be found"
  )

(defvar *current-elements* nil
  "A list of current elements; the top one on the list is the current
  element, and the prior one can be restored by setting this to the
  cdr of itself
  "
  )

(defvar *elements-found* nil
  "The text values for elements currently found"
  )

(defvar *finds* nil
  "If the list of elements-to-find cycles more than once, then each iteration
   is added to this list.  The final result will be the value of this variable.
  "
  )

(defun current-element-to-find ()
  (car *current-elements-to-find*)
  )

(defun current-element ()
  (car *current-elements*)
  )

(defxmlparser default-parser (eql ':default)
  :enter (
          (push (symbol-name name) *current-elements*)
          )
  :text (
         (if (equal (current-element) (current-element-to-find))
             (progn
               (setf *elements-found*
                     (append *elements-found*
                             (list text-string)       
                             )
                     )
               (move-to-next-element-to-find)
               )
             )
         )
  :exit (
         (pop *current-elements*)
         )
  )

(defun move-to-next-element-to-find ()
  (pop *current-elements-to-find*)
  (if (not *current-elements-to-find*)
      (progn
        (setf *current-elements-to-find* *elements-to-find*)
        (setf *finds*
              (append *finds*
                      (list *elements-found*)
                      )
              )
        (setf *elements-found* nil)
        )
      )
  )

(defun on-start-element (name attributes seed)
  (let (
        (new-seed (cons (1+ (car seed)) (1+ (cdr seed))))
        )
    (enter-element (current-handler) name attributes)
    new-seed
    )
  )

(defun on-finish-element (name attributes parent-seed seed)
  (declare (ignorable parent-seed))
  (let (
        (new-seed (cons (1- (car seed)) (1+ (cdr seed))))
        )
    (exit-element (current-handler) name attributes)
    new-seed
    )
  )

(defun on-text (text-string seed)
  (let (
        (new-seed (cons (car seed) (1+ (cdr seed))))
        )
    (capture-text (current-handler) text-string)
    new-seed
    )  
  )

(defun find-responses (body elements-to-find)
  (let* 
    ( 
     (*elements-to-find* elements-to-find)
     (*current-elements-to-find* *elements-to-find*)
     (*current-elements* nil)
     (*elements-found* nil)
     (*finds* nil)
     )
    
    (with-input-from-string (is body)
      (default-parser is)
      )
    (if *elements-found*
        (append *finds* (list *elements-found*))
        *finds*
        )
    )
  )

(defun path-p (some-path)
  (if (<= (length some-path) (length *current-elements*))
      (equal (subseq *current-elements* 0 (length some-path))
             some-path
             )
      nil
      )
  )

(defclass builder ()
  (
   (all-results
    :initform nil
    :accessor results-of)
   (current-result
    :initform nil
    :accessor current-of
    )
   )
  )

(defxmlparser builder-parser builder
  :enter (
          (push (symbol-name name) *current-elements*)
          )
  :text (
         (call-next-method)
         )
  :exit (
         (pop *current-elements*)
         )
  )

(defclass find-path-builder (builder)
  (
   (path
    :initform nil
    :initarg :path
    :accessor path-to-match
    )
   )
  )

(defxmlparser find-path-parser find-path-builder
  :text (
         (if (path-p (path-to-match handler))
             (progn
               (putend text-string (results-of handler) )
               )
             )
         (call-next-method)
         )
  :finish (
           (car (results-of handler))
           )
  )