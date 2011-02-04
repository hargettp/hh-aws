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

;;; TODO Most of the functions in this file were written before the author's knowledge
;;; of Lisp had begun to mature; thus, most if not call should either be rewritten
;;; or replaced as time allows

;; Symbols

(defun use-symbol 
  (
   base-name
   
   &key 
   (package nil)
   (prefix "")
   (postfix "")                    
   )
  "Build a symbol from component parts"
  (intern (concatenate 
            'string
            prefix
            (if (symbolp base-name) (symbol-name base-name) base-name)
            postfix
            ) 
           (if package package *package*)
           )
  )

;; collections

(defun detokenize (elements token &optional (prefix ""))
  "Combine elements into a string, separating each element by token"
  (let ( 
        (car-element (car elements))
        (cdr-elements (cdr elements))
        )
    (if cdr-elements
        (concatenate 'string car-element token
                     (detokenize 
                      cdr-elements 
                      token
                      prefix 
                      )
                     )
        car-element
        )
    )
  )

(defmacro putend (some-object some-place)
  "Append some-object to the end of the list in some-place.
   Modifies some-place.
   "
  `(setf ,some-place
         (nconc ,some-place
                 (list ,some-object)
                 )
         )
  )

;; Hashtables

(defun hash-entries (some-hash)
  (let ( 
        (entries nil) 
        )
    (maphash (lambda (k v)
               (setq entries
                     (cons (cons k v) entries)
                     )
               )
             some-hash
             )
    entries
    )
  )

(defun sort-hashtable (some-hash predicate)
  "Use predicate to compare pairs of keys & values; predicate
   should return true iff pair 1 is less than pair 2 (each pair
   is a cons of (key.value) ). 
  "
  (sort (hash-entries some-hash) predicate
        )
  )

(defun map-name-value-pairs (name-value-pairs some-lambda)
  "Apply some lambda (takes 2 arguments) to each name / value
   pair in name-value-pairs
  "
  (if name-value-pairs
      (let (
            (name (car (car name-value-pairs)))
            (value (cdr (car name-value-pairs)))
            (remaining (cdr name-value-pairs))
            )
        (cons
         (apply some-lambda name (list value))
         (map-name-value-pairs remaining some-lambda)
         )
        )
      nil
      )
  )

;; Strings

(defmacro format-string (format-control-string &rest args)
  `(with-output-to-string (os)
     (format os ,format-control-string ,@args)
     )
  )

(defun string-to-bytes (a-string &key (pad-to 1))
  "Borrowed from Arnesi (http://common-lisp.net/project/bese/arnesi.html)"
  (let ( 
        (padded-length (* pad-to (ceiling (length a-string) pad-to)))
        )
    (map-into (make-array padded-length :element-type '(unsigned-byte 8))
              #'char-code 
              a-string
              )
    )
  )
  
(defun bytes-to-string (some-bytes)
  "Borrowed from Arnesi (http://common-lisp.net/project/bese/arnesi.html)"
  (map-into (make-array (length some-bytes) :element-type 'character)
            #'code-char 
            some-bytes
            )
  )

(defun string-starts-with (target-string search-string)
  "
  Return T if target-string starts with search-string, 
  nil otherwise 
  "
  (if (> (length search-string) (length target-string))
      nil
      (equal (subseq target-string 0 (length search-string)) 
             search-string
             )    
      )
  )

(defun string-ends-with (target-string search-string)
  "
  Return T if target-string ends with search-string, 
  nil otherwise 
  "
  (let ( (index (- (length target-string) (length search-string))) )
    (if (>= index 0)
        ;; test to see if end of target-string is search-string 
        (equal (subseq target-string index) search-string)
        ;; return nil, because search string too long
        nil
        ) 
    )
  )

(defun ensure-string-ends-with (target-string string-ending)
  (if (string-ends-with target-string string-ending)
      ;; already does
      target-string
      ;; concatenate the ending
      (concatenate 'string target-string string-ending)
      )
  )

;; Filesystem helpers

(defun home-dir ()
  "Return the user's home directory
  "
  (ensure-string-ends-with (sb-ext:posix-getenv "HOME") "/")
  )

;; Console helpers
(defmacro cout (&rest args)
  `(format *standard-output* ,@args)
  )