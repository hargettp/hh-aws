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

;; borrowed from Hunchentoot (BSD license)

(eval-when (:compile-toplevel :execute :load-toplevel)
  
  (defvar *http-reason-phrase-map* (make-hash-table)
    "Used to map numerical return codes to reason phrases.")
  )
(defmacro def-http-return-code (name value reason-phrase)
  "Shortcut to define constants for return codes.  NAME is a
Lisp symbol, VALUE is the numerical value of the return code, and
REASON-PHRASE is the phrase \(a string) to be shown in the
server's status line."
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (defconstant ,name ,value ,(format nil "HTTP return code \(~A) for '~A'."
                                        value reason-phrase))
     (setf (gethash ,value *http-reason-phrase-map*) ,reason-phrase))
  )

(def-http-return-code +http-continue+ 100 "Continue")
(def-http-return-code +http-switching-protocols+ 101 "Switching Protocols")
(def-http-return-code +http-ok+ 200 "OK")
(def-http-return-code +http-created+ 201 "Created")
(def-http-return-code +http-accepted+ 202 "Accepted")
(def-http-return-code +http-non-authoritative-information+ 203 "Non-Authoritative Information")
(def-http-return-code +http-no-content+ 204 "No Content")
(def-http-return-code +http-reset-content+ 205 "Reset Content")
(def-http-return-code +http-partial-content+ 206 "Partial Content")
(def-http-return-code +http-multi-status+ 207 "Multi-Status")
(def-http-return-code +http-multiple-choices+ 300 "Multiple Choices")
(def-http-return-code +http-moved-permanently+ 301 "Moved Permanently")
(def-http-return-code +http-moved-temporarily+ 302 "Moved Temporarily")
(def-http-return-code +http-see-other+ 303 "See Other")
(def-http-return-code +http-not-modified+ 304 "Not Modified")
(def-http-return-code +http-use-proxy+ 305 "Use Proxy")
(def-http-return-code +http-temporary-redirect+ 307 "Temporary Redirect")
(def-http-return-code +http-bad-request+ 400 "Bad Request")
(def-http-return-code +http-authorization-required+ 401 "Authorization Required")
(def-http-return-code +http-payment-required+ 402  "Payment Required")
(def-http-return-code +http-forbidden+ 403 "Forbidden")
(def-http-return-code +http-not-found+ 404 "Not Found")
(def-http-return-code +http-method-not-allowed+ 405 "Method Not Allowed")
(def-http-return-code +http-not-acceptable+ 406 "Not Acceptable")
(def-http-return-code +http-proxy-authentication-required+ 407 "Proxy Authentication Required")
(def-http-return-code +http-request-time-out+ 408 "Request Time-out")
(def-http-return-code +http-conflict+ 409 "Conflict")
(def-http-return-code +http-gone+ 410 "Gone")
(def-http-return-code +http-length-required+ 411 "Length Required")
(def-http-return-code +http-precondition-failed+ 412 "Precondition Failed")
(def-http-return-code +http-request-entity-too-large+ 413 "Request Entity Too Large")
(def-http-return-code +http-request-uri-too-large+ 414 "Request-URI Too Large")
(def-http-return-code +http-unsupported-media-type+ 415 "Unsupported Media Type")
(def-http-return-code +http-requested-range-not-satisfiable+ 416 "Requested range not satisfiable")
(def-http-return-code +http-expectation-failed+ 417 "Expectation Failed")
(def-http-return-code +http-failed-dependency+ 424 "Failed Dependency")
(def-http-return-code +http-internal-server-error+ 500 "Internal Server Error")
(def-http-return-code +http-not-implemented+ 501 "Not Implemented")
(def-http-return-code +http-bad-gateway+ 502 "Bad Gateway")
(def-http-return-code +http-service-unavailable+ 503 "Service Unavailable")
(def-http-return-code +http-gateway-time-out+ 504 "Gateway Time-out")
(def-http-return-code +http-version-not-supported+ 505 "Version not supported")
