(defpackage :pgsnipe/connstring
  (:use :common-lisp :quri :pgsnipe/utils)
  (:export #:parse))

(in-package :pgsnipe/connstring)

(defun parse-userinfo (userinfo)
  (let ((colon (position #\: userinfo)))
    (if colon
        (list (subseq userinfo 0 colon)
              (subseq userinfo (1+ colon)))
        (list userinfo))))

;;; Parse a connection string based on the one described here:
;;;
;;; https://www.postgresql.org/docs/current/static/libpq-connect.html
;;;
(defun parse (connstring)
  "Parse a connection string URL and return an plist with the
attributes to pass to postmodern to establish the connection."
  (let ((uri (uri connstring)))
    (unless (find (uri-scheme uri) #("postgresql" "postgres") :test #'string=)
      (error "The URI must start with the schema postgresql://"))
    (destructuring-bind (user &optional password)
        (parse-userinfo (uri-userinfo uri))
      (let ((params (uri-query-params uri)))
        (flet ((get-param (name)
                 (cdr (find name params :test #'string= :key #'car))))
          (list
           ;; Database
           (if (uri-path uri)
               (subseq (uri-path uri) 1)
               (get-username))
           ;; User
           (or user
               (get-param "user")
               (get-username))
           ;; Password
           password
           ;; Host
           (or (uri-host uri) (get-param "host") :unix)
           ;; Port
           :port (or (uri-port uri) (get-param "port") 5432)))))))




