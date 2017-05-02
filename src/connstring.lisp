;;; src/connstring.lisp ---

;; Copyright (c) 2017 David Vázquez Púa

;; This file is part of pgsnipe.

;; pgsnipe is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; pgsnipe is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with pgsnipe.  If not, see <http://www.gnu.org/licenses/>.


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




