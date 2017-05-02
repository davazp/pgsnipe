;;; src/execute.lisp

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


(defpackage :pgsnipe/execute
  (:use :common-lisp :pgsnipe/connstring :postmodern)
  (:export #:execute-file
           #:execute-string
           #:execute-stream))

(in-package :pgsnipe/execute)

(defvar *stream*)
(defvar *buffer*)

(defun next-char ()
  (let ((ch (read-char *stream* nil)))
    (when ch
      (write-char ch *buffer*))
    ch))

(defun read-to (char)
  (with-output-to-string (out)
    (loop
       for ch = (next-char)
       while (char/= ch char)
       do (write-char ch out))))



(defun consume-simple-string ()
  (read-to #\'))

(defun consume-quoted-identifier ()
  (read-to #\"))

(defun consume-dollar-quoted-string ()
  (let ((tag (read-to #\$)))
    (print (list 'start tag))
    (loop
       for ch = (next-char)
       when (char= ch #\$)
       do (progn
            (let ((matched-end
                   (loop
                      for i from 0
                      for c = (peek-char nil *stream*)
                      when (= i (length tag)) do (return t)
                      when (char/= c (char tag i)) do (return)
                      do (next-char))))
              (when (and matched-end (char= (peek-char nil *stream*) #\$))
                (next-char)
                (return)))))))


(defun consume-comment ()
  (read-to #\newline))


(defun consume-statement (&key recursive)
  (loop
     (let ((ch (next-char)))
       (cond
         ((null ch)
          (return))
         ((char= ch #\')
          (consume-simple-string))
         ((char= ch #\$)
          (consume-dollar-quoted-string))
         ((char= ch #\")
          (consume-quoted-identifier))
         ((char= ch #\-)
          (let ((ahead (peek-char nil *stream*)))
            (when (char= ahead #\-)
              (consume-comment))))
         ((char= ch #\()
          (consume-statement :recursive t))
         ((and (char= ch #\)) recursive)
          (return))
         ((and (char= ch #\;) (not recursive))
          (return))))))


(defun parse-statement (&optional (*stream* *stream*))
  (unless (null (peek-char nil *stream* nil))
    (with-output-to-string (*buffer*)
      (block statement-found
        (consume-statement)))))


(defun execute-stream (connstring stream)
  (with-connection (parse connstring)
    (loop
       for stmt = (parse-statement stream)
       while stmt
       do (with-simple-restart (continue "Continue")
            (handler-bind ((warning #'muffle-warning))
              (postmodern:execute stmt))))))


(defun execute-string (connstring string)
  (with-input-from-string (in string)
    (execute-stream connstring in)))

(defun execute-file (connstring file)
  (with-open-file (in file)
    (execute-stream connstring in)))
