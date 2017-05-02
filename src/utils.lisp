;;; src/utils.lisp ---

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

(defpackage :pgsnipe/utils
  (:use :common-lisp)
  (:export #:not-implemented
           #:yes-p
           #:no-p
           #:get-username
           #:with-plist
           #:pick))

(in-package :pgsnipe/utils)


(defun not-implemented (name)
  (error "The function ~A is not implemented in ~A." name (lisp-implementation-type)))

(defun yes-p (x)
  "Check if X is equal to YES. Throw an error if it is not YES/NO."
  (cond
    ((equal x "YES") t)
    ((equal x "NO") nil)
    (t
     (error "~S is not YES or NO." x))))

(defun no-p (x)
  "Check if X is NO. Throw an error if it is not YES/NO."
  (not (yes-p x)))


(defun pick (plist fields)
  (let ((out nil))
    (dolist (field fields out)
      (setf (getf out field)
            (getf plist field)))))


(defmacro with-plist ((&rest fields) plist &body body)
  `(destructuring-bind (&key ,@fields &allow-other-keys) ,plist
     ,@body))


;;; Operating system

(defun get-username ()
  #-sbcl (not-implemented 'get-username)
  (let* ((uid (sb-posix:getuid))
         (pw (sb-posix:getpwuid uid)))
    (sb-posix:passwd-name pw)))
