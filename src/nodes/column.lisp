;;; src/nodes/column.lisp

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

(defpackage :pgsnipe/nodes/column
  (:use :common-lisp :pgsnipe/nodes/base)
  (:export #:column
           #:column-name
           #:column-default
           #:column-table
           #:column-default-value
           #:column-default-column
           #:column-nullable-p
           #:column-data-type))

(in-package :pgsnipe/nodes/column)


(defclass column ()
  ((table
    :initarg :table
    :reader column-table)
   (name
    :initarg :name
    :reader column-name)
   (default
    :initarg :default
    :type default-value
    :accessor column-default)
   (is-nullable
    :initarg :is-nullable
    :reader column-nullable-p)
   (data-type
    :initarg :data-type
    :reader column-data-type)))

(defmethod column-default-value ((c column))
  (let ((def (column-default c)))
    (column-default-value def)))

(defclass column-default (node)
  ((column
    :initarg :column
    :reader column-default-column)
   (value
    :initarg :value
    :reader column-default-value)))



(defmethod print-object ((x column) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (column-name x) stream)))
