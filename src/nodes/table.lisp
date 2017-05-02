;;; src/nodes/table.lisp

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


(defpackage :pgsnipe/nodes/table
  (:use :common-lisp :pgsnipe/nodes/base :pgsnipe/lexical)
  (:export #:table
           #:table-schema-name
           #:table-name
           #:table-columns
           #:table-primary-key
           #:table-check-constraints
           #:copy-table))

(in-package :pgsnipe/nodes/table)

(defclass table (node)
  ((schema
    :initarg :schema-name
    :reader table-schema-name)
   (name
    :initarg :name
    :reader table-name)
   (columns
    :initform nil
    :accessor table-columns)
   (primary-key
    :accessor table-primary-key)
   (check-constraints
    :initform nil
    :accessor table-check-constraints)))


(defmethod sql-qualified-name ((tbl table))
  (sql-qualified-identifier (table-schema-name tbl) (table-name tbl)))


(defmethod print-object ((x table) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (sql-qualified-name x) stream)))


(defun copy-table (table &key
                           (schema (table-schema-name table))
                           (name (table-name table)))
  (let ((new (make-instance 'table :schema-name schema :name name :oid nil :classid nil)))
    (setf (table-columns new) (table-columns table)
          (table-primary-key new) (table-primary-key table)
          (table-check-constraints new) (table-check-constraints new))
    new))
