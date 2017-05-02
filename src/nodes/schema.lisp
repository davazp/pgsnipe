;;; src/nodes/schema.lisp

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

(defpackage :pgsnipe/nodes/schema
  (:use :common-lisp :pgsnipe/nodes/base :pgsnipe/lexical)
  (:export #:schema
           #:schema-name
           #:schema-owner))

(in-package :pgsnipe/nodes/schema)

(defclass schema (node)
  ((name
    :initarg :name
    :reader schema-name)
   (owner
    :initarg :owner
    :reader schema-owner)))

(defmethod print-object ((x schema) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (schema-name x) stream)))

(defmethod sql-qualified-name ((x schema))
  (sql-identifier (schema-name x)))
