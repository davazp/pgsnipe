;;; src/nodes/sequence.lisp

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

(defpackage :pgsnipe/nodes/sequence
  (:use :common-lisp
        :pgsnipe/nodes/base
        :pgsnipe/lexical)
  (:export #:sequence*
           #:sequence-schema-name
           #:sequence-name
           #:sequence-start-value
           #:sequence-minimum-value
           #:sequence-maximum-value
           #:sequence-increment
           #:sequence-cycle-option))

(in-package :pgsnipe/nodes/sequence)


(defclass sequence* (node)
  ((schema-name
    :initarg :schema-name
    :type string
    :initform *default-schema*
    :reader sequence-schema-name)
   (name
    :initarg :name
    :type string
    :reader sequence-name)
   (start-value
    :initarg :start-value
    :type integer
    :reader sequence-start-value)
   (minimum-value
    :initarg :minimum-value
    :type integer
    :reader sequence-minimum-value)
   (maximum-value
    :initarg :maximum-value
    :type integer
    :reader sequence-maximum-value)
   (increment
    :initarg :increment
    :type integer
    :reader sequence-increment)
   (cycle-option
    :initarg :cycle-option
    :type boolean
    :reader sequence-cycle-option)))


(defmethod sql-qualified-name ((seq sequence*))
  (sql-qualified-identifier (sequence-schema-name seq)
                            (sequence-name seq)))


(defmethod print-object ((x sequence) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (sql-qualified-name x) stream)))
