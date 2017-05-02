;;; src/nodes/constraint.lisp

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

(defpackage :pgsnipe/nodes/constraint
  (:use :common-lisp :pgsnipe/nodes/base)
  (:export #:constraint
           #:constraint-schema-name
           #:constraint-name
           #:constraint-is-deferrable
           #:constraint-initially-deferred

           #:check-constraint
           #:check-constraint-clause

           #:primary-key-constraint
           #:primary-key-columns))

(in-package :pgsnipe/nodes/constraint)

(defclass constraint (node)
  ((schema-name
    :initarg :schema-name
    :reader constraint-schema-name)
   (name
    :initarg :name
    :reader constraint-name)
   (is-deferrable
    :initarg :is-deferrable
    :reader constraint-is-deferrable)
   (initially-deferred
    :initarg :initially-deferred
    :reader constraint-initially-deferred)))

(defclass check-constraint (constraint)
  ((clause
    :initarg :check-clause
    :reader check-constraint-clause)))

(defclass primary-key-constraint (constraint)
  ((columns
    :accessor primary-key-columns)))
