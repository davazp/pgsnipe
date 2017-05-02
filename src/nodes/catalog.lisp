;;; src/nodes/catalog.lisp

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

(defpackage :pgsnipe/nodes/catalog
  (:use :common-lisp :pgsnipe/nodes/base)
  (:export #:catalog
           #:catalog-name
           #:catalog-schemata
           #:catalog-sequences
           #:catalog-tables

           #:register-dependency
           #:dependencies
           #:dependants))

(in-package :pgsnipe/nodes/catalog)

(defclass catalog ()
  ((name
    :initarg :name
    :reader catalog-name)
   (connection-string
    :initarg :connection-string
    :reader catalog-connection-string)
   (schemata
    :initarg :schemata
    :type list
    :accessor catalog-schemata)
   (sequences
    :initarg :sequences
    :type list
    :accessor catalog-sequences)
   (tables
    :initarg :catalog-tables
    :type list
    :accessor catalog-tables)
   (dependencies
    :initform nil
    :accessor %catalog-dependencies)))


(defun register-dependency (catalog object dependency type)
  (push (list object dependency type)
        (%catalog-dependencies catalog)))

(defgeneric dependencies (object catalog)
  (:method (object (catalog catalog))
    (loop
       for dep in (%catalog-dependencies catalog)
       when (eq object (first dep))
       collect (second dep))))

(defgeneric dependants (object catalog)
  (:method (object (catalog catalog))
    (loop
       for dep in (%catalog-dependencies catalog)
       when (eq object (second dep))
       collect (first dep))))


(defmethod print-object ((x catalog) stream)
  (print-unreadable-object (x stream :type t)
    (prin1 (catalog-name x) stream)))
