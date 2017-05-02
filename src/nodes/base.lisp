;;; src/nodes/base.lisp

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

(uiop/package:define-package :pgsnipe/nodes/base
  (:use :common-lisp)
  (:export #:sql-qualified-name
           #:node
           #:node-classid
           #:node-id))

(in-package :pgsnipe/nodes/base)

(defclass node ()
  ((classid
    :initarg :classid
    :initform (error "classid is required")
    :reader node-classid)
   (oid
    :initarg :oid
    :initform (error "oid is required")
    :reader node-id)))

(defgeneric sql-qualified-name (object)
  (:documentation "Return a string that uniquely identify OBJECT in
the database among the objects of the same type.

Note that PostgreSQL does not force this name to be unique in the
whole database. For example, constraints could be duplicated. However,
we can always use it to distinguish between other objects defined
under the same parent object.

See the note at the bottom of 

   https://www.postgresql.org/docs/current/static/information-schema.html

for futher information."))
