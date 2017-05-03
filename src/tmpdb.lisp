;;; src/tmpdb.lisp ---

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

(defpackage :pgsnipe/tmpdb
  (:use :common-lisp :postmodern :pgsnipe/utils)
  (:export #:create-tmpdb #:dropdb))

(in-package :pgsnipe/tmpdb)


(defvar *maintenance-database* "postgres"
  "The database that pgsnipe will connect to in order to create the
new database. By default, the 'postgres' database is created, as all
the PostgreSQL utility commands do.")


(defun create-tmpdb ()
  (with-connection (list *maintenance-database* (get-username) nil :unix)
    (loop
       for i from 1 to 5
       do (let ((name (format nil "pgsnipe_tmpl_~d" i)))
            (handler-case
                (progn
                  (execute (format nil "CREATE DATABASE ~a;" name))
                  (return-from create-tmpdb name))
              (error () nil))))
    (error "Temporal database couldn't be created")))


(defun dropdb (database)
  (with-connection (list "postgres" (get-username) nil :unix)
    (execute (format nil "DROP DATABASE ~a" database))))
