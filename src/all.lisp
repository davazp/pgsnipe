;;; src/all.lisp ---

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

(uiop/package:define-package :pgsnipe/all
  (:nicknames :pgsnipe)
  (:use :common-lisp
        :pgsnipe/nodes
        :pgsnipe/inspect))

(in-package :pgsnipe/all)

(defvar *version*
  (asdf:component-version (asdf:find-system "pgsnipe"))
  "The pgsnipe version")

#+nil
(defun connect ()
  (apply #'postmodern:connect-toplevel (pgsnipe/postgres-connstring:parse "postgresql:///")))
