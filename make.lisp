;;; make.lisp ---

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

#-asdf3
(error "ASDF3 needs to be installed to build pgsnipe.")

(defpackage :pgsnipe-bootstrap
  (:use :common-lisp))

(in-package :pgsnipe-bootstrap)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "pgsnipe")
  (asdf:load-system "pgsnipe-cli"))

(let ((options
       (list :toplevel #'pgsnipe-cli:main
             :executable t
             :save-runtime-options t)))
  (when (find :sb-core-compression *features*)
    (setf options (acons :compression 5 options)))

  (apply #'sb-ext:save-lisp-and-die "pgsnipe" options))
