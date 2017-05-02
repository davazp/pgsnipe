;;; bin/main.lisp ---

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

(defpackage :pgsnipe-cli/main
  (:use :common-lisp
        :pgsnipe
        :postmodern)
  (:nicknames :pgsnipe-cli)
  (:export #:main))

(in-package :pgsnipe-cli)

(defvar *program*)

(defun help ()
  "Display the help of the command."
  (format *error-output* "Usage: ~a <script>~%" *program*)
  (sb-ext:exit :abort t))

(defun main ()
  (let ((*program* (first sb-ext:*posix-argv*))
        (args (subseq sb-ext:*posix-argv* 1)))
    (cond
      ((equalp args '("--version"))
       (format t "pgsnipe ~a~%" pgsnipe::*version*))

      ((equalp args '("--help"))
       (help))

      (t
       (unless (= (length args) 1)
         (help))

       (handler-case
           (let ((script (first args)))
             #+nil (pgsnipe/migrate:migrate "postgres:///" script))

         (postmodern:database-error (condition)
           (format *error-output* "ERROR: ~a~%" (postmodern:database-error-message condition)))

         (simple-error (condition)
           (format *error-output* "ERROR: ~?"
                   (simple-condition-format-control condition)
                   (simple-condition-format-arguments condition))))))))
