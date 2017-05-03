;;; src/migrate.lisp ---

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

(uiop:define-package :pgsnipe/migrate
  (:mix :pgsnipe/diff
        :pgsnipe/tmpdb
        :pgsnipe/execute
        :common-lisp
        :alexandria)
  (:export #:migrate))

(in-package :pgsnipe/migrate)


(defun migrate (connstring script &rest others &key &allow-other-keys)
  "Migrate the database at CONNSTRING to look like the database
described in the SCRIPT file."
  ;; We open the script early to ensure it exists, as it is a faster
  ;; operation than creating the temporary database.
  (with-input-from-file (input script)
    (let* ((tmpdb (create-tmpdb))
           (tmpdb-connstring (format nil "postgres:///~a" tmpdb)))
      (unwind-protect
           (progn
             ;; Create the temporary table and apply the script to it.
             (execute-stream tmpdb-connstring input)
             ;; Generate the script to update the database, by
             ;; comparing the temporary database we just created to
             ;; the target database.
             (let ((delta
                    (with-output-to-string (out)
                      (apply #'generate-diff tmpdb-connstring connstring out others))))
               (write-string delta *standard-output*)
               ;; Apply the delta script generated to the target database
               (execute-string connstring delta)))
        ;; Destroy the temporary database
        (dropdb tmpdb)))))
