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

(uiop:define-package :pgsnipe-cli/main
  (:mix :common-lisp
        :pgsnipe
        :postmodern)
  (:nicknames :pgsnipe-cli)
  (:import-from :unix-opts)
  (:export #:main))

(in-package :pgsnipe-cli)

(defvar *program*)



(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :commit
   :description "commit the migration script"
   :long "commit")
  (:name :verbose
   :description "verbose output"
   :short #\v
   :long "verbose"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))


(defun help ()
  (opts:describe
   :prefix "pgsnipe — A tool to manage PostgreSQL migrations"
   ;; :suffix "so that's how it works…"
   :usage-of "pgsnipe"
   :args     "<script>")
  (sb-ext:exit :abort t))


(defun main ()
  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (format t "fatal: option ~s needs an argument!~%"
                  (opts:option condition)))

        (opts:arg-parser-failed (condition)
          (format t "fatal: cannot parse ~s as argument of ~s~%"
                  (opts:raw-arg condition)
                  (opts:option condition))))
    
    ;; Here all options are checked independently, it's trivial to code any
    ;; logic to process them.
    (when-option (options :help)
      (help))
    
    (when-option (options :verbose)
      (format t "OK, running in verbose mode…~%"))

    (unless (= (length free-args) 1)
      (help))

    (let ((script (first free-args))
          (commit (getf options :commit)))

      (handler-case
          (migrate "postgres://" script :commit commit)

        (database-error (condition)
          (format *error-output* "error: ~a~%" (database-error-message condition)))

        (simple-error (condition)
          (format *error-output* "error: ~?"
                  (simple-condition-format-control condition)
                  (simple-condition-format-arguments condition)))))))
