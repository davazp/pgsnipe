;;; src/main.lisp ---

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

(uiop/package:define-package :pgsnipe/main
  (:nicknames :pgsnipe)
  (:use :common-lisp
        :pgsnipe/nodes
        :pgsnipe/inspect
        :pgsnipe/diff))

(in-package :pgsnipe/main)

(defvar *version*
  (asdf:component-version (asdf:find-system "pgsnipe"))
  "The pgsnipe version")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "sb-concurrency"))

(defun inspect-databases (source-connstring target-connstring)
  (let (source target)
    (let ((master (sb-concurrency:make-mailbox :name "master")))
      (sb-thread:make-thread
       (lambda ()
         (setq source (inspect-catalog source-connstring))
         (sb-concurrency:send-message master "source-finished")))
      (sb-thread:make-thread
       (lambda ()
         (setq target (inspect-catalog target-connstring))
         (sb-concurrency:send-message master "target-finished")))
      ;; Wait until both databases are inspected.
      (sb-concurrency:receive-message master)
      (sb-concurrency:receive-message master))
    (values source target)))


(defun diff-databases (source-connstring target-connstring)
  (multiple-value-bind (source target)
      (inspect-databases source-connstring target-connstring)
    (generate-diff source target *standard-output*)))


#+nil
(defun connect ()
  (apply #'postmodern:connect-toplevel (pgsnipe/postgres-connstring:parse "postgresql:///")))
