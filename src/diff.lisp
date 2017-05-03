;;; src/diff.lisp ---

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

(uiop:define-package :pgsnipe/diff
  (:mix :pgsnipe/nodes
        :pgsnipe/inspect
        :pgsnipe/lexical
        :common-lisp
        :alexandria)
  (:export #:generate-diff))

(in-package :pgsnipe/diff)

(defvar *target-catalog*)
(defvar *dropped*)

(defgeneric diff (source target)
  (:documentation "Generate a diff between the SOURCE and TARGET."))

(defgeneric create (object)
  (:documentation "Generate code to create OBJECT."))

(defgeneric alter (source target)
  (:documentation "Generate code to alter SOURCE into TARGET."))

(defgeneric drop (object)
  (:documentation "Generate code to drop OBJECT."))

(defmethod drop :around (object)
  ;; If an object has already been dropped, don't do anything
  (unless (gethash object *dropped*)
    (prog1 (append
            ;; Drop all the dependant objects in the target catalog.
            (and (boundp '*target-catalog*)
                 (mappend #'drop (dependants object *target-catalog*)))
            ;; And then the given object.
            (call-next-method))
      (setf (gethash object *dropped*) t))))


(defun diff-sets (set1 set2)
  "Generate a list of actions to convert SET1 into SET2. Objects from
both sets are assumed to be defined on the same object. As sets, the
order of the elements is not considered."
  (append
   (mappend #'create (set-difference set1 set2 :key #'sql-qualified-name :test #'string=))
   (mappend #'drop   (set-difference set2 set1 :key #'sql-qualified-name :test #'string=))
   (let ((updates nil))
     (dolist (x set1 (reverse updates))
       (when-let* ((y (find (sql-qualified-name x) set2 :key #'sql-qualified-name :test #'string=)))
         (dolist (patch (alter x y))
           (push patch updates)))))))


;;; Catalogs

(defun header (title)
  (list (format nil "~%---- ~a ----" title)))

(defmethod diff ((source catalog) (target catalog))
  (append
   (diff-sets (catalog-schemata source) (catalog-schemata target))
   (diff-sets (catalog-sequences source) (catalog-sequences target))
   (diff-sets (catalog-tables source) (catalog-tables target))))


;;; Schemata
;;; 
;;; TODO: Support for owner

(defmethod create ((s schema))
  (list (format nil "CREATE SCHEMA ~a;" (schema-name s))))

(defmethod drop ((s schema))
  (list (format nil "DROP SCHEMA ~a;" (schema-name s))))

(defmethod alter ((s1 schema) (s2 schema))
  nil)


;;; Sequences

(defmethod create ((seq sequence*))
  (list
   (with-output-to-string (out)
     (format out "CREATE SEQUENCE ~a~%" (sql-qualified-name seq))
     (format out "  START WITH ~d~%" (sequence-start-value seq))
     (format out "  INCREMENT BY ~d~%" (sequence-increment seq))
     (format out "  MINVALUE ~d~%" (sequence-minimum-value seq))
     (format out "  MAXVALUE ~d~%" (sequence-maximum-value seq))
     (if (sequence-cycle-option seq)
         (format out "  CYCLE")
         (format out "  NO CYCLE"))
     (format out ";~%~%"))))

(defmethod drop ((seq sequence))
  (list (format nil "DROP SEQUENCE ~a;" (sql-qualified-name seq))))

(defmethod alter ((source sequence*) (target sequence*))
  (let ((alters
         (remove nil
                 (list
                  (when (/= (sequence-increment source) (sequence-increment target))
                    (lambda (out)
                      (format out "~%  INCREMENT BY ~d  -- current value is ~d"
                              (sequence-increment target)
                              (sequence-increment source))))
                  
                  (when (/= (sequence-minimum-value source) (sequence-minimum-value target))
                    (lambda (out)
                      (format out "~%  MINVALUE ~d  -- current value is ~d"
                              (sequence-minimum-value target)
                              (sequence-minimum-value source))))
                  
                  (when (/= (sequence-maximum-value source) (sequence-maximum-value target))
                    (lambda (out)
                      (format out "~%  MAXVALUE ~d  -- current value is ~d"
                              (sequence-maximum-value target)
                              (sequence-maximum-value source))))
                  
                  (when (/= (sequence-start-value source) (sequence-start-value target))
                    (lambda (out)
                      (format out "~%  START WITH ~d  -- current value is ~d"
                              (sequence-start-value target)
                              (sequence-start-value source))))
                  
                  (when (not (eql (sequence-cycle-option source) (sequence-cycle-option target)))
                    (lambda (out)
                      (format out "~%  ~:[NO ~;~]CYCLE" (sequence-cycle-option target))))))))
    
    (when alters
      (list
       (with-output-to-string (out)
         ;; 
         ;; TODO: How to integarte renaming of objects here?
         ;; 
         ;; SOURCE and TARGET are assumed to have the same name.
         ;; 
         (format out "ALTER SEQUENCE ~a" (sql-qualified-name source))
         (dolist (alter alters)
           (funcall alter out))
         (format out ";~%"))))))



;;; Tables

(defmethod create ((table table))
  (list (with-output-to-string (out)
          (format out "CREATE TABLE ~a (~%~{    ~a~^,~%~}~%);~%"
                  (sql-qualified-name table)
                  (mapcar (lambda (col)
                            (format nil "~a ~a DEFAULT ~a"
                                    (column-name col)
                                    (column-data-type col)
                                    (column-default-value col)))
                          (table-columns table))))))

(defmethod drop ((table table))
  (list (format nil "DROP TABLE ~a;~%" (sql-qualified-name table))))


(defmethod drop ((def column-default))
  (let* ((column (column-default-column def))
         (table (column-table column)))
    (list (format nil "ALTER TABLE ~a ALTER COLUMN ~a DROP DEFAULT;~%"
                  (sql-qualified-name table)
                  (column-name column)))))


(defun migrate-data (from to)
  (let ((columns (intersection
                  (mapcar #'column-name (table-columns from))
                  (mapcar #'column-name (table-columns to))
                  :test #'string=)))
    (when columns
      (list
       (with-output-to-string (out)
         (format out "INSERT INTO ~a (~{~a~^, ~})~%" (sql-qualified-name to) columns)
         (format out "SELECT ~{~a~^, ~}~%" columns)
         (format out "FROM ~a;~%" (sql-qualified-name from)))))))


(defun rename-table (table name)
  (let ((table-name (sql-qualified-name table)))
    (list (format nil "ALTER TABLE ~a RENAME TO ~a;~%" table-name name))))


;;;
;;; We want target to be like source
;;;
(defun compare-columns (source target)
  (let* ((source-columns (table-columns source))
         (target-columns (table-columns target))

         (added (remove-if (lambda (col)
                             (find (column-name col) target-columns :key #'column-name :test #'string=))
                           source-columns))

         (dropped (remove-if (lambda (col)
                               (find (column-name col) source-columns :key #'column-name :test #'string=))
                             target-columns)))

    ;; Compatibility condition: every added column must be at the end
    ;; of the table.
    (cond
      ((and (null added) (null dropped))
       (values nil nil t))
      ((let ((remaining-columns
              ;; Columns from TARGET that we are not going to drop.
              (remove-if (lambda (col) (find col dropped)) target-columns)))
         (starts-with-subseq remaining-columns
                             source-columns
                             :key #'column-name
                             :test #'string=))

       (values added dropped t))
      (t
       (values added dropped nil)))))


(defmethod alter ((source table) (target table))
  (multiple-value-bind (added dropped compatible)
      (compare-columns source target)
    (cond
      (compatible
       (append (mapcar (lambda (col)
                         (format nil "ALTER TABLE ~a ADD ~a ~a DEFAULT ~a;~%"
                                 (sql-qualified-name target)
                                 (column-name col)
                                 (column-data-type col)
                                 (column-default-value col)))
                       added)
               (mapcar (lambda (col)
                         (format nil "ALTER TABLE ~a DROP COLUMN ~a;"
                                 (sql-qualified-name target)
                                 (column-name col)))
                       dropped)))
      (t
       (let ((tmp (copy-table source :name (format nil "__PGSNIPE_TMP_~a" (table-name source)))))
         (append (create tmp)
                 (migrate-data target tmp)
                 (drop source)
                 (rename-table tmp (sql-qualified-name target))))))))



(defun generate-diff (source target stream &key (commit nil))
  "Generate a SQL script to update TARGET so it looks like SOURCE."
  (let ((*target-catalog* target)
        (*dropped* (make-hash-table)))

    (let ((schema (sql-identifier *default-schema*)))
      (format stream "SET search_path TO ~a;~%" schema))

    (write-line "BEGIN TRANSACTION;" stream)
    (write-line "" stream)
    
    (dolist (x (diff source target))
      (write-line x stream))
    (terpri stream)
    (if commit
        (write-line "COMMIT TRANSACTION;" stream)
        (write-line "ROLLBACK TRANSACTION;" stream))))
