;;; src/inspect.lisp

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

(uiop/package:define-package :pgsnipe/inspect
  (:mix :pgsnipe/nodes
        :pgsnipe/utils
        :pgsnipe/connstring
        :alexandria
        :common-lisp
        :postmodern)
  (:export #:inspect-catalog
           #:inspect-databases))

(in-package :pgsnipe/inspect)

(defvar *ignored-schemata*
  '("pg_catalog" "pg_toast" "pg_temp_1" "pg_toast_temp_1" "information_schema")
  "Schemas to ignore.")


;;; 
;;; Inspect information_schema and system catalogs
;;; 

(defun query-implementation-info ()
  (query "SHOW server_version" :single))


(defun query-catalog-name ()
  (values (query "SELECT * FROM information_schema.information_schema_catalog_name" :single)))

(defun query-schemata ()
  (query "SELECT 'pg_catalog.pg_namespace'::regclass::oid as classid,
                 schema_name::regnamespace::oid as oid,
                 schema_name as name,
                 schema_owner as owner
          FROM information_schema.schemata
          WHERE schema_name <> ALL($1)"
         (coerce *ignored-schemata* 'vector) :plists))

(defun query-tables ()
  (query "SELECT 'pg_catalog.pg_class'::regclass::oid as classid,
                 (quote_ident(table_schema) || '.' || quote_ident(table_name))::regclass::oid,
                 table_schema as schema_name,
                 table_name as name
          FROM information_schema.tables
          WHERE table_schema <> ALL($1)"
         (coerce *ignored-schemata* 'vector) :plists))

(defun query-columns ()
  (query "SELECT c.table_schema as table_schema_name,
                 c.table_name,
                 c.column_name as name,

                'pg_catalog.pg_attrdef'::regclass::oid as default_classid,
                 def.oid as default_oid,
                 c.column_default as default,

                 c.is_nullable,
                 c.data_type,
                 c.udt_schema,
                 c.udt_name,
                 e.data_type as element_data_type,
                 COALESCE(c.character_maximum_length, e.character_maximum_length) AS character_maximum_length
          FROM information_schema.columns c
          LEFT JOIN information_schema.element_types e
          ON ((c.table_catalog, c.table_schema, c.table_name, 'TABLE', c.dtd_identifier)
               = (e.object_catalog, e.object_schema, e.object_name, e.object_type, e.collection_type_identifier))

          LEFT JOIN pg_catalog.pg_attrdef def ON (
             def.adrelid = (quote_ident(c.table_schema) || '.' || quote_ident(c.table_name))::regclass AND 
             def.adnum = c.ordinal_position
          )

          WHERE c.table_schema <> ALL($1)
          ORDER BY c.ordinal_position
" (coerce *ignored-schemata* 'vector) :plists))



(defun query-check-constraints ()
  ;; We cannot use the information_schema.check_constraints view here
  ;; as it includes implicitly defined NOT NULL constraints.
  ;;
  ;; Instead, we rely on the pg_catalog. The definition of
  ;; check_constraints is taken directly from the definition of the
  ;; information_schema catalog in the Postgresql source code.
  ;;
  (query "
WITH check_constraints AS (
   SELECT con.oid,
          'pg_catalog.pg_constraint'::regclass::oid as classid,
          current_database() AS constraint_catalog,
          rs.nspname AS constraint_schema,
          con.conname AS constraint_name,
          substring(pg_get_constraintdef(con.oid) from 7) AS check_clause
   FROM pg_constraint con
   LEFT OUTER JOIN pg_namespace rs ON (rs.oid = con.connamespace)
   LEFT OUTER JOIN pg_class c ON (c.oid = con.conrelid)
   LEFT OUTER JOIN pg_type t ON (t.oid = con.contypid)
   WHERE pg_has_role(coalesce(c.relowner, t.typowner), 'USAGE')
      AND con.contype = 'c'
)
   SELECT cc.oid,
          cc.classid,
          tc.table_schema,
          tc.table_name,
          cc.constraint_schema,
          cc.constraint_name,
          tc.is_deferrable,
          tc.initially_deferred,
          cc.check_clause
   FROM information_schema.table_constraints tc
   JOIN check_constraints cc
     ON tc.constraint_catalog = cc.constraint_catalog
    AND tc.constraint_schema  = cc.constraint_schema
    AND tc.constraint_name    = cc.constraint_name
" :plists))


(defun query-primary-keys ()
  (query "
SELECT 'pg_constraint'::regclass::oid as classid,
       tc.constraint_schema as schema_name,
       tc.constraint_name as name,
       tc.table_schema as table_schema_name,
       tc.table_name as table_name,
       tc.is_deferrable::boolean,
       tc.initially_deferred::boolean,
       kc.column_name,
       kc.ordinal_position
FROM information_schema.table_constraints tc
JOIN information_schema.key_column_usage kc
  ON (tc.constraint_catalog, tc.constraint_schema, tc.constraint_name)
   = (kc.constraint_catalog, kc.constraint_schema, kc.constraint_name)
WHERE tc.constraint_type='PRIMARY KEY'
  AND tc.constraint_schema <> ALL($1)
ORDER BY kc.ordinal_position"
         (coerce *ignored-schemata* 'vector) :plists))


(defun query-sequences ()
  (query "
SELECT 'pg_catalog.pg_class'::regclass::oid as classid,
       (quote_ident(sequence_schema) || '.' || quote_ident(sequence_name))::regclass::oid as oid,
       sequence_schema as schema_name,
       sequence_name as name,
       start_value::bigint,
       minimum_value::bigint,
       maximum_value::bigint,
       increment::bigint,
       cycle_option::boolean
FROM information_schema.sequences
WHERE sequence_schema <> ALL($1)"
         (coerce *ignored-schemata* 'vector) :plists))



(defun query-foreign-keys ()
  (query "
SELECT 'pg_catalog.pg_constraint'::regclass::oid as classid,
       (
           SELECT oid FROM pg_catalog.pg_constraint c
           WHERE c.conname = tc.constraint_name
             AND c.connamespace = tc.constraint_schema::regnamespace::oid
             AND c.conrelid = (quote_ident(tc.table_schema) || '.' || quote_ident(tc.table_name))::regclass::oid
       ),

       rc.constraint_schema,
       rc.constraint_name,
       rc.match_option,
       rc.update_rule,
       rc.delete_rule,

       sc.table_schema,
       sc.table_name,
       sc.column_name,
       sc.ordinal_position,

       tc.table_schema as referenced_table_schema,
       tc.table_name   as referenced_table_name,
       tc.column_name  as referenced_column_name

FROM information_schema.referential_constraints rc
JOIN information_schema.key_column_usage sc
  ON (sc.constraint_catalog, sc.constraint_schema, sc.constraint_name) = (rc.constraint_catalog, rc.constraint_schema, rc.constraint_name)
JOIN information_schema.key_column_usage tc
  ON (tc.constraint_catalog, tc.constraint_schema, tc.constraint_name) = (rc.unique_constraint_catalog, rc.unique_constraint_schema, rc.unique_constraint_name)
WHERE tc.ordinal_position = sc.position_in_unique_constraint
ORDER by sc.ordinal_position;
" :plists))



(defun query-dependencies ()
  (query "
SELECT classid, objid, objsubid,
       refclassid, refobjid, refobjsubid,
       deptype::char
FROM pg_catalog.pg_depend
WHERE deptype='n'
" :plists))




;;; 
;;; Build node instances from the information schema
;;;

(defvar *object-index*)

(defun make-object-index ()
  (make-hash-table :test #'equal))

(defun resolve-object (classid objid &optional subobjid)
  (let* ((key (list classid objid))
         (object (gethash key *object-index*)))
    (cond
      ((null object)
       nil)
      ((and subobjid (plusp subobjid))
       (assert (eq (type-of object) 'table))
       (let ((column (nth (1- subobjid) (table-columns object))))
         (assert (not (null column)))
         column))
      (t
       object))))

(defun register-object (object)
  (when (boundp '*object-index*)
    (let ((classid (node-classid object))
          (objid (node-id object)))
      (when (and classid objid)
        (let ((key (list classid objid)))
          (setf (gethash key *object-index*) object))))))


(defun inspect-catalog (connstring)
  (let ((conninfo (parse connstring)))
    (postmodern:with-connection conninfo
      (let* ((name (query-catalog-name))
             (catalog (make-instance 'catalog :name name :connection-string connstring))
             (*object-index* (make-object-index)))

        (setf (catalog-sequences catalog) (inspect-sequences)
              (catalog-schemata catalog) (inspect-schemata)
              (catalog-tables catalog) (inspect-tables))
        ;; 
        ;; Register the dependencies between the objects
        ;;
        (dolist (dep (query-dependencies))
          (with-plist (classid objid objsubid refclassid refobjid refobjsubid) dep
            (let ((dependant (resolve-object classid objid objsubid))
                  (dependency (resolve-object refclassid refobjid refobjsubid)))
              ;; Only consider a dependency if the dependant object or
              ;; the dependency are tracked objects.
              (when (or dependant dependency)
                (register-dependency catalog
                                     (or dependant
                                         (make-instance 'node :oid objid :classid classid))
                                     (or dependency
                                         (make-instance 'node :oid refobjid :classid refclassid))
                                     :normal)))))
        catalog))))


(defun inspect-sequences ()
  (mapcar (lambda (row)
            (register-object (apply #'make-instance 'sequence* row)))
          (query-sequences)))

(defun inspect-schemata ()
  (mapcar (lambda (row)
            (register-object (apply #'make-instance 'schema row)))
          (query-schemata)))



;;; Now we implement a few functions to query table columns and
;;; constraints. We do not want to do too many database roundtrips so
;;; `inspect-tables' will bind all of them into the following special
;;; variables. Some functions are provided to query the information in
;;; a more sensitive way.


(defun row-references-table-p
    (table &optional (schema-column :table-schema-name) (table-column :table-name))
  "Check if the result of a query result references the table TABLE.
   - SCHEMA-COLUMN is the column that references the schema name.
   - TABLE-COLUMN  is the column that references the table name in the schema.
"
  (lambda (row)
    (and (string= (table-schema-name table) (getf row schema-column))
         (string= (table-name table) (getf row table-column)))))


(defun query-columns-in-table (table columns)
  "Like `query-columns', but restrict the result to the columns in TABLE."
  (remove-if (complement (row-references-table-p table)) columns))

(defun query-table-primary-key-columns (table primary-keys)
  "Like `query-primary-keys' but returns just primary key for TABLE."
  (remove-if (complement (row-references-table-p table)) primary-keys))


(defun inspect-columns (table columns)
  (mapcar (lambda (row)
            (let* ((initargs (pick row '(:name :is-nullable :data-type)))
                   (column (apply #'make-instance 'column :table table initargs))
                   (default (make-instance 'column-default
                                           :column column
                                           :classid (getf row :default-classid)
                                           :oid (getf row :default-oid)
                                           :value (getf row :default))))
              (register-object default)
              (setf (column-default column) default)
              column))
          
          (query-columns-in-table table columns)))


(defun inspect-primary-key (table primary-keys)
  (let ((pk-fields (query-table-primary-key-columns table primary-keys)))
    (if (null pk-fields)
        nil
        (let* ((table-columns (table-columns table))
               (row (first pk-fields))
               (initargs (pick row '(:classid :oid :name :schema-name :initially-deferred :is-deferrable)))
               (pk (apply #'make-instance 'primary-key-constraint initargs)))
          ;; Note that PK-FIELDS is ordered by ORDINAL_POSITION, so we
          ;; will return the column objects in the same order.
          (setf (primary-key-columns pk)
                (mapcar (lambda (pkcol)
                          (or (find (getf pkcol :column-name) table-columns
                                    :test #'string=
                                    :key #'column-name)
                              (error "The primary key of the table ~a references a column ~a but we could not find the column definition in the table."
                                     (table-name table) (getf pkcol :name))))
                        pk-fields))
          pk))))


(defun inspect-tables ()
  (let ((tables (query-tables))
        (columns (query-columns))
        (primary-keys (query-primary-keys)))
    (mapcar (lambda (row)
              (let ((table (apply #'make-instance 'table row)))
                (setf (table-columns table) (inspect-columns table columns))
                (setf (table-primary-key table) (inspect-primary-key table primary-keys))
                (register-object table)
                table))
            tables)))
