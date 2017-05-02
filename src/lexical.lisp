;;; src/lexical.lisp ---

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

(uiop/package:define-package :pgsnipe/lexical
  (:use :common-lisp)
  (:export #:*default-schema*
           #:sql-identifier
           #:sql-qualified-identifier))

(in-package :pgsnipe/lexical)


(defvar *postgresql-reserved-words*
  '("ABS"
    "ALIAS"
    "ALL"
    "ALLOCATE"
    "ANALYSE"
    "ANALYZE"
    "AND"
    "ANY"
    "ARE"
    "ARRAY"
    "AS"
    "ASC"
    "ASENSITIVE"
    "ASYMMETRIC"
    "ATOMIC"
    "AUTHORIZATION"
    "AVG"
    "BETWEEN"
    "BINARY"
    "BLOB"
    "BOTH"
    "CALL"
    "CARDINALITY"
    "CASCADED"
    "CASE"
    "CAST"
    "CEIL"
    "CEILING"
    "CHARACTER_LENGTH"
    "CHAR_LENGTH"
    "CHECK"
    "CLOB"
    "COLLATE"
    "COLLECT"
    "COLUMN"
    "COMPLETION"
    "CONDITION"
    "CONNECT"
    "CONSTRAINT"
    "CORR"
    "CORRESPONDING"
    "COUNT"
    "COVAR_POP"
    "COVAR_SAMP"
    "CREATE"
    "CROSS"
    "CUBE"
    "CUME_DIST"
    "CURRENT"
    "CURRENT_DATE"
    "CURRENT_DEFAULT_TRANSFORM_GROUP"
    "CURRENT_PATH"
    "CURRENT_ROLE"
    "CURRENT_TIME"
    "CURRENT_TIMESTAMP"
    "CURRENT_TRANSFORM_GROUP_FOR_TYPE"
    "CURRENT_USER"
    "DATE"
    "DEFAULT"
    "DEFERRABLE"
    "DENSE_RANK"
    "DEREF"
    "DESC"
    "DESCRIBE"
    "DESTROY"
    "DESTRUCTOR"
    "DETERMINISTIC"
    "DICTIONARY"
    "DISCONNECT"
    "DISTINCT"
    "DO"
    "DYNAMIC"
    "ELEMENT"
    "ELSE"
    "END"
    "END-EXEC"
    "EVERY"
    "EXCEPT"
    "EXEC"
    "EXP"
    "FALSE"
    "FILTER"
    "FLOOR"
    "FOR"
    "FOREIGN"
    "FREE"
    "FREEZE"
    "FROM"
    "FULL"
    "FUSION"
    "GET"
    "GRANT"
    "GROUP"
    "GROUPING"
    "HAVING"
    "HOST"
    "IDENTITY"
    "IGNORE"
    "ILIKE"
    "IN"
    "INDICATOR"
    "INITIALIZE"
    "INITIALLY"
    "INNER"
    "INTERSECT"
    "INTERSECTION"
    "INTO"
    "IS"
    "ISNULL"
    "ITERATE"
    "JOIN"
    "LATERAL"
    "LEADING"
    "LEFT"
    "LESS"
    "LIKE"
    "LIMIT"
    "LN"
    "LOCALTIME"
    "LOCALTIMESTAMP"
    "LOWER"
    "MAX"
    "MEMBER"
    "MERGE"
    "METHOD"
    "MIN"
    "MOD"
    "MODIFIES"
    "MODIFY"
    "MODULE"
    "MULTISET"
    "NATURAL"
    "NCLOB"
    "NEW"
    "NORMALIZE"
    "NOT"
    "NOTNULL"
    "NULL"
    "OCTET_LENGTH"
    "OFF"
    "OFFSET"
    "OLD"
    "ON"
    "ONLY"
    "OPEN"
    "OPERATION"
    "OR"
    "ORDER"
    "OUTER"
    "OVER"
    "OVERLAPS"
    "PARAMETER"
    "PARAMETERS"
    "PARTITION"
    "PERCENTILE_CONT"
    "PERCENTILE_DISC"
    "PERCENT_RANK"
    "PLACING"
    "POSTFIX"
    "POWER"
    "PREFIX"
    "PREORDER"
    "PRIMARY"
    "RANGE"
    "RANK"
    "READS"
    "RECURSIVE"
    "REF"
    "REFERENCES"
    "REFERENCING"
    "REGR_AVGX"
    "REGR_AVGY"
    "REGR_COUNT"
    "REGR_INTERCEPT"
    "REGR_R2"
    "REGR_SLOPE"
    "REGR_SXX"
    "REGR_SXY"
    "REGR_SYY"
    "RESULT"
    "RETURN"
    "RIGHT"
    "ROLLUP"
    "ROW_NUMBER"
    "SCOPE"
    "SEARCH"
    "SELECT"
    "SENSITIVE"
    "SESSION_USER"
    "SIMILAR"
    "SOME"
    "SPECIFIC"
    "SPECIFICTYPE"
    "SQL"
    "SQLCODE"
    "SQLERROR"
    "SQLEXCEPTION"
    "SQLSTATE"
    "SQLWARNING"
    "SQRT"
    "STATIC"
    "STDDEV_POP"
    "STDDEV_SAMP"
    "SUBMULTISET"
    "SUM"
    "SYMMETRIC"
    "SYSTEM_USER"
    "TABLE"
    "TABLESAMPLE"
    "TERMINATE"
    "THAN"
    "THEN"
    "TIMEZONE_HOUR"
    "TIMEZONE_MINUTE"
    "TO"
    "TRAILING"
    "TRANSLATE"
    "TRANSLATION"
    "TRUE"
    "UESCAPE"
    "UNION"
    "UNIQUE"
    "UNNEST"
    "UPPER"
    "USER"
    "USING"
    "VALUE"
    "VARIABLE"
    "VAR_POP"
    "VAR_SAMP"
    "VERBOSE"
    "WHEN"
    "WHENEVER"
    "WHERE"
    "WIDTH_BUCKET"
    "WINDOW"
    "WITHIN")
  "List of reserved names in PostgreSQL. This names cannot be used in
tables or columns, and should be quoted.

This list has been generated from:
   https://www.postgresql.org/docs/current/static/sql-keywords-appendix.html
")


(defun quote-sql-identifier (name)
  "Quote NAME to be a valid sql valid identifier"
  (with-output-to-string (out)
    (write-char #\" out)
    (with-input-from-string (in name)
      (loop
         for ch across name
         if (char= ch #\") do (write-string "\"\"" out)
         else do (write-char ch out)))
    (write-char #\" out)))


(defun valid-sql-identifier-p (str)
  "Check if STR is a valid unquoted identifier."
  (and (>= (length str) 1)
       (not (find (string-upcase str) *postgresql-reserved-words* :test #'string=))
       (alpha-char-p (char str 0))
       (every (lambda (ch)
                (or (alphanumericp ch)
                    (find ch #(#\_))))
              (subseq str 1))))

(defun sql-identifier (name)
  "Quote NAME if it is not a valid SQL identifier"
  (if (valid-sql-identifier-p name)
      name
      (quote-sql-identifier name)))



(defvar *default-schema* "public"
  "The default schema. If the schema is not specified in a name, this schema will be used.")


(defun sql-qualified-identifier (schema name)
  (with-output-to-string (out)
    (unless (string= schema *default-schema*)
      (write-string (sql-identifier schema) out)
      (write-char #\. out))
    (write-string (sql-identifier name) out)))
