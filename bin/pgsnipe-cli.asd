;;; bin/pgsnipe-cli.asd ---

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

#-asdf3 (error "pgsnipe requires ASDF 3")

(asdf:defsystem :pgsnipe-cli
  :name "pgsnipe CLI"
  :description "Command line interface for pgsnipe"
  :version (:read-file-form "../version.lisp-expr")
  :license "GPLv3"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:uiop :pgsnipe-cli/main))
