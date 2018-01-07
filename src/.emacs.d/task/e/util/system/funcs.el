;;; package --- Summary -*- lexical-binding: t -*-
;;
;; Author: Minae Yui <minae.yui.sain@gmail.com>
;; Version: 0.1
;; URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; .
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar --path-hash-table (make-hash-table :test 'eq)
  "Hashmap that contains some system paths.
Relation: symbol => string")

(defun func/system/mac-p ()
  "Return t if systeme is mac."
  (eq system-type 'darwin))

(defun func/system/linux-p ()
  "Return t if system is linux."
  (eq system-type 'gnu/linux))

(defun func/system/windows-p ()
  "Return t if system is windows."
  (eq system-type 'windows-nt))

(defun func/system/user-login ()
  "Return user login."
  (getenv
   (if (func/system/windows-p) "USERNAME" "USER")))

(defun func/system/user-home ()
  "Return user home path."
  (getenv
   (if (func/system/windows-p) "HOMEPATH" "HOME")))

(defun func/system/getenv (env-variable &optional default-value)
  "Return environment variable value of ENV-VARIABLE.
If it's nil return DEFAULT-VALUE."
  (if (stringp env-variable)
      (or (getenv env-variable)
          default-value
          (error (format "Environment variable %s was not found"
                         env-variable)))
    (error "Can't get value of non-string key from environment")))

(defun func/system/path-exists (key)
  "Returns t if path with KEY is registered"
  (if (gethash key --path-hash-table)
      t
    nil))

(defun func/system/path-get (key)
  "Returns t if path with KEY is registered"
  (gethash key --path-hash-table))

(defun func/system/path-register (key path)
  "Register PATH with KEY"
  (puthash key path --path-hash-table))

(defun init ()
  "Init system functions"
  )
