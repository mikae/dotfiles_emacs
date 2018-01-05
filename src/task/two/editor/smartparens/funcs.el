;; funcs.el --- 
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

;; Functions
(defun serika-f/smartparens/activate ()
  "Enable `smartparens' in the current buffer."
  (smartparens-mode +1))

;; Init
(defun init ()
  "Configure smartparens."
  (serika-c/eg/add-install :type 'git
                           :name 'smartparens
                           :src  "https://github.com/shinkiley/smartparens")

  (serika-c/eg/add-many-by-name 'smartparens
    ("require")
    (func/func/require 'smartparens)

    ("settings")
    (progn
      (sp-pair "\\\\(" nil :actions :rem)
      (sp-pair "\\{"   nil :actions :rem)
      (sp-pair "\\("   nil :actions :rem)
      (sp-pair "\\\""  nil :actions :rem)
      (sp-pair "/*"    nil :actions :rem)
      (sp-pair "\""    nil :actions :rem)
      (sp-pair "'"     nil :actions :rem)
      (sp-pair "("     nil :actions :rem)
      (sp-pair "["     nil :actions :rem)
      (sp-pair "{"     nil :actions :rem)
      (sp-pair "`"     nil :actions :rem))))
