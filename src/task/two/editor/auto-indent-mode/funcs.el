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

;; (cl-defun serika-f/auto-indent-mode/activate (&key (style 'conservative style-p))
;;   "Activate auto indent mode."
;;   (when style-p
;;     (func/var/ensure-local auto-indent-indent-style style))
;;   (auto-indent-mode +1))

;; (defun init ()
;;   "Configure `auto-indent-mode'."
;;   (serika-c/eg/add-install :type 'git
;;                            :name 'auto-indent-mode
;;                            :src "https://github.com/mikae/auto-indent-mode.el")

;;   (serika-c/eg/add-many-by-name 'auto-indent-mode
;;     ("require")
;;     (func/func/require 'auto-indent-mode)

;;     ("settings")
;;     ()

;;     ("global-keymap")
;;     (func/keymap/define-global "C-x t i" #'auto-indent-mode)))
