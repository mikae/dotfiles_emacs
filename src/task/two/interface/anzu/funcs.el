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

(defun serika-f/anzu/activate-global ()
  "Activate `anzu-global-mode'."
  (global-anzu-mode +1))

(defun init ()
  "Configure `anzu'."
  (serika-c/eg/add-install :type 'git
                           :name 'anzu
                           :src  "https://github.com/mikae/emacs-anzu")

  (serika-c/eg/add-install :type 'git
                           :name 'evil-anzu
                           :parents '("install anzu")
                           :src  "https://github.com/mikae/emacs-evil-anzu")

  (serika-c/eg/add-many-by-name 'anzu
                                ("require")
                                (func/func/require 'anzu)

                                ("require evil")
                                (func/func/require 'evil-anzu)

                                ("settings")
                                (lambda ()
                                  (setq anzu-cons-mode-line-p nil))

                                ("post activate")
                                #'serika-f/anzu/activate-global))
