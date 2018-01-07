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

(defun init ()
  "Configure window behaviour in Emacs."
  (serika-c/eg/add-many-by-name 'windows
    ("global-keymap create-new")
    (func/keymap/define-global
      ;; built-in
      "C-x w a" #'split-window-right
      "C-x w A" #'split-window-below
      "C-x w r" #'delete-window
      "C-x w R" #'delete-other-windows

      ;; `windmove'
      "C-x w n" #'windmove-left
      "C-x w e" #'windmove-down
      "C-x w i" #'windmove-up
      "C-x w o" #'windmove-right

      ;; `buffer-move'
      "C-x w N" #'buf-move-left
      "C-x w E" #'buf-move-down
      "C-x w I" #'buf-move-up
      "C-x w O" #'buf-move-right)))
