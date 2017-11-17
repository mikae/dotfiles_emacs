;; funcs.el --- `custom-modifiers.el' configuration -*- lexical-binding: t -*-
;;
;; Author: Minae Yui <minae.yui.sain@gmail.com>
;; Version: 0.1
;; URL: todo
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
(defun serika-f/custom-modifiers/activate ()
  "Activate custom modifiers."
  (interactive)
  (custom-modifiers-mode +1))

(defun serika-f/custom-modifiers/toggle ()
  "Activate custom modifiers."
  (interactive)
  (custom-modifiers-mode (if custom-modifiers-mode
                             -1
                           +1)))

(defun init ()
  "Configure custom modifiers"
  (serika-c/eg/add-install :type    'git
                           :name    'custom-modifiers
                           :src     "https://github.com/mikae/custom-modifiers"
                           :parents '("zero lib install"))

  (serika-c/eg/add-many-by-name 'custom-modifiers
    ("zero lib require")
    (lambda ()
      (require 'custom-modifiers)
      (require 'custom-modifiers-layout))

    ("settings")
    (lambda ()
      (custom-modifiers-layout-use-colemak)
      (custom-modifiers-modifiers-use-default)
      (custom-modifiers-global-map-configure)

      (cm-define-global "C-q"   #'func/buffer/kill)
      (cm-define-global "C-w"   #'custom-modifiers-command-loop-end)

      (cm-define-global "ESC-q" #'save-buffers-kill-emacs))

    ;;("post activate")
    ;;(serika-f/custom-modifiers/activate)
    ))
