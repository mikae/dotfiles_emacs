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

(defun serika-f/nasm/setup-buffer ()
  "Setup buffers for `nasm-mode'"
  (when (func/buffer/check-modes 'nasm-mode)
    (setq tab-width      4
          truncate-lines t)

    (serika-f/evil/activate :evil-shift-width 4
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)

    (serika-f/yasnippet/activate)
    (serika-f/flycheck/activate)
    (serika-f/eldoc/activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)))

(defun init ()
  "Configure Emacs for NASM."
  (serika-c/eg/add-install :type 'git
                           :name 'nasm-mode
                           :src  "https://github.com/shinkiley/nasm-mode")

  (serika-c/eg/add-many-by-name 'nasm
    ("require")
    (func/func/require 'nasm-mode)

    ("settings")
    (serika-f/settings/register-ft 'nasm-mode
                                   "\\.nasm\\'")

    ("keymap")
    (progn
      (func/keymap/save nasm-mode-map)
      (func/keymap/create nasm-mode-map
        "TAB" #'yas-expand

        "C-t =" #'evil-indent
        "C-t /" #'evilnc-comment-or-uncomment-lines
        ))

    ("hook")
    (func/hook/add 'nasm-mode-hook
                   #'serika-f/nasm/setup-buffer)))
