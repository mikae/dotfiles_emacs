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

(defun serika-f/graphviz-dot/setup-buffer ()
  "Setup dot buffer."
  (when (eq major-mode
            'graphviz-dot-mode)
    (func/var/ensure-local tab-width      2
                           truncate-lines t)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/smartparens/activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)))

(defun init ()
  "Configure Emacs for writing dot files."
  (serika-c/eg/add-install :type 'git
                           :name 'dot
                           :src  "https://github.com/shinkiley/graphviz-dot-mode")

  (serika-c/eg/add-many-by-name 'dot
    ("require")
    (func/func/require 'graphviz-dot-mode)

    ("settings")
    (progn
      (serika-f/settings/register-ft #'graphviz-dot-mode
                                     "\\.dot\\'"))

    ("keymap")
    (progn
      (func/keymap/save graphviz-dot-mode-map)
      (func/keymap/create graphviz-dot-mode-map))

    ("hook")
    (func/hook/add 'graphviz-dot-mode-hook
                   #'serika-f/graphviz-dot/setup-buffer)))
