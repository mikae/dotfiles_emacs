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

(defun serika-f/toml//setup-buffer ()
  "Setup buffer for `toml' modes."
  (when (eq major-mode
            'toml-mode)
    (func/var/ensure-local tab-width      2
                           truncate-lines t)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)

    (serika-f/flycheck/activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)))

(defun init ()
  "Configure Emacs for `toml' writing."
  (serika-c/eg/add-install :type    'git
                           :name    'toml-mode
                           :src     "https://github.com/shinkiley/toml-mode.el"
                           :parents '("install toml"))

  (serika-c/eg/add-many-by-name 'toml
    ("require")
    (func/func/require 'toml-mode)

    ("settings")
    (serika-f/settings/register-ft 'toml-mode
                                   "\\.toml\\'")

    ("settings smartparens")
    (progn
      (sp-local-pair 'toml-mode "("    ")")
      (sp-local-pair 'toml-mode "{"    "}")
      (sp-local-pair 'toml-mode "["    "]")
      (sp-local-pair 'toml-mode "\""   "\"")
      (sp-local-pair 'toml-mode "'"    "'")
      (sp-local-pair 'toml-mode "\\\"" "\\\"")
      (sp-local-pair 'toml-mode "\\'" "\\'"))

    ("keymap")
    (progn
      (func/keymap/save   toml-mode-map)
      (func/keymap/create toml-mode-map
        "C-t =" #'evil-indent
        "C-t /" #'evilnc-comment-or-uncomment-lines
        ))

    ("hook")
    (func/hook/add 'toml-mode-hook
                   #'serika-f/toml//setup-buffer)))
