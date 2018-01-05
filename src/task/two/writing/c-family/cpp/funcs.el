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

;; Funcs
(defun serika-f/c++/setup-buffer ()
  "Setup `c++-mode' buffer"
  (when (eq major-mode 'c++-mode)
    (setq tab-width      4
          truncate-lines t)

    (func/var/ensure-local c-default-style "linux"
                           c-basic-offset  4)

    (serika-f/evil/activate :evil-shift-width 4
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)
    (serika-f/yasnippet/activate)

    (serika-f/ycmd/activate)
    (serika-f/company/activate :backends-set '(company-ycmd))
    (serika-f/flycheck/activate)
    (flycheck-ycmd-setup)

    (serika-f/eldoc/activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)))

;; Init
(defun init ()
  "Configure Emacs for editing c++-files."
  (serika-c/eg/add-many-by-name 'cpp
    ("settings")
    (serika-f/settings/register-ft 'c++-mode
                                   "\\.cpp$"
                                   "\\.h$")

    ("settings smartparens")
    (progn
      (sp-local-pair 'c++-mode "("    ")")
      (sp-local-pair 'c++-mode "{"    "}")
      (sp-local-pair 'c++-mode "["    "]")
      (sp-local-pair 'c++-mode "\""   "\"")
      (sp-local-pair 'c++-mode "'"    "'")
      (sp-local-pair 'c++-mode "\\\"" "\\\"")
      (sp-local-pair 'c++-mode "\\'"  "\\'"))

    ("keymap")
    (progn
      (func/keymap/save   c++-mode-map)
      (func/keymap/create c++-mode-map
        "TAB" #'yas-expand

        "C-t =" #'evil-indent
        "C-t /" #'evilnc-comment-or-uncomment-lines

        "C-c a a" #'dumb-jump-go
        "C-c a A" #'dumb-jump-back
        "C-c a r" #'ff-find-other-file))

    ("hook")
    (func/hook/add 'c++-mode-hook
                   #'serika-f/c++/setup-buffer)))
