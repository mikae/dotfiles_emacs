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

(defun serika-f/fsharp/setup-buffer ()
  "Setup fsharp buffers"
  (when (func/buffer/check-modes 'fsharp-mode)
    (func/var/ensure-local tab-width            2
                           fsharp-indent-offset 2
                           truncate-lines       t)

    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)

    (serika-f/flycheck/activate)
    (serika-f/yasnippet/activate)
    (serika-f/company/activate)
    ;; (serika-f/eldoc/activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)
    ;; (serika-f/prettify-symbols/activate :name "fsharp")

    (call-interactively 'fsharp-ac/load-project)))

(defun init ()
  "Configure fsharp support"
  (serika-c/eg/add-install :type      'git
                           :name      'fsharp-mode
                           :src       "https://github.com/mikae/emacs-fsharp-mode")

  (serika-c/eg/add-many-by-name 'fsharp
    ("require")
    (func/func/require 'fsharp-mode)

    ("settings")
    (progn
      (serika-f/settings/register-ft 'fsharp-mode
                                     "\\\.fs[iylx]?$")

      (setq inferior-fsharp-program        "/usr/bin/fsharpi --readline-"
            fsharp-compiler                "/usr/bin/fsharpc"
            fsharp-ac-intellisence-enabled t)
      )

    ("keymap")
    (progn
      (func/keymap/save fsharp-mode-map)
      (func/keymap/create fsharp-mode-map
                          "TAB" #'yas-expand

                          "C-t =" #'evil-indent
                          "C-t /" #'evilnc-comment-or-uncomment-lines
                          ))

    ("hook")
    (progn
      (func/hook/add 'fsharp-mode-hook #'serika-f/fsharp/setup-buffer))
    ))
