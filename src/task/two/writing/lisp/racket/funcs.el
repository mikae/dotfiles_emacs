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

(defun serika-f/racket//setup-buffer ()
  "Setup racket buffers"
  (when (eq major-mode
            'racket-mode)
    (func/var/ensure-local tab-width 2
                           truncate-lines t)
    (serika-f/evil/activate :evil-state       'normal
                            :evil-shift-width 2)
    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)

    (serika-f/flycheck/activate)
    (serika-f/yasnippet/activate)
    (serika-f/company/activate)

    (serika-f/prettify-symbols/activate :name "racket")
    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)))

(defun init ()
  "Configure `racket'."
  (serika-c/eg/add-install :type 'git
                           :name 'racket
                           :src  "https://github.com/mikae/racket-mode")

  (serika-c/eg/add-many-by-name 'racket
                                ("require")
                                (lambda ()
                                  (require 'racket-mode))

                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'racket-mode "\\.rkt\\'"))

                                ("settings smartparens")
                                (lambda ()
                                  (sp-local-pair 'racket-mode "("    ")")
                                  (sp-local-pair 'racket-mode "{"    "}")
                                  (sp-local-pair 'racket-mode "["    "]")
                                  (sp-local-pair 'racket-mode "\""   "\"")
                                  (sp-local-pair 'racket-mode "`"    "'")
                                  (sp-local-pair 'racket-mode "\\\"" "\\\""))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/save racket-mode-map)
                                  (func/keymap/create racket-mode-map
                                                      "TAB"   #'yas-expand

                                                      "C-c a"   #'racket-run
                                                      "C-c A"   #'racket-repl
                                                      "C-c r"   #'racket-send-last-sexp
                                                      "C-c R"   #'racket-send-region
                                                      "C-c C-r" #'racket-send-definition

                                                      "C-t ="   #'evil-indent
                                                      "C-t /"   #'evilnc-comment-or-uncomment-lines)

                                  ;; (func/keymap/save racket-repl-mode-map)
                                  (func/keymap/define racket-repl-mode-map
                                                      ;; "RET"         #'racket-repl-eval-or-newline-and-indent
                                                      ;; "<backspace>" #'ignore

                                                      ;; "C-c C-c"     #'comint-interrupt-subjob

                                                      "A-e" #'comint-next-input
                                                      "A-i" #'comint-previous-input
                                                      "A-n" #'backward-char
                                                      "A-o" #'forward-char)
                                  )

                                ("hook")
                                (lambda ()
                                  (func/hook/add 'racket-mode-hook
                                                 #'serika-f/racket//setup-buffer))))
