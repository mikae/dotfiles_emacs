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

(defun serika-f/rust//setup-buffer ()
  "Setup rust buffers"
  (when (eq major-mode
            'rust-mode)
    ;; (func/var/ensure-local)
    (serika-f/evil/activate :evil-state       'normal
                            :evil-shift-width 4)

    (serika-f/smartparens/activate)
    (serika-f/aggressive-indent/activate)
    (serika-f/yasnippet/activate)
    (serika-f/company/activate :backends-set '((company-racer)
                                               (company-files
                                                company-yasnippet)))

    (serika-f/flycheck/activate :eval '(flycheck-rust-setup))

    (serika-f/eldoc/activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)))

(defun init ()
  "Configure Emacs for editing Rust files."
  (dolist (elem '((rust-mode     . "https://github.com/mikae/rust-mode")
                  (flycheck-rust . "https://github.com/mikae/flycheck-rust")
                  (emacs-racer   . "https://github.com/mikae/emacs-racer")
                  (company-racer . "https://github.com/mikae/company-racer")))
    (serika-c/eg/add-install :type    'git
                             :name    (car elem)
                             :src     (cdr elem)
                             :parents '("install rust")))

  (serika-c/eg/add-many-by-name 'rust
                                ("require")
                                (func/func/require 'rust-mode
                                                   'flycheck-rust
                                                   'racer
                                                   'company-racer)

                                ("settings")
                                (serika-f/settings/register-ft 'rust-mode
                                                               "\\.rs\\'")

                                ("settings smartparens")
                                (lambda ()
                                  (sp-local-pair 'rust-mode "("    ")")
                                  (sp-local-pair 'rust-mode "{"    "}")
                                  (sp-local-pair 'rust-mode "["    "]")
                                  (sp-local-pair 'rust-mode "\""   "\"")
                                  (sp-local-pair 'rust-mode "'"    "'")
                                  (sp-local-pair 'rust-mode "\\\"" "\\\"")
                                  (sp-local-pair 'rust-mode "\\'"  "\\'"))

                                ("settings multi-compile")
                                (serika-f/multi-compile/configure 'rust-mode
                                                                  "rust-debug"   "cargo run"
                                                                  "rust-release" "cargo run --release"
                                                                  "rest-test"    "cargo test")
                                ("keymap")
                                (progn
                                  (func/keymap/save   rust-mode-map)
                                  (func/keymap/create rust-mode-map
                                                      "TAB" #'yas-expand

                                                      "C-t =" #'evil-indent
                                                      "C-t /" #'evilnc-comment-or-uncomment-lines
                                                      "C-t b" #'rust-format-buffer

                                                      ;; arstd
                                                      "C-c a a" #'dumb-jump-go
                                                      "C-c a A" #'dumb-jump-back
                                                      "C-c z z" #'multi-compile-run))

                                ("hook")
                                (func/hook/add 'rust-mode-hook
                                               #'serika-f/rust//setup-buffer)))
