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

(defun serika-f/cucumber/setup-buffer ()
  "Configure `cucumber' buffers."
  (when (eq major-mode
            'feature-mode)
    (func/var/ensure-local tab-width      2
                           truncate-lines t)
    (serika-f/evil/activate :evil-state 'normal
                            :evil-shift-width 2)

    (serika-f/smartparens/activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)))

(defun init ()
  "Configure `cucumber'."
  (serika-c/eg/add-install :type 'git
                           :name 'cucumber
                           :src "https://github.com/mikae/cucumber.el")

  (serika-c/eg/add-many-by-name 'cucumber
                                ("require")
                                (func/func/require 'feature-mode)

                                ("settings")
                                (lambda ()
                                  (serika-f/settings/register-ft 'feature-mode
                                                                 "\\.feature\\'"))

                                ("settings smartparens")
                                (lambda ()
                                  (sp-local-pair 'emacs-lisp-mode "("  ")")
                                  (sp-local-pair 'emacs-lisp-mode "{"  "}")
                                  (sp-local-pair 'emacs-lisp-mode "["  "]")
                                  (sp-local-pair 'emacs-lisp-mode "\"" "\""))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/save   feature-mode-map)
                                  (func/keymap/create feature-mode-map))

                                ("hook")
                                (lambda ()
                                  (func/hook/add 'feature-mode-hook
                                                 #'serika-f/cucumber/setup-buffer))))
