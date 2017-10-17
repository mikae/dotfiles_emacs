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

;; Setup buffer
(defun serika-f/vimrc/setup-buffer ()
  "Setup vimrc buffer."
  (when (eq major-mode
            'vimrc-mode)
    (func/var/ensure-local tab-width      2
                           truncate-lines t)
    (serika-f/evil/activate :evil-shift-width 2
                            :evil-state       'normal)

    (serika-f/smartparens/activate)
    (serika-f/yasnippet/activate)

    (serika-f/settings/show-trailing-whitespaces)
    (serika-f/linum-relative/activate)
    (serika-f/rainbow-delimiters/activate)
    (serika-f/highlight-symbol/activate)
    ;; (serika-f/prettify-symbols/activate :name "vimrc")
    ))

(defun init ()
  "Configure `vimrc-mode'."
  (serika-c/eg/add-install :type 'git
                           :name 'vimrc-mode
                           :src  "https://github.com/mikae/vimrc-mode")

  (serika-c/eg/add-many-by-name 'vimrc-mode
                                ("require")
                                (func/func/requirer 'vimrc-mode)

                                ("settings")
                                (serika-f/settings/register-ft 'vimrc-mode
                                                               "\\.vim\\(rc\\)?\\'"
                                                               "\\.nvim\\(rc\\)?\\'"
                                                               "\\.cvim\\(rc\\)?\\'")

                                ("keymap")
                                (lambda ()
                                  (func/keymap/save   vimrc-mode-map)
                                  (func/keymap/create vimrc-mode-map))

                                ("hook")
                                (lambda ()
                                  (func/hook/add 'vimrc-mode-hook
                                                 #'serika-f/vimrc/setup-buffer))))
