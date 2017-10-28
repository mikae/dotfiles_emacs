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

;; Functions
(defun serika-f/tex/setup-buffer ()
  "Configure tex buffers"
  (when (func/buffer/check-modes 'latex-mode)
    (func/var/ensure-local tab-width 4)

    (serika-f/evil/activate :evil-shift-width 4
                            :evil-state 'normal)

    (serika-f/company/activate :backends-set '(company-auctex))
    (serika-f/flycheck/activate)

    (serika-f/linum-relative/activate)))

;; Init
(defun init ()
  "Configure Emacs for editing TeX files."
  (serika-c/eg/add-install :type 'package
                           :name 'tex
                           :package-list '(auctex
                                           company-auctex))

  (serika-c/eg/add-many-by-name 'tex
                                ("require")
                                (progn
                                  ;; todo: change this
                                  (load "~/.emacs.d/elpa/auctex-11.91.0/auctex.el")
                                  (load "~/.emacs.d/elpa/auctex-11.91.0/preview.el")
                                  (func/func/require 'company-auctex))

                                ("settings")
                                (progn
                                  (serika-f/settings/register-ft 'TeX-mode
                                                                 "\\.tex\\'")
                                  (setq TeX-auto-save  t
                                        TeX-parse-self t
                                        TeX-save-query nil)

                                  )

                                ("hook")
                                (func/hook/add 'TeX-mode-hook
                                               #'serika-f/tex/setup-buffer)))
