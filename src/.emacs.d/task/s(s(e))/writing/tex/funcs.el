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
;; (defun serika-f/tex/indent-buffer ()
;;   "Indent buffer."
;;   (interactive)
;;   (indent-region (poin-min) (point-max)))

;; (defun serika-f/tex/setup-buffer ()
;;   "Configure tex buffers"
;;   (when (func/buffer/check-modes 'latex-mode)
;;     (func/var/ensure-local tab-width 4)

;;     (serika-f/evil/activate :evil-shift-width 4
;;                             :evil-state       'normal)

;;     (serika-f/company/activate :backends-set '(company-auctex))
;;     (serika-f/flycheck/activate)

;;     (serika-f/linum-relative/activate)))

;; ;; Init
;; (defun init ()
;;   "Configure Emacs for editing TeX files."
;;   (dolist (--package '((auctex         . "https://github.com/shinkiley/auctex")
;;                        (company-auctex . "https://github.com/shinkiley/company-auctex")))
;;     (serika-c/eg/add-install :type 'git
;;                              :name (car --package)
;;                              :src  (cdr --package)))

;;   (serika-c/eg/add-many-by-name 'tex
;;     ("require")
;;     (progn
;;       ;; todo: change this
;;       (load "~/.emacs.d/plugin/auctex/auctex.el")
;;       (load "~/.emacs.d/plugin/auctex/preview.el")
;;       (func/func/require 'company-auctex))

;;     ("settings")
;;     (progn
;;       (serika-f/settings/register-ft 'LaTeX-mode
;;                                      "\\.tex\\'")
;;       (setq TeX-auto-save  t
;;             TeX-parse-self t
;;             TeX-save-query nil))

;;     ;; ("keymap")
;;     ;; (progn
;;     ;;   (func/keymap/save latex-mode-map)
;;     ;;   (func/keymap/create LaTeX-mode-map
;;     ;;                       "C-t ="   #'LaTeX-indent-line
;;     ;;                       "C-t C-=" #'serika-f/tex/indent-buffer))

;;     ("hook")
;;     (func/hook/add 'TeX-mode-hook
;;                    #'serika-f/tex/setup-buffer)))
