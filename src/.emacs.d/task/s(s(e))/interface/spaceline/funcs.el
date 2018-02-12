;; funcs.el --- 
;;
;; Author:  <yui@yui-pc>
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

(defmacro serika-f/spaceline/compile (name left right)
  "Compile spaceline modeline."
  `(spaceline-compile ',name
     ',left
     ',right))

(defmacro serika-f/spaceline/compile-default-with-extras (name &optional extra-left extra-right)
  "Compile default spaceline modeline with extra segments."
  `(serika-f/spaceline/compile ,name
     (buffer-id
      (buffer-encoding-abbrev
       point-position
       line-column)
      buffer-modified
      buffer-position
      evil-state
      anzu
      ,@extra-left)

     (org-pomodoro
      version-control
      major-mode
      ,@extra-right)))

(defun serika-f/spaceline/compile-default ()
  "Compile default spaceline modeline."
  (serika-f/spaceline/compile-default-with-extras main))

(defun init ()
  "Configure `spaceline'."
  (dolist (--package '((powerline . "https://github.com/shinkiley/powerline")
                       (spaceline . "https://github.com/shinkiley/spaceline")))
    (serika-c/eg/add-install :type 'git
                             :name (car --package)
                             :src  (cdr --package)
                             :parents '("install spaceline")))

  (serika-c/eg/add-many-by-name 'spaceline
    ("require")
    (func/func/require 'spaceline
                       'spaceline-segments)

    ("settings")
    (progn
      (setq spaceline-face-func
            (lambda (face active)
              (if active
                  (cond
                   ((eq face 'face1)
                    'powerline-active1)
                   ((eq face 'face2)
                    'powerline-active2)
                   ((eq face 'line)
                    'mode-line)
                   ((eq face 'highlight)
                    'powerline-active2))
                (cond
                 ((eq face 'face1)
                  'powerline-inactive1)
                 ((eq face 'face2)
                  'powerline-inactive2)
                 ((eq face 'line)
                  'mode-line)
                 ((eq face 'highlight)
                  'powerline-inactive2)))))

      (serika-f/spaceline/compile-default)

      (setq-default mode-line-format
                    '("%e" (:eval (spaceline-ml-main)))))))

(put 'serika-f/spaceline/compile 'lisp-indent-function 'defun)
(put 'serika-f/spaceline/compile-default-with-extras 'lisp-indent-function 'defun)

;;; funcs.el ends here
