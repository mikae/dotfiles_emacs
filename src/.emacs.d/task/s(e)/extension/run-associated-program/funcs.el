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

(defvar serika-run-associated-program-alist
  '(("mp4" . "smplayer")
    ("mkv" . "smplayer"))
  "Alist for `run-associated-program-alist'.")

(defun init ()
  "Configure `run-associated-program'."
  (serika-c/eg/add-install :type 'git
                           :name 'run-associated-program
                           :src  "https://github.com/shinkiley/emacs-run-associated-program")

  (serika-c/eg/add-many-by-name 'run-associated-program
    ("require")
    (func/func/require 'run-associated-program)

    ("settings")
    (progn
      (cl-loop for (ext . program) in serika-run-associated-program-alist
               do
               (run-associated-program-register ext program)))))
