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
(defun serika-f/ycmd/activate ()
  "Activate ycmd mode in current buffer."
  (interactive)
  (ycmd-mode +1))

;; Init
(defun init ()
  "Configure ycmd"
  (serika-c/eg/add-install :type    'git
                           :name    'emacs-ycmd
                           :src     "https://github.com/abingham/emacs-ycmd"
                           :parents '("install ycmd"))

  (serika-c/eg/add-install :type 'package
                           :name 'emacs-ycmd
                           :package-list '(request
                                           request-deferred))

  (serika-c/eg/add-many-by-name 'ycmd
                                ("require")
                                (func/func/require 'ycmd
                                                   'company-ycmd)

                                ("settings")
                                (lambda ()
                                  (setq ycmd-server-command '("python" "/home/yui/git_other/ycmd/ycmd"))
                                  (setq ycmd-global-config  (f-join serika-conf-directory
                                                                    ".global_config.py"))
                                  (setq ycmd-force-semantic-completion t)
                                  )))
