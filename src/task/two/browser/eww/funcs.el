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


(defun serika-f/eww/setup-buffer ()
  "Configure `eww' buffers."
  (when (eq major-mode
            'eww-mode)
    (serika-f/evil/activate :evil-state 'eww)))

(defun init ()
  "Configure `eww'."
  (serika-c/eg/add-many-by-name 'eww
                                ("require")
                                (func/func/require 'eww)

                                ("settings")
                                (lambda ()
                                  )

                                ("settings evil")
                                (lambda ()
                                  (evil-define-state eww
                                    "State for Emacs Web Wowser."
                                    :tag "<eww>"
                                    :suppress-keymap t))

                                ("keymap")
                                (lambda ()
                                  ;; (func/keymap/save hackernews-map)
                                  ;; (func/keymap/save hackernews-mode-map)
                                  ;; (func/keymap/create hackernews-map)
                                  ;; (func/keymap/create hackernews-mode-map)

                                  (func/keymap/define evil-eww-state-map
                                                      ))

                                ("global-keymap")
                                (lambda ()
                                  )

                                ("hook")
                                (lambda ()
                                  (func/hook/add 'eww-mode-hook
                                                 #'serika-f/ewwsetup-buffer))))
