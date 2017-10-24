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

(defun serika-f/hackernews/setup-buffer ()
  "Configure `hackernews' buffers."
  (when (eq major-mode
            'hackernews-mode)
    (serika-f/evil/activate :evil-state 'hackernews)))

;; Init
(defun init ()
  "Configure `hackernews'."
  (serika-c/eg/add-install :type 'git
                           :name 'hackernews
                           :src  "https://github.com/mikae/hackernews.el")

  (serika-c/eg/add-many-by-name 'hackernews
                                ("require")
                                (func/func/require 'hackernews)

                                ("settings")
                                (lambda ()
                                  )

                                ("settings evil")
                                (lambda ()
                                  (evil-define-state hackernews
                                    "State for hackernews."
                                    :tag "<HackerNews>"
                                    :suppress-keymap t))

                                ("keymap")
                                (lambda ()
                                  (func/keymap/save hackernews-map)
                                  (func/keymap/save hackernews-mode-map)
                                  (func/keymap/create hackernews-map)
                                  (func/keymap/create hackernews-mode-map)

                                  (func/keymap/define evil-hackernews-state-map
                                                      ;; qwfpg
                                                      "C-q" #'func/buffer/kill
                                                      "q" #'hackernews-reload
                                                      "Q" #'hackernews-switch-feed
                                                      "w" #'hackernews-load-more-stories

                                                      ;; arstd
                                                      "a" #'hackernews-top-stories
                                                      "A" #'hackernews-new-stories
                                                      "r" #'hackernews-best-stories
                                                      "R" #'hackernews-show-stories
                                                      "s" #'hackernews-ask-stories
                                                      "S" #'hackernews-job-stories

                                                      ;;neio'
                                                      "e" #'hackernews-next-item
                                                      "i" #'hackernews-previous-item
                                                      "E" #'scroll-down-command
                                                      "I" #'scroll-up-command

                                                      "A-e" #'hackernews-next-comment
                                                      "A-E" #'hackernews-next-item

                                                      ;; km,/.
                                                      "?" #'describe-mode

                                                      ;; RET
                                                      ;; "RET" #'hackernews-button-browse-internal
                                                      ))

                                ("global-keymap")
                                (lambda ()
                                  (func/keymap/define-global "C-x C-f h" 'hackernews))

                                ("hook")
                                (lambda ()
                                  (func/hook/add 'hackernews-mode-hook
                                                 #'serika-f/hackernews/setup-buffer))))
