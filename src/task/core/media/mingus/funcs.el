;;; package --- Summary
;;; Commentary:
;;; Code:

;; Public
(defun serika-f/mingus/seek-forward (&optional count)
  "Seek forward `COUNT' seconds."
  (interactive)
  (mingus-seek (or count 10)))

(defun serika-f/mingus/seek-backward (&optional count)
  "Seek backward `COUNT' seconds."
  (interactive)
  (mingus-seek (- (or count 10))))

(defun serika-f/mingus/clear ()
  "Clear playlist."
  (interactive)
  (mingus-clear t))

(defmacro serika-f/mingus/get-current-info (target-key)
  "Get info or current song in `mingus'."
  `(let ((--data (car (mingus-get-songs "currentsong"))))
     (getf --data ',target-key)))

(defun serika-f/mingus/get-current-pos ()
  "Get current song position."
  (let* ((data (mpd-get-status mpd-inter-conn))
         (time-total (plist-get data 'time-total))
         (time-elapsed (plist-get data 'time-elapsed)))
    time-elapsed))

;; Global
(defun init ()
  "Configure `mingus'."
  (serika-c/eg/add-install :type         'package
                           :package-list '(mingus)
                           :name         'mingus)

  (serika-c/eg/add-many-by-name 'mingus
                                ("require")
                                (lambda ()
                                  (require 'mingus))

                                ("settings")
                                (lambda ()
                                  (mingus-set-host 'mingus-mpd-host "localhost")
                                  (mingus-set-port 'mingus-mpd-port 7701)

                                  (setq mingus-set-amount 10))

                                ("settings spaceline")
                                (lambda ()
                                  (spaceline-define-segment mingus-title
                                    "The current song name."
                                    (or (serika-f/mingus/get-current-info Title)
                                        "Unknown title"))

                                  (spaceline-define-segment mingus-artist
                                    "The current song name."
                                    (or (serika-f/mingus/get-current-info Artist)
                                        "Unknown artist"))

                                  (spaceline-define-segment mingus-album
                                    "The current song name."
                                    (or (serika-f/mingus/get-current-info Album)
                                        "Unknown album"))

                                  (spaceline-define-segment mingus-length
                                    "The current song name."
                                    (format "%s-%s"
                                            (mingus-sec->min:sec (serika-f/mingus/get-current-pos))
                                            (mingus-sec->min:sec (serika-f/mingus/get-current-info Time))))

                                  (spaceline-compile 'mingus
                                                     '(mingus-artist
                                                       mingus-title
                                                       mingus-length)
                                                     '()))

                                ("keymap")
                                (lambda ()
                                  (cl-macrolet ((configure-keymap (map)
                                                                  `(func/keymap/save ,map)
                                                                  `(func/keymap/create ,map
                                                                                       "1"   #'mingus-help
                                                                                       "2"   #'mingus
                                                                                       "3"   #'mingus-browse

                                                                                       "r"   #'mingus-repeat
                                                                                       "y"   #'mingus-single
                                                                                       "z"   #'mingus-random

                                                                                       "p"   #'mingus-pause

                                                                                       ","   #'serika-f/mingus/seek-backward
                                                                                       "."   #'serika-f/mingus/seek-forward

                                                                                       "A-e" #'evil-next-visual-line
                                                                                       "A-i" #'evil-previous-visual-line

                                                                                       "c"   #'serika-f/mingus/clear
                                                                                       "q"   #'mingus-git-out

                                                                                       "u"   #'mingus-update

                                                                                       "A-1" #'evil-search-forward
                                                                                       "A-2" #'evil-search-backward
                                                                                       "A-z" #'evil-search-next
                                                                                       "A-Z" #'evil-search-previous)))
                                    ;; `mingus-help-map'
                                    (configure-keymap mingus-help-map)
                                    (func/keymap/define mingus-help-map
                                                        "A-n" #'evil-backward-char
                                                        "A-o" #'evil-forward-char)

                                    ;; `mingus-playlist-map'
                                    (configure-keymap mingus-playlist-map)
                                    (func/keymap/define mingus-playlist-map
                                                        "RET" #'mingus-play
                                                        "s"   (lambda () (interactive) (mingus-shuffle))
                                                        "d"   #'mingus-del-dwim2)

                                    ;; `mingus-browse-map'
                                    (configure-keymap mingus-browse-map)
                                    (func/keymap/define mingus-browse-map
                                                        "a"   'mingus-insert

                                                        "A-n" 'mingus-open-parent
                                                        "A-o" 'mingus-down-dir-or-play-song
                                                        "RET" 'mingus-insert-and-play)

                                    (func/keymap/save   mingus-global-map)
                                    (func/keymap/create mingus-global-map)))

                                ("global-keymap")
                                (lambda ()
                                  (global-set-key (kbd "<C-m> m") 'mingus))

                                ("hook")
                                (lambda ()
                                  (add-hook 'mingus-playlist-hooks (lambda ()
                                                                     (auto-revert-mode 1)))
                                  (func/hook/add 'mingus-playlist-hooks
                                                 (lambda ()
                                                   (setq mode-line-format
                                                         '("%e" (:eval (spaceline-ml-mingus)))))))))
