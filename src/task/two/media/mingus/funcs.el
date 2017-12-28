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
    (progn
      (require 'mingus))

    ("settings")
    (progn
      (mingus-set-host 'mingus-mpd-host "localhost")
      (mingus-set-port 'mingus-mpd-port 7701)

      (setq mingus-set-amount 10))

    ("settings spaceline")
    (progn
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
    (progn
      (cl-macrolet ((configure-keymap (map)
                                      `(func/keymap/save ,map)
                                      `(func/keymap/create ,map
                                                           ;; arstd
                                                           "a a" #'mingus-del-dwim2
                                                           "a r" #'mingus-shuffle
                                                           "a s" #'serika-f/mingus/clear
                                                           "a t" #'mingus-update

                                                           "r a" #'mingus-repeat
                                                           "r r" #'mingus-single
                                                           "r s" #'mingus-random
                                                           "r t" #'mingus-pause

                                                           "s a" #'mingus-help
                                                           "s r" #'mingus
                                                           "s s" #'mingus-browse

                                                           ;; qwfpg
                                                           "t a" #'mingus-git-out

                                                           ;; km,./
                                                           ","   #'serika-f/mingus/seek-backward
                                                           "."   #'serika-f/mingus/seek-forward

                                                           ;; neio
                                                           "e"   #'evil-next-line
                                                           "i"   #'evil-previous-line

                                                           "A-1" #'evil-search-forward
                                                           "A-2" #'evil-search-backward
                                                           "A-z" #'evil-search-next
                                                           "A-Z" #'evil-search-previous)))
        ;; `mingus-help-map'
        (configure-keymap mingus-help-map)
        (func/keymap/define mingus-help-map
                            ;; neio
                            "n" #'backward-char
                            "o" #'forward-char)

        ;; `mingus-playlist-map'
        (configure-keymap mingus-playlist-map)
        (func/keymap/define mingus-playlist-map
                            "RET" #'mingus-play)

        ;; `mingus-browse-map'
        (configure-keymap mingus-browse-map)
        (func/keymap/define mingus-browse-map
                            "RET" #'mingus-insert-and-play

                            ;; qwfpg
                            "q q" #'mingus-insert

                            ;; neio
                            "n"   #'mingus-open-parent
                            "o"   #'mingus-down-dir-or-play-song)

        (func/keymap/save   mingus-global-map)
        (func/keymap/create mingus-global-map)))

    ("global-keymap")
    (func/keymap/define-global "<C-m> m" #'mingus)

    ("hook")
    (progn
      (add-hook 'mingus-playlist-hooks #'auto-revert-mode)
      (func/hook/add 'mingus-playlist-hooks
                     (lambda ()
                       (setq mode-line-format
                             '("%e" (:eval (spaceline-ml-mingus)))))))))
