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

;; Global
(defun serika-g/mingus//require ()
  "Require modules for `mingus'."
  (require 'mingus))

(defun serika-g/mingus//settings ()
  "Configure `mingus' settings."
  (mingus-set-host 'mingus-mpd-host "localhost")
  (mingus-set-port 'mingus-mpd-port 7701)

  (setq mingus-set-amount 10))

(defun serika-g/mingus//keymap ()
  "Configure `mingus' keymaps."
  (cl-flet ((configure-keymap (map)
                (define-key map (kbd "1") 'mingus-help)
                (define-key map (kbd "2") 'mingus)
                (define-key map (kbd "3") 'mingus-browse)

                (define-key map (kbd "r") 'mingus-repeat)
                (define-key map (kbd "y") 'mingus-single)
                (define-key map (kbd "z") 'mingus-random)

                (define-key map (kbd "p") 'mingus-pause)

                (define-key map (kbd ",") 'serika-f/mingus/seek-backward)
                (define-key map (kbd ".") 'serika-f/mingus/seek-forward)

                (define-key map (kbd "j") 'evil-next-visual-line)
                (define-key map (kbd "k") 'evil-previous-visual-line)

                (define-key map (kbd "c") 'serika-f/mingus/clear)
                (define-key map (kbd "q") 'mingus-git-out)

                (define-key map (kbd "u") 'mingus-update)

                (define-key map (kbd "/") 'evil-search-forward)
                (define-key map (kbd "?") 'evil-search-backward)
                (define-key map (kbd "n") 'evil-search-next)
                (define-key map (kbd "N") 'evil-search-previous)
                ()))
    ;; `mingus-help-map'
    (setq mingus-help-map (make-sparse-keymap))
    (configure-keymap mingus-help-map)

    (define-key mingus-help-map (kbd "h") 'evil-backward-char)
    (define-key mingus-help-map (kbd "l") 'evil-forward-char)

    ;; `mingus-playlist-map'
    (setq mingus-playlist-map (make-sparse-keymap))
    (configure-keymap mingus-playlist-map)

    (define-key mingus-playlist-map (kbd "RET") 'mingus-play)
    (define-key mingus-playlist-map (kbd "s")   (lambda () (interactive) (mingus-shuffle)))
    (define-key mingus-playlist-map (kbd "d")   'mingus-del-dwim2)

    ;; `mingus-browse-map'
    (setq mingus-browse-map (make-sparse-keymap))
    (configure-keymap mingus-browse-map)

    (define-key mingus-browse-map (kbd "a")   'mingus-insert)

    (define-key mingus-browse-map (kbd "h")   'mingus-open-parent)
    (define-key mingus-browse-map (kbd "l")   'mingus-down-dir-or-play-song)
    (define-key mingus-browse-map (kbd "RET") 'mingus-insert-and-play)

    (setq mingus-global-map (make-sparse-keymap))))

(defun serika-g/mingus//global-keymap ()
  "Configure global keymap to invoke `mingus'."
  (global-set-key (kbd "<C-m> m") 'mingus))

(defun serika-g/mingus//hook ()
  "Configure `mingus' hooks."
  (add-hook 'mingus-playlist-hooks (lambda ()
                                     (auto-revert-mode 1))))

(defun init ()
  "Configure `mingus'."
  (serika-c/eg/add-install :package-list '(mingus)
                           :name         'mingus)

  (serika-c/eg/add :parents '("require")
                   :name    'mingus
                   :func    #'serika-g/mingus//require)

  (serika-c/eg/add :parents '("settings")
                   :name    'mingus
                   :func    #'serika-g/mingus//settings)

  (serika-c/eg/add :parents '("keymap")
                   :name    'mingus
                   :func    #'serika-g/mingus//keymap)

  (serika-c/eg/add :parents '("global-keymap")
                   :name    'mingus
                   :func    #'serika-g/mingus//global-keymap)

  (serika-c/eg/add :parents '("hook")
                   :name    'mingus
                   :func    #'serika-g/mingus//hook))