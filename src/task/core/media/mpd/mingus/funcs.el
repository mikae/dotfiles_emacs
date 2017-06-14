;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'mingus)

(defun serika/mingus//settings ()
  "Configure `mingus' settings."
  (mingus-set-host 'mingus-mpd-host "localhost")
  (mingus-set-port 'mingus-mpd-port 7701)

  (setq mingus-set-amount 10))

(defun serika/mingus//keymap ()
  "Configure `mingus' keymaps."
  (let ((gm
         (lambda (map)
           (define-key map (kbd "1") 'mingus-help)
           (define-key map (kbd "2") 'mingus)
           (define-key map (kbd "3") 'mingus-browse)

           (define-key map (kbd "r") 'mingus-repeat)
           (define-key map (kbd "y") 'mingus-single)
           (define-key map (kbd "z") 'mingus-random)

           (define-key map (kbd "p") 'mingus-play)
           (define-key map (kbd "P") 'mingus-pause)

           (define-key map (kbd ",") (lambda () (interactive) (mingus-seek 10)))
           (define-key map (kbd ".") (lambda () (interactive) (mingus-seek -10)))

           (define-key map (kbd "j") 'evil-next-visual-line)
           (define-key map (kbd "k") 'evil-previous-visual-line)

           (define-key map (kbd "c") (lambda () (interactive) (mingus-clear t)))
           (define-key map (kbd "q") 'mingus-git-out)

           (define-key map (kbd "u") 'mingus-update)

           (define-key map (kbd "/") 'evil-search-forward)
           (define-key map (kbd "?") 'evil-search-backward)
           (define-key map (kbd "n") 'evil-search-next)
           (define-key map (kbd "N") 'evil-search-previous)
           ())
         ))
    (setq mingus-help-map (make-sparse-keymap))
    (funcall gm mingus-help-map)

    (setq mingus-playlist-map (make-sparse-keymap))
    (funcall gm mingus-playlist-map)

    (define-key mingus-playlist-map (kbd "RET") 'mingus-play)
    (define-key mingus-playlist-map (kbd "s")   (lambda () (interactive) (mingus-shuffle)))
    (define-key mingus-playlist-map (kbd "d")   'mingus-del-dwim2)

    (setq mingus-browse-map (make-sparse-keymap))
    (funcall gm mingus-browse-map)

    (define-key mingus-browse-map (kbd "a")   'mingus-insert)

    (define-key mingus-browse-map (kbd "h")   'mingus-open-parent)
    (define-key mingus-browse-map (kbd "l")   'mingus-down-dir-or-play-song)
    (define-key mingus-browse-map (kbd "RET") 'mingus-insert-and-play)

    (setq mingus-global-map (make-sparse-keymap))))

(defun serika/mingus//global-keymap ()
  "Configure global keymap to invoke `mingus'."
  (global-set-key (kbd "<C-m> m") 'mingus))

(defun serika/mingus//hook ()
  "Configure `mingus' hooks."
  (add-hook 'mingus-playlist-hooks (lambda ()
                                     (auto-revert-mode 1))))

(defun init ()
  "Configure `mingus'."
  (serika/mingus//settings)
  (serika/mingus//keymap)
  (serika/mingus//global-keymap)
  (serika/mingus//hook))
