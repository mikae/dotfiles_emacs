;;; package --- Summary
;;; Commentary:
;;; Code:
;;; Todo:
;;; - setup org-mode integration

;; Global
(defun serika/epub//require ()
  "Require modules for `epub'."
  (require 'ereader))

(defun serika/epub//auto-mode-alist ()
  "Configure `auto-mode-alist'."
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . ereader-mode)))

(defun serika/epub//keymap ()
  "Configure `ereader-mode-map'."
  (let ((map (make-sparse-keymap)))
    ;; Searching
    (define-key map (kbd "/") 'evil-search-forward)
    (define-key map (kbd "?") 'evil-search-backward)
    (define-key map (kbd "n") 'evil-search-next)
    (define-key map (kbd "N") 'evil-search-previous)

    ;; Scrolling
    (define-key map (kbd "A-d") 'evil-scroll-down)
    (define-key map (kbd "A-u") 'evil-scroll-up)
    (define-key map (kbd "A-f") 'evil-scroll-page-down)
    (define-key map (kbd "A-b") 'evil-scroll-page-up)
    (define-key map (kbd "j")   'evil-scroll-down)
    (define-key map (kbd "k")   'evil-scroll-up)

    ;; Defaults
    (define-key map (kbd "C-c A") 'ereader-hide-annotation)
    (define-key map (kbd "C-c G") 'ereader-goto-chapter)
    (define-key map (kbd "C-c M") 'ereader-hide-all-annotations)
    (define-key map (kbd "C-c R") 'ereader-load-annotations)
    (define-key map (kbd "C-c a") 'ereader-show-annotation)
    (define-key map (kbd "C-c c") 'ereader-message-chapter)
    (define-key map (kbd "C-c g") 'ereader-goto-chapter)
    (define-key map (kbd "C-c m") 'ereader-show-all-annotations)
    ;; (define-key map (kbd "C-c l") 'org-store-link)

    ;; Save old keymap && use new keymap
    (setq --serika-ereader-mode-map ereader-mode-map)
    (setq ereader-mode-map map)
    ()))

;; Init
(defun init ()
  "Configure `epub'."
  (serika/epub//require)
  (serika/epub//auto-mode-alist)
  (serika/epub//keymap))
