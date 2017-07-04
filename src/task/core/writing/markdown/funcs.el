;;; package --- Summary
;;; Commentary:
;;; Code:

;; Global
(defun serika/markdown//require ()
  "Require modules for `markdown'."
  (require 'markdown-mode))

(defun serika/markdown//auto-mode-alist ()
  "Configure `auto-mode-alist' for `markdown'."
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

(defun serika/markdown//keymap ()
  "Configure `markdown-mode-map'."
  ;; Links
  (let ((map (make-sparse-keymap)))
    ;; `Anchors'
    (define-key map (kbd "C-c a l")    #'markdown-insert-link)
    (define-key map (kbd "C-c a u")    #'markdown-insert-uri)
    (define-key map (kbd "C-c a f")    #'markdown-insert-footnote)
    (define-key map (kbd "C-c a w")    #'markdown-insert-wiki-link)

    ;; `image'
    (define-key map (kbd "C-c i i")    #'markdown-insert-image)
    (define-key map (kbd "C-c i t")    #'markdown-toggle-inline-images)

    ;; `style'
    (define-key map (kbd "C-c s e")    #'markdown-insert-italic)
    (define-key map (kbd "C-c s b")    #'markdown-insert-bold)
    (define-key map (kbd "C-c s c")    #'markdown-insert-code)
    (define-key map (kbd "C-c s C")    #'markdown-insert-gfm-code-block)
    (define-key map (kbd "C-c s -")    #'markdown-insert-strike-through)

    ;; `hr'
    (define-key map (kbd "C-c -")      #'markdown-insert-hr)

    ;; `headers'
    (define-key map (kbd "C-c t !")    #'markdown-insert-header-setext-1)
    (define-key map (kbd "C-c t @")    #'markdown-insert-header-setext-2)
    (define-key map (kbd "C-c t 1")    #'markdown-insert-header-atx-1)
    (define-key map (kbd "C-c t 2")    #'markdown-insert-header-atx-2)
    (define-key map (kbd "C-c t 3")    #'markdown-insert-header-atx-3)
    (define-key map (kbd "C-c t 4")    #'markdown-insert-header-atx-4)
    (define-key map (kbd "C-c t 5")    #'markdown-insert-header-atx-5)
    (define-key map (kbd "C-c t 6")    #'markdown-insert-header-atx-6)

    ;; `regions'
    (define-key map (kbd "C-c r p")    #'markdown-pre-region)
    (define-key map (kbd "C-c r q")    #'markdown-blockquote-region)

    ;; `compile'
    (define-key map (kbd "C-c c b")    #'markdown-other-window)
    (define-key map (kbd "C-c c p")    #'markdown-preview)
    (define-key map (kbd "C-c c e")    #'markdown-export)
    (define-key map (kbd "C-c c v")    #'markdown-export-and-preview)
    (define-key map (kbd "C-c c l")    #'markdown-live-preview-mode)

    ;; `open'
    (define-key map (kbd "C-c o")      #'markdown-follow-thing-at-point)

    ;; `promotion'
    (define-key map (kbd "C-c -")      #'markdown-promote)
    (define-key map (kbd "C-c =")      #'markdown-demote)

    ;; `completion'
    (define-key map (kbd "C-c RET")    #'markdown-complete)

    ;; `list'
    (define-key map (kbd "<C-return>") #'markdown-insert-list-item)

    ;; `kill'
    (define-key map (kbd "C-c k")      #'markdown-kill-thing-at-point)

    ;; Backup old keymaps
    (setq --serika-markdown-mode-map markdown-mode-map
          --serika-gfm-mode-map      gfm-mode-map)

    ;; Use new keymaps
    (setq markdown-mode-map map
          gfm-mode-map      map))
  )

;; Local
(defun serika/markdown//evil ()
  "Configure `evil' for `markdown-mode'."
  (setq evil-shift-width 4)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika/markdown//interface ()
  "Configure interface for `markdown-mode'."
  (setq show-trailing-whitespace t)
  (linum-mode  1))

(defun serika/markdown//buffer-local-variables ()
  "Configure buffer-local variables for `markdown-mode'."
  (setq tab-width 4))

;; Init
(defun init ()
  "Configure `markdown'."
  (serika/markdown//require)
  (serika/markdown//auto-mode-alist)
  (serika/markdown//keymap)

  (add-hook 'markdown-mode-hook #'serika/markdown//evil)
  (add-hook 'markdown-mode-hook #'serika/markdown//buffer-local-variables)
  (add-hook 'markdown-mode-hook #'serika/markdown//interface))
