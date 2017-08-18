;;; package --- Summary
;;; Commentary:
;;; Code:

;; Local
(defun serika-l/markdown//evil ()
  "Configure `evil' for `markdown-mode'."
  (setq evil-shift-width 4)
  (evil-local-mode +1)
  (evil-normal-state))

(defun serika-l/markdown//interface ()
  "Configure interface for `markdown-mode'."
  (setq show-trailing-whitespace t)
  (serika-f/linum-relative/activate))

(defun serika-l/markdown//buffer-local-variables ()
  "Configure buffer-local variables for `markdown-mode'."
  (setq tab-width 4))

;; Init
(defun init ()
  "Configure `markdown'."
  (serika-c/eg/add-install :package-list '(markdown-mode markdown-mode+)
                           :name         'markdown)

  (serika-c/eg/add-many-by-name
   'markdown
   ("require")
   (lambda ()
     (require 'markdown-mode))

   ("settings")
   (lambda ()
     (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
     (add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))
     (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

   ("keymap")
   (lambda ()
     (cl-macrolet ((--setup-markdown-keymap
                    (map)
                    `(progn
                       (setq ,map (make-sparse-keymap))

                       ;; `Anchors'
                       (define-key ,map (kbd "C-c a l")    #'markdown-insert-link)
                       (define-key ,map (kbd "C-c a u")    #'markdown-insert-uri)
                       (define-key ,map (kbd "C-c a f")    #'markdown-insert-footnote)
                       (define-key ,map (kbd "C-c a w")    #'markdown-insert-wiki-link)

                       ;; `image'
                       (define-key ,map (kbd "C-c i i")    #'markdown-insert-image)
                       (define-key ,map (kbd "C-c i t")    #'markdown-toggle-inline-images)

                       ;; `style'
                       (define-key ,map (kbd "C-c s e")    #'markdown-insert-italic)
                       (define-key ,map (kbd "C-c s b")    #'markdown-insert-bold)
                       (define-key ,map (kbd "C-c s c")    #'markdown-insert-code)
                       (define-key ,map (kbd "C-c s C")    #'markdown-insert-gfm-code-block)
                       (define-key ,map (kbd "C-c s -")    #'markdown-insert-strike-through)

                       ;; `hr'
                       (define-key ,map (kbd "C-c -")      #'markdown-insert-hr)

                       ;; `headers',
                       (define-key ,map (kbd "C-c t !")    #'markdown-insert-header-setext-1)
                       (define-key ,map (kbd "C-c t @")    #'markdown-insert-header-setext-2)
                       (define-key ,map (kbd "C-c t 1")    #'markdown-insert-header-atx-1)
                       (define-key ,map (kbd "C-c t 2")    #'markdown-insert-header-atx-2)
                       (define-key ,map (kbd "C-c t 3")    #'markdown-insert-header-atx-3)
                       (define-key ,map (kbd "C-c t 4")    #'markdown-insert-header-atx-4)
                       (define-key ,map (kbd "C-c t 5")    #'markdown-insert-header-atx-5)
                       (define-key ,map (kbd "C-c t 6")    #'markdown-insert-header-atx-6)

                       ;; `regions',
                       (define-key ,map (kbd "C-c r p")    #'markdown-pre-region)
                       (define-key ,map (kbd "C-c r q")    #'markdown-blockquote-region)

                       ;; `compile',
                       (define-key ,map (kbd "C-c c b")    #'markdown-other-window)
                       (define-key ,map (kbd "C-c c p")    #'markdown-preview)
                       (define-key ,map (kbd "C-c c e")    #'markdown-export)
                       (define-key ,map (kbd "C-c c v")    #'markdown-export-and-preview)
                       (define-key ,map (kbd "C-c c l")    #'markdown-live-preview-mode)

                       ;; `open'
                       (define-key ,map (kbd "C-c o")      #'markdown-follow-thing-at-point)

                       ;; `promotion'
                       (define-key ,map (kbd "C-c -")      #'markdown-promote)
                       (define-key ,map (kbd "C-c =")      #'markdown-demote)

                       ;; `completion'
                       (define-key ,map (kbd "C-c RET")    #'markdown-complete)

                       ;; `list'
                       (define-key ,map (kbd "<C-return>") #'markdown-insert-list-item)

                       ;; `kill'
                       (define-key ,map (kbd "C-c k")      #'markdown-kill-thing-at-point))))

       (func/keymap/save markdown-mode-map
                             gfm-mode-map)

       ;; Use new keymaps
       (--setup-markdown-keymap markdown-mode-map)
       (--setup-markdown-keymap gfm-mode-map)))

   ("hook")
   (lambda ()
     (add-hook 'markdown-mode-hook #'serika-l/markdown//evil)
     (add-hook 'markdown-mode-hook #'serika-l/markdown//buffer-local-variables)
     (add-hook 'markdown-mode-hook #'serika-l/markdown//interface
               ))))
