;;; package --- Summary
;;; Commentary:
;;; Code:

;; Funcs
(defun serika-f/helm/activate ()
  "Activate `helm'."
  (helm-mode +1))

;; Init
(defun init ()
  "Configure `helm'."
  (serika-c/eg/add-install :type      'git
                           :name      'helm
                           :src       "https://github.com/shinkiley/helm"
                           :post-hook "make")

  (serika-c/eg/add-many-by-name 'helm
    ("require")
    (func/func/require 'helm-lib
                       'helm-config
                       'helm-source
                       'helm-buffers)

    ("settings")
    (progn
      (when (executable-find "curl")
        (setq helm-google-suggest-use-curl-p t))

      (setq helm-split-window-in-side-p           t
            helm-buffers-fuzzy-matching           t
            helm-move-to-line-cycle-in-source     t
            helm-ff-search-library-in-sexp        t
            helm-ff-file-name-history-use-recentf t))

    ("keymap")
    (progn
      (func/keymap/save   helm-map)
      (func/keymap/create helm-map
        "A-e" #'helm-next-line
        "A-i" #'helm-previous-line
        "A-E" #'helm-next-source
        "A-I" #'helm-previous-source

        "C-g" #'helm-keyboard-quit

        "RET" #'helm-maybe-exit-minibuffer
        "TAB" #'helm-select-action)

      (func/keymap/save   helm-buffer-map)
      (func/keymap/create helm-buffer-map))

    ("global-keymap")
    (func/keymap/define-global
      "M-a"     #'helm-M-x
      "C-x f"   #'helm-find-files
      "C-x h y" #'helm-show-kill-ring)

    ("post activate")
    #'serika-f/helm/activate))
