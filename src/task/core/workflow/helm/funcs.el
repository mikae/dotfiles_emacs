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
  (serika-c/eg/add-install :package-list '(helm)
                           :name         'helm)

  (serika-c/eg/add-many-by-name 'helm
                        ("require")
                        (lambda ()
                          (require 'helm-lib)
                          (require 'helm-config))

                        ("settings")
                        (lambda ()
                          (when (executable-find "curl")
                            (setq helm-google-suggest-use-curl-p t))

                          (setq helm-split-window-in-side-p           t)
                          (setq helm-buffers-fuzzy-matching           t)
                          (setq helm-move-to-line-cycle-in-source     t)
                          (setq helm-ff-search-library-in-sexp        t)
                          (setq helm-ff-file-name-history-use-recentf t))

                        ("keymap")
                        (lambda ()
                          (cl-macrolet ((cl (ind)
                                            `(lambda ()
                                               (interactive)
                                               (helm-select-nth-action (- ,ind))))
                                        (cm (map)
                                            `(func/keymap/save   ,map)
                                            `(func/keymap/create ,map
                                                                 "A-e" 'helm-next-line
                                                                 "A-i" 'helm-previous-line
                                                                 "A-E" 'helm-next-source
                                                                 "A-I" 'helm-previous-source
                                                                 "RET" 'helm-maybe-exit-minibuffer
                                                                 "TAB" 'helm-select-action
                                                                 "A-1" (cl 0)
                                                                 "A-2" (cl 1)
                                                                 "A-3" (cl 2)
                                                                 "A-4" (cl 3)
                                                                 "A-5" (cl 4)
                                                                 "A-6" (cl 5)
                                                                 "A-7" (cl 6)
                                                                 "A-8" (cl 7)
                                                                 "A-9" (cl 8)
                                                                 "A-0" (cl 9))))
                            (cm helm-map)

                            (func/keymap/save   helm-buffer-map)
                            (func/keymap/create helm-buffer-map)))

                        ("global-keymap")
                        (lambda ()
                          (func/keymap/define-global "M-a"   #'helm-M-x

                                                     "C-x f" #'helm-find-files
                                                     "C-x y" #'helm-show-kill-ring))

                        ("post activate")
                        #'serika-f/helm/activate))
