;;; package --- Summary
;;; Commentary:
;;; Code:

(defun init ()
  "Configure `spaceline'."
  (serika-c/eg/add-install :type 'package
                           :name 'spaceline
                           :package-list '(spaceline))

  (serika-c/eg/add-many-by-name 'spaceline
                                ("require")
                                (func/func/requirer spaceline
                                                    spaceline-segments)

                                ("settings")
                                (lambda ()
                                  (set-face-background 'powerline-active1 "#444444")
                                  (set-face-background 'powerline-active2 "#333333")
                                  (set-face-foreground 'powerline-active1 "#aaaaaa")
                                  (set-face-foreground 'powerline-active2 "#cccccc")

                                  (set-face-background 'powerline-inactive1 "#333333")
                                  (set-face-background 'powerline-inactive2 "#222222")
                                  (set-face-foreground 'powerline-inactive1 "#999999")
                                  (set-face-foreground 'powerline-inactive2 "#888888")

                                  (set-face-background 'mode-line "#333333")
                                  (set-face-foreground 'mode-line "#bbbbbb")
                                  (setq spaceline-face-func
                                        (lambda (face active)
                                          (if active
                                              (cond
                                               ((eq face 'face1)
                                                'powerline-active1)
                                               ((eq face 'face2)
                                                'powerline-active2)
                                               ((eq face 'line)
                                                'mode-line)
                                               ((eq face 'highlight)
                                                'powerline-active2))
                                            (cond
                                             ((eq face 'face1)
                                              'powerline-inactive1)
                                             ((eq face 'face2)
                                              'powerline-inactive2)
                                             ((eq face 'line)
                                              'mode-line)
                                             ((eq face 'highlight)
                                              'powerline-inactive2)))))

                                  (spaceline-compile 'main
                                                     '(buffer-id
                                                       (buffer-encoding-abbrev
                                                        point-position
                                                        line-column)
                                                       buffer-modified
                                                       buffer-position
                                                       evil-state)
                                                     '(version-control
                                                       projectile-root
                                                       major-mode))
                                  (setq-default mode-line-format
                                                '("%e" (:eval (spaceline-ml-main)))))))
