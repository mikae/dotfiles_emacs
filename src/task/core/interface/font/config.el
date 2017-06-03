;; Set font
(set-face-attribute 'default nil
                    :font (concat serika-interface-font-default
                                  " "
                                  (number-to-string serika-interface-font-power)))
(set-frame-font (concat serika-interface-font-default
                        " "
                        (number-to-string serika-interface-font-power)) nil t)
