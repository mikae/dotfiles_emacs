(require 'func-package)

;; Install `moe-theme'
(serika/package/make-sure-installed 'moe-theme)

;; Load `moe-theme'
(require 'moe-theme)

;; Show highlighted buffer-id as decoration. (Default: nil)
(setq moe-theme-highlight-buffer-id t)

(moe-theme-random-color)
(powerline-moe-theme)
(moe-dark)
