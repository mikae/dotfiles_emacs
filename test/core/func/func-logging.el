;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'func-logging)
(require 'func-buffer)

(ert-deftest log ()
  (setq serika-logging-format "Log: %m")
  (serika/logging/log "test")
  (with-current-buffer (get-buffer "*Messages*")
    (should (string= (serika-f/buffer/last-string)
                     "Log: test"))))
