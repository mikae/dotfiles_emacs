(require 'ert)
(require 'func-buffer)

;; `check-modes'
(ert-deftest check-modes|check-one ()
  (let ((buf (generate-new-buffer "test")))
    (setq major-mode 'test-mode-1)
    (should     (serika-f/buffer/check-modes 'test-mode-1))
    (should-not (serika-f/buffer/check-modes 'test-mode-2))))

(ert-deftest check-modes|check-many ()
  (let ((buf (generate-new-buffer "test")))
    (setq major-mode 'test-mode-1)
    (should (serika-f/buffer/check-modes 'test-mode-1
                                         'test-mode-2))
    (should (serika-f/buffer/check-modes 'test-mode-1
                                         'test-mode-2))
    (setq major-mode 'test-mode-2)
    (should (serika-f/buffer/check-modes 'test-mode-1
                                         'test-mode-2))
    (should (serika-f/buffer/check-modes 'test-mode-1
                                         'test-mode-2))))

(ert-deftest check-modes|check-none ()
  (let ((buf (generate-new-buffer "test")))
    (setq major-mode 'test-mode-3)
    (should-not (serika-f/buffer/check-modes 'test-mode-1
                                         'test-mode-2))
    (should-not (serika-f/buffer/check-modes 'test-mode-1
                                         'test-mode-2))))

;; `last-string'
(defmacro with-buffer-with-text (text &rest forms)
  `(let ((buffer (generate-new-buffer "test-buffer")))
     (with-current-buffer buffer
       (insert ,text)
       (progn ,@forms))))

(ert-deftest last-string|check-none ()
  (with-buffer-with-text ""
                         (should (string= ""
                                          (serika-f/buffer/last-string)))))

(ert-deftest last-string|check-results-oncely ()
  (with-buffer-with-text "I LIKE ANIME"
                         (should (string= "I LIKE ANIME"
                                          (serika-f/buffer/last-string))))
  (with-buffer-with-text "I LIKE ANIME, TOO"
                         (should (string= "I LIKE ANIME, TOO"
                                          (serika-f/buffer/last-string)))))

(ert-deftest last-string|check-results-multiple ()
  (with-buffer-with-text "I LIKE ANIME\nI LIKE TOO"
                         (should (string= "I LIKE ANIME\nI LIKE TOO"
                                          (serika-f/buffer/last-string 2))))
  (with-buffer-with-text "1\n2\n3\n4\n5"
                         (should (string= "1\n2\n3\n4\n5"
                                          (serika-f/buffer/last-string 5)))))
