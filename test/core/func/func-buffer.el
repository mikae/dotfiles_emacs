(require 'ert)
(require 'func-buffer)

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
