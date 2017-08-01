;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'ert)
(require 'func-keymap)

;; `p'
(ert-deftest p|check-result ()
  (should     (serika-f/keymap/p (make-sparse-keymap)))
  (should-not (serika-f/keymap/p 'ff))
  (should-not (serika-f/keymap/p (list 1))))

;; `create'
(ert-deftest create|throw-if-odd-args-1 ()
  (should-error (serika-f/keymap/create "C-f")))

(ert-deftest create|throw-if-odd-args-2 ()
  (should-error (serika-f/keymap/create "C-f" 'ignore
                                        "C-e")))

(ert-deftest create|keymap-is-created ()
  (should (serika-f/keymap/p (serika-f/keymap/create))))

(ert-deftest create|bindings-are-created ()
  (let ((map (serika-f/keymap/create "C-f" 'ignore
                                     "C-n" 'ignore-2)))
    (should (eq (lookup-key map (kbd "C-f"))
                'ignore))
    (should (eq (lookup-key map (kbd "C-n"))
                'ignore-2))))

;; `define'
(ert-deftest define|same-keymap ()
  (let* ((map   (serika-f/keymap/create))
         (map-2 (serika-f/keymap/define map)))
    (should (eq map
                map-2))))

(ert-deftest define|add-bindings ()
  (let ((map   (serika-f/keymap/create)))
    (serika-f/keymap/define map
                            "C-f" 'ignore
                            "C-t" 'ignore-2)
    (should (eq (lookup-key map (kbd "C-f"))
                'ignore))
    (should (eq (lookup-key map (kbd "C-t"))
                'ignore-2))))

;; `define-global'
(ert-deftest define-global|return-nil ()
  (should (eq (serika-f/keymap/define-global)
              nil)))

(ert-deftest define-global|add-bindings ()
  (let ((map (make-sparse-keymap)))
    (setq global-map map)
    (use-global-map map))
  (serika-f/keymap/define-global "C-f" 'ignore
                                 "C-t" 'ignore-2)
  (should (eq (lookup-key global-map (kbd "C-f"))
              'ignore))
  (should (eq (lookup-key global-map (kbd "C-t"))
              'ignore-2)))
