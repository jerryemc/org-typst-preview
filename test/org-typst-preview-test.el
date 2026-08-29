;;; org-typst-preview-test.el --- Tests for org-typst-preview -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Roi Fe Wu

;; This file is part of org-typst-preview.

;;; Code:

(require 'ert)
(require 'org-typst-preview)

(ert-deftest org-typst-preview-normalizes-native-fragments ()
  (should (equal (org-typst-preview--normalize-fragment "$x^2$")
                 "$x^2$"))
  (should (equal (org-typst-preview--normalize-fragment "$ x^2 $")
                 "$ x^2 $"))
  (should (equal (org-typst-preview--normalize-fragment "$$x^2$$")
                 "$ x^2 $"))
  (should (equal (org-typst-preview--normalize-fragment "\\(x^2\\)")
                 "$x^2$"))
  (should (equal (org-typst-preview--normalize-fragment "\\[x^2\\]")
                 "$ x^2 $")))

(ert-deftest org-typst-preview-detects-block-math ()
  (should-not (org-typst-preview--block-p "$x^2$"))
  (should (org-typst-preview--block-p "$ x^2 $"))
  (should (org-typst-preview--block-p "$\nx^2\n$")))

(ert-deftest org-typst-preview-collects-org-fragments ()
  (with-temp-buffer
    (org-mode)
    (insert "Inline $sum_(i=1)^n i$.\n\n"
            "$ integral_0^1 x^2 dif x = 1/3 $\n")
    (let ((fragments
           (org-typst-preview--collect-fragments (point-min) (point-max))))
      (should (= (length fragments) 2))
      (should (equal (plist-get (car fragments) :value)
                     "$sum_(i=1)^n i$"))
      (should-not (plist-get (car fragments) :block))
      (should (equal (plist-get (cadr fragments) :value)
                     "$ integral_0^1 x^2 dif x = 1/3 $"))
      (should (plist-get (cadr fragments) :block)))))

(ert-deftest org-typst-preview-ignores-literal-org-content ()
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src typst\n"
            "$ this_must_not_be_previewed $\n"
            "#+end_src\n\n"
            "$ this_must_be_previewed $")
    (let ((fragments
           (org-typst-preview--collect-fragments (point-min) (point-max))))
      (should (= (length fragments) 1))
      (should (equal (plist-get (car fragments) :value)
                     "$ this_must_be_previewed $")))))

(ert-deftest org-typst-preview-remaps-org-preview-commands ()
  (with-temp-buffer
    (org-mode)
    (org-typst-preview-mode 1)
    (should (eq (command-remapping 'org-latex-preview)
                'org-typst-preview))
    (should (eq (command-remapping 'org-clear-latex-preview)
                'org-typst-preview-clear))))

(ert-deftest org-typst-preview-builds-self-contained-source ()
  (let ((org-format-latex-options
         '(:foreground "#112233" :background "Transparent" :scale 2.0))
        (org-typst-preview-font-size 10.0)
        (org-typst-preview-preamble "#let answer = 42"))
    (let ((source
           (org-typst-preview--render-source "$answer$" 'default)))
      (should (string-match-p "fill: none" source))
      (should (string-match-p "size: 20\\.000pt" source))
      (should (string-match-p "rgb(\"#112233\")" source))
      (should (string-match-p "#let answer = 42" source))
      (should (string-suffix-p "$answer$\n" source)))))

(ert-deftest org-typst-preview-cache-key-tracks-source ()
  (should
   (equal (org-typst-preview--cache-key "$x$")
          (org-typst-preview--cache-key "$x$")))
  (should-not
   (equal (org-typst-preview--cache-key "$x$")
          (org-typst-preview--cache-key "$y$"))))

(ert-deftest org-typst-preview-invalidates-stale-waiters ()
  (with-temp-buffer
    (insert "$x$")
    (let ((waiter
           (org-typst-preview--make-waiter
            :buffer (current-buffer)
            :begin (copy-marker (point-min))
            :end (copy-marker (point-max) t)
            :text "$x$"
            :block nil)))
      (should (org-typst-preview--waiter-valid-p waiter))
      (goto-char 2)
      (delete-char 1)
      (insert "y")
      (should-not (org-typst-preview--waiter-valid-p waiter))
      (org-typst-preview--release-waiter waiter))))

(ert-deftest org-typst-preview-compiles-svg-asynchronously ()
  :tags '(:integration)
  (skip-unless (executable-find org-typst-preview-executable))
  (let* ((cache-directory (make-temp-file "org-typst-preview-test-" t))
         (org-typst-preview-image-directory cache-directory)
         (org-preview-latex-image-directory cache-directory)
         (org-format-latex-options
          '(:foreground "#111111" :background "Transparent" :scale 1.0))
         (org-typst-preview--jobs (make-hash-table :test #'equal))
         (org-typst-preview--queue nil)
         (org-typst-preview--running 0)
         (deadline (+ (float-time) 15.0)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "$ sum_(i=1)^n i = (n(n+1))/2 $")
          (let ((fragment
                 (car (org-typst-preview--collect-fragments
                       (point-min) (point-max)))))
            (should fragment)
            (should (eq (org-typst-preview--schedule-fragment fragment)
                        'queued))
            (while (and (> (hash-table-count org-typst-preview--jobs) 0)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05))
            (should (= (hash-table-count org-typst-preview--jobs) 0))
            (should (= (length
                        (org-typst-preview--our-overlays
                         (point-min) (point-max)))
                       1))
            (let ((svgs (directory-files cache-directory t "\\.svg\\'")))
              (should (= (length svgs) 1))
              (should (org-typst-preview--valid-cache-file-p (car svgs)))
              (with-temp-buffer
                (insert-file-contents (car svgs))
                (should (search-forward "<svg" nil t))))
            (org-typst-preview-clear (point-min) (point-max))
            (should (eq (org-typst-preview--schedule-fragment fragment)
                        'cached))
            (should (= (length
                        (org-typst-preview--our-overlays
                         (point-min) (point-max)))
                       1))))
      (delete-directory cache-directory t))))

(ert-deftest org-typst-preview-reports-compiler-errors ()
  :tags '(:integration)
  (skip-unless (executable-find org-typst-preview-executable))
  (let* ((cache-directory (make-temp-file "org-typst-preview-error-" t))
         (org-typst-preview-image-directory cache-directory)
         (org-typst-preview-preamble "#let deliberately-invalid = (")
         (org-typst-preview-log-buffer
          (generate-new-buffer-name " *org-typst-preview-error-log*"))
         (org-format-latex-options
          '(:foreground "#111111" :background "Transparent" :scale 1.0))
         (org-typst-preview--jobs (make-hash-table :test #'equal))
         (org-typst-preview--queue nil)
         (org-typst-preview--running 0)
         (deadline (+ (float-time) 15.0)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "$x$")
          (let ((fragment
                 (car (org-typst-preview--collect-fragments
                       (point-min) (point-max)))))
            (should (eq (org-typst-preview--schedule-fragment fragment)
                        'queued))
            (while (and (> (hash-table-count org-typst-preview--jobs) 0)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05))
            (should (= (hash-table-count org-typst-preview--jobs) 0))
            (should-not (directory-files cache-directory nil "\\.svg\\'"))
            (let ((overlays
                   (org-typst-preview--our-overlays
                    (point-min) (point-max))))
              (should (= (length overlays) 1))
              (should (overlay-get (car overlays)
                                   'org-typst-preview-error)))
            (should (get-buffer org-typst-preview-log-buffer))))
      (when-let* ((buffer (get-buffer org-typst-preview-log-buffer)))
        (kill-buffer buffer))
      (delete-directory cache-directory t))))

(provide 'org-typst-preview-test)

;;; org-typst-preview-test.el ends here
