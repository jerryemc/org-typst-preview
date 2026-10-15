;;; org-typst-preview-test.el --- Tests for org-typst-preview -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Roi Fe Wu

;;; Code:

(require 'ert)
(require 'org-typst-preview)

(defmacro org-typst-preview-test--with-buffer (&rest body)
  "Run BODY in a temporary buffer, including normal buffer cleanup hooks."
  (declare (indent 0))
  `(let ((buffer (generate-new-buffer " *org-typst-preview-test*")))
     (unwind-protect
         (with-current-buffer buffer ,@body)
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun org-typst-preview-test--native-available-p ()
  "Return non-nil when native Typst highlighting can be tested."
  (and (require 'typst-ts-mode nil t) (treesit-ready-p 'typst t)))

(defun org-typst-preview-test--faces (text mode)
  "Return the face at each character of TEXT in MODE."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (funcall mode))
    (font-lock-ensure)
    (mapcar (lambda (pos) (get-text-property pos 'face))
            (number-sequence (point-min) (1- (point-max))))))

(ert-deftest org-typst-preview-native-source-faces-match-typst ()
  (skip-unless (org-typst-preview-test--native-available-p))
  (dolist (delimiters '(("#(" . "#)") ("#[" . "#]")))
    (let* ((body "\n  sum_(i=1)^n α + frac(1, 2) // comment\n  \"text\"\n")
           (native (org-typst-preview-test--faces
                    (concat "$ " body "\n$") #'typst-ts-mode)))
      (org-typst-preview-test--with-buffer
        (org-mode)
        (variable-pitch-mode 1)
        (insert (car delimiters) body (cdr delimiters))
        (org-typst-preview-mode 1)
        (font-lock-ensure)
        (dotimes (i (length body))
          (let* ((face (nth (+ i 2) native))
                 (actual (get-text-property (+ i 3) 'face))
                 (base (if face (car (last actual)) actual)))
            (when face
              (should (equal (butlast actual)
                             (if (listp face) face (list face)))))
            (dolist (attribute '(:family :height :weight :slant))
              (should (equal (plist-get base attribute)
                             (face-attribute 'default attribute))))))
        (should (memq 'typst-ts-math-indicator-face
                      (get-text-property 1 'face)))))))

(ert-deftest org-typst-preview-highlighting-preserves-nonfragments ()
  (skip-unless (org-typst-preview-test--native-available-p))
  (let* ((text "* Heading\n*bold* and $x^2$ and \\#(escaped#).\n\n#+begin_example\n#[literal#]\n#+end_example\n#+begin_src typst\n#(literal#)\n#+end_src\n")
         (expected (org-typst-preview-test--faces text #'org-mode)))
    (org-typst-preview-test--with-buffer
      (org-mode)
      (insert text)
      (org-typst-preview-mode 1)
      (font-lock-ensure)
      (should (equal expected
                     (mapcar (lambda (pos) (get-text-property pos 'face))
                             (number-sequence 1 (1- (point-max)))))))))

(ert-deftest org-typst-preview-highlighting-refontifies-multiline-edits ()
  (skip-unless (org-typst-preview-test--native-available-p))
  (org-typst-preview-test--with-buffer
    (org-mode)
    (insert "Before\n#[\nx +\ny\n#]\nAfter")
    (org-typst-preview-mode 1)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "y")
    (backward-delete-char 1)
    (insert "\"hello\"")
    (font-lock-flush (line-beginning-position) (line-end-position))
    (font-lock-ensure (line-beginning-position) (line-end-position))
    (should (memq 'font-lock-string-face
                  (get-text-property (- (point) 3) 'face)))
    (search-forward "#]")
    (backward-delete-char 2)
    (font-lock-flush (line-beginning-position) (line-end-position))
    (font-lock-ensure)
    (let ((text (buffer-string)))
      (should (equal (mapcar (lambda (pos) (get-text-property pos 'face))
                            (number-sequence 1 (1- (point-max))))
                     (org-typst-preview-test--faces
                      (substring-no-properties text) #'org-mode))))))

(ert-deftest org-typst-preview-highlighting-lifecycle ()
  (skip-unless (org-typst-preview-test--native-available-p))
  (let ((typst-ts-mode-hook (list (lambda () (ert-fail "Editing hook ran"))))
        hidden)
    (org-typst-preview-test--with-buffer
      (org-mode)
      (insert "#(x^2#)")
      (org-typst-preview-mode 1)
      (setq hidden org-typst-preview--fontification-buffer)
      (should (buffer-live-p hidden))
      (font-lock-ensure)
      (set-buffer-modified-p nil)
      (org-typst-preview-mode -1)
      (font-lock-ensure)
      (should-not (buffer-live-p hidden))
      (should-not (buffer-modified-p))
      (should (equal (get-text-property 3 'face)
                     (nth 2 (org-typst-preview-test--faces "#(x^2#)" #'org-mode))))
      (org-typst-preview-mode 1)
      (setq hidden org-typst-preview--fontification-buffer))
    (should-not (buffer-live-p hidden))))

(ert-deftest org-typst-preview-highlighting-is-optional ()
  (with-temp-buffer
    (org-mode)
    (let ((org-typst-preview-fontify nil))
      (org-typst-preview-mode 1)
      (should-not org-typst-preview--fontification-buffer)
      (should (eq (command-remapping 'org-latex-preview) #'org-typst-preview)))
    (org-typst-preview-mode -1)
    (cl-letf (((symbol-function 'treesit-ready-p) (lambda (&rest _) nil)))
      (org-typst-preview-mode 1)
      (should-not org-typst-preview--fontification-buffer))))

(ert-deftest org-typst-preview-parses-boundaries ()
  (with-temp-buffer
    (org-mode)
    (insert "#(x^2#)\n#[\n  sum_(i=1)^n i\n#]")
    (let ((inline (org-typst-preview--fragment-at (point-min))))
      (should (equal (plist-get inline :body) "x^2"))
      (should (equal (plist-get inline :value) "$x^2$"))
      (should-not (plist-get inline :block)))
    (goto-char (point-min))
    (search-forward "#[")
    (let ((block (org-typst-preview--fragment-at (- (point) 2))))
      (should (equal (plist-get block :body) "sum_(i=1)^n i"))
      (should (equal (plist-get block :value) "$ sum_(i=1)^n i $"))
      (should (plist-get block :block)))))

(ert-deftest org-typst-preview-collects-only-typst-fragments ()
  (with-temp-buffer
    (org-mode)
    (insert "Inline #(sum_(i=1)^n i#).\n\n"
            "#[integral_0^1 x^2 dif x = 1/3#]\n"
            "LaTeX remains $x^2$.")
    (let ((fragments
           (org-typst-preview--fragments (point-min) (point-max))))
      (should (= (length fragments) 2))
      (should (equal (plist-get (car fragments) :value)
                     "$sum_(i=1)^n i$"))
      (should (equal (plist-get (cadr fragments) :value)
                     "$ integral_0^1 x^2 dif x = 1/3 $")))))

(ert-deftest org-typst-preview-ignores-literal-and-escaped-boundaries ()
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src typst\n"
            "#[not_a_preview#]\n"
            "#+end_src\n\n"
            "\\#(escaped#) and #(previewed#)")
    (let ((fragments
           (org-typst-preview--fragments (point-min) (point-max))))
      (should (= (length fragments) 1))
      (should (equal (plist-get (car fragments) :body) "previewed")))))

(ert-deftest org-typst-preview-builds-typst-source-with-org-settings ()
  (let ((org-format-latex-options
         '(:foreground "red" :background "Transparent" :scale 2.0))
        (org-typst-preview-font-size 10.0)
        (org-typst-preview-preamble "#let answer = 42"))
    (let ((source (org-typst-preview--source "$answer$" 'default)))
      (should (string-match-p "fill: none" source))
      (should (string-match-p "size: 20\\.000pt" source))
      (should (string-match-p "fill: rgb(255,0,0)" source))
      (should (string-match-p "#let answer = 42" source))
      (should (string-suffix-p "$answer$\n" source)))))

(ert-deftest org-typst-preview-mode-remaps-org-commands ()
  (with-temp-buffer
    (org-mode)
    (org-typst-preview-mode 1)
    (should (eq (command-remapping 'org-latex-preview)
                'org-typst-preview))
    (should (eq (command-remapping 'org-clear-latex-preview)
                'org-typst-preview-clear))
    (should (memq #'org-typst-preview--export-before-parsing
                  org-export-before-parsing-functions))))

(ert-deftest org-typst-preview-expands-typst-export-boundaries ()
  (with-temp-buffer
    (org-mode)
    (insert "Inline #(x^2#).\n\n#[sum_(i=1)^n i#]")
    (org-typst-preview--export-before-parsing 'typst)
    (let ((result (buffer-string)))
      (should (string-match-p
               (regexp-quote "Inline @@typst:$x^2$@@.") result))
      (should (string-match-p
               (regexp-quote
                "#+begin_export typst\n$ sum_(i=1)^n i $\n#+end_export")
               result)))))

(ert-deftest org-typst-preview-compiles-and-caches-svg ()
  :tags '(:integration)
  (skip-unless (executable-find org-typst-preview-executable))
  (let* ((cache-directory (make-temp-file "org-typst-preview-test-" t))
         (org-typst-preview-image-directory cache-directory)
         (org-format-latex-options
          '(:foreground "red" :background "white" :scale 1.0))
         (org-typst-preview--jobs (make-hash-table :test #'equal))
         (org-typst-preview--queue nil)
         (org-typst-preview--running 0)
         (deadline (+ (float-time) 15.0)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#[sum_(i=1)^n i = (n(n+1))/2#]")
          (let ((fragment
                 (car (org-typst-preview--fragments
                       (point-min) (point-max)))))
            (should (eq (org-typst-preview--schedule fragment) 'scheduled))
            (while (and (> (hash-table-count org-typst-preview--jobs) 0)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05))
            (should (= (hash-table-count org-typst-preview--jobs) 0))
            (should (= (length (org-typst-preview--overlays
                                (point-min) (point-max)))
                       1))
            (let ((svg (car (directory-files cache-directory t "\\.svg\\'"))))
              (should svg)
              (with-temp-buffer
                (insert-file-contents svg)
                (should (search-forward "<svg" nil t))))
            (org-typst-preview-clear)
            (should (eq (org-typst-preview--schedule fragment) 'cached))))
      (delete-directory cache-directory t))))

(ert-deftest org-typst-preview-discards-edited-pending-preview ()
  :tags '(:integration)
  (skip-unless (executable-find org-typst-preview-executable))
  (let* ((cache-directory (make-temp-file "org-typst-preview-stale-" t))
         (org-typst-preview-image-directory cache-directory)
         (org-format-latex-options
          '(:foreground "red" :background "Transparent" :scale 1.0))
         (org-typst-preview--jobs (make-hash-table :test #'equal))
         (org-typst-preview--queue nil)
         (org-typst-preview--running 0)
         (deadline (+ (float-time) 15.0)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#(x#)")
          (org-typst-preview--schedule
           (car (org-typst-preview--fragments (point-min) (point-max))))
          (goto-char 3)
          (delete-char 1)
          (insert "y")
          (while (and (> (hash-table-count org-typst-preview--jobs) 0)
                      (< (float-time) deadline))
            (accept-process-output nil 0.05))
          (should-not (org-typst-preview--overlays
                       (point-min) (point-max))))
      (delete-directory cache-directory t))))

(ert-deftest org-typst-preview-keeps-latest-compiler-error ()
  :tags '(:integration)
  (skip-unless (executable-find org-typst-preview-executable))
  (let* ((cache-directory (make-temp-file "org-typst-preview-error-" t))
         (org-typst-preview-image-directory cache-directory)
         (org-typst-preview-preamble "#let invalid = (")
         (org-typst-preview-log-buffer
          (generate-new-buffer-name " *org-typst-preview-error*"))
         (org-format-latex-options
          '(:foreground "red" :background "Transparent" :scale 1.0))
         (org-typst-preview--jobs (make-hash-table :test #'equal))
         (org-typst-preview--queue nil)
         (org-typst-preview--running 0)
         (deadline (+ (float-time) 15.0)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#(x#)")
          (org-typst-preview--schedule
           (car (org-typst-preview--fragments (point-min) (point-max))))
          (while (and (> (hash-table-count org-typst-preview--jobs) 0)
                      (< (float-time) deadline))
            (accept-process-output nil 0.05))
          (should (get-buffer org-typst-preview-log-buffer))
          (should-not (org-typst-preview--overlays
                       (point-min) (point-max))))
      (when-let* ((buffer (get-buffer org-typst-preview-log-buffer)))
        (kill-buffer buffer))
      (delete-directory cache-directory t))))

(provide 'org-typst-preview-test)

;;; org-typst-preview-test.el ends here
