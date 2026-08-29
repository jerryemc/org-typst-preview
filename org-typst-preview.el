;;; org-typst-preview.el --- Async Typst previews for Org math -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Roi Fe Wu

;; Author: Roi Fe Wu <roifewu@gmail.com>
;; Maintainer: Roi Fe Wu <roifewu@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: outlines, tex, typst, multimedia
;; URL: https://github.com/roife/org-typst-preview

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Render native Typst math in Org buffers through the interface people
;; already use for `org-latex-preview'.  Org continues to parse `$...$' as a
;; `latex-fragment'; this package changes only the preview renderer.
;;
;; Enable `org-typst-preview-mode' to remap `org-latex-preview' and
;; `org-toggle-latex-fragment' to the asynchronous Typst implementation.  The
;; usual `C-c C-x C-l' binding and prefix argument behavior are preserved.
;;
;; The renderer reuses `org-format-latex-options' for foreground, background,
;; and scale, and reuses `org-preview-latex-image-directory' unless
;; `org-typst-preview-image-directory' is customized.  Generated overlays use
;; Org's `org-latex-overlay' type, so `org-clear-latex-preview' can also remove
;; completed Typst previews.
;;
;; Inline Typst math has no spaces next to the delimiters:
;;
;;     $sum_(i=1)^n i$
;;
;; Display math has spaces next to both delimiters:
;;
;;     $ sum_(i=1)^n i = (n(n+1))/2 $
;;
;; Rendering is asynchronous, concurrent work is bounded, identical jobs are
;; coalesced, and successful SVG output is cached by content.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'org)
(require 'org-element)
(require 'seq)
(require 'subr-x)

(defgroup org-typst-preview nil
  "Asynchronous Typst previews for Org math fragments."
  :group 'org-latex
  :prefix "org-typst-preview-")

(defcustom org-typst-preview-executable "typst"
  "Typst executable used to render previews."
  :group 'org-typst-preview
  :type 'string)

(defcustom org-typst-preview-process-arguments
  '("compile" "--format" "svg")
  "Arguments passed to Typst before the input and output file names.

The resulting command has the form:

  EXECUTABLE ARGUMENTS INPUT OUTPUT"
  :group 'org-typst-preview
  :type '(repeat string))

(defcustom org-typst-preview-image-directory nil
  "Directory in which to cache generated SVG previews.

When nil, reuse `org-preview-latex-image-directory'.  Relative directories
are resolved from the Org buffer's `default-directory'.  For remote or
non-file buffers, a relative directory is resolved below
variable `temporary-file-directory'."
  :group 'org-typst-preview
  :type '(choice (const :tag "Reuse Org LaTeX preview directory" nil)
                 directory))

(defcustom org-typst-preview-preamble ""
  "Typst code inserted after the package's default set rules.

Use this to import packages or define symbols shared by preview fragments.
The preamble is part of the preview cache key."
  :group 'org-typst-preview
  :type 'string)

(defcustom org-typst-preview-font-size 11.0
  "Base Typst font size in points.

The effective size is multiplied by either `org-typst-preview-scale' or the
`:scale' value in `org-format-latex-options'."
  :group 'org-typst-preview
  :type 'number)

(defcustom org-typst-preview-font-family nil
  "Optional Typst font family for preview text.

Nil lets Typst select its normal text and math fonts."
  :group 'org-typst-preview
  :type '(choice (const :tag "Typst default" nil) string))

(defcustom org-typst-preview-scale nil
  "Scale applied to Typst previews.

Nil reuses the `:scale' value in `org-format-latex-options'.  A number
overrides that value for Typst previews only."
  :group 'org-typst-preview
  :type '(choice (const :tag "Use org-format-latex-options" nil) number))

(defcustom org-typst-preview-page-margin "0pt"
  "Typst page margin used around each tightly cropped preview."
  :group 'org-typst-preview
  :type 'string)

(defcustom org-typst-preview-max-processes 4
  "Maximum number of concurrent Typst compiler processes."
  :group 'org-typst-preview
  :type 'integer)

(defcustom org-typst-preview-auto-delay 0.45
  "Idle delay in seconds before automatically rendering edited math."
  :group 'org-typst-preview
  :type 'number)

(defcustom org-typst-preview-log-buffer "*Org Preview Typst Output*"
  "Name of the buffer that receives Typst compiler diagnostics."
  :group 'org-typst-preview
  :type 'string)

(defcustom org-typst-preview-when-risky nil
  "Whether Typst preview may run while Org marks content as untrusted.

Keep this nil unless you accept that a Typst preamble or imported package can
read files allowed by the local Typst compiler configuration."
  :group 'org-typst-preview
  :type 'boolean
  :risky t)

(defface org-typst-preview-error
  '((t :inherit error :underline (:style wave)))
  "Face used to mark a Typst fragment whose compilation failed."
  :group 'org-typst-preview)

(cl-defstruct
    (org-typst-preview--waiter
     (:constructor org-typst-preview--make-waiter))
  buffer begin end text block)

(cl-defstruct
    (org-typst-preview--job
     (:constructor org-typst-preview--make-job))
  key source target source-directory command waiters process process-buffer
  source-file render-file started finished)

(defvar org-typst-preview--jobs (make-hash-table :test #'equal)
  "Map cache file names to queued or running preview jobs.")

(defvar org-typst-preview--queue nil
  "FIFO list of Typst preview jobs waiting to start.")

(defvar org-typst-preview--running 0
  "Number of Typst preview processes currently running.")

(defvar-local org-typst-preview--auto-timer nil
  "Idle timer used by `org-typst-preview-auto-mode'.")

(defvar-local org-typst-preview-auto-mode nil
  "Non-nil when edited Typst fragments are previewed automatically.")

(defconst org-typst-preview--cache-version 1
  "Version mixed into generated preview cache keys.")

(defun org-typst-preview--safe-to-run-p ()
  "Return non-nil when it is safe to invoke the Typst compiler."
  (or org-typst-preview-when-risky
      (not (and (boundp 'untrusted-content) untrusted-content))))

(defun org-typst-preview--check-executable ()
  "Signal a user error when the configured Typst executable is unavailable."
  (unless (or (and (file-name-absolute-p org-typst-preview-executable)
                   (file-executable-p org-typst-preview-executable))
              (executable-find org-typst-preview-executable))
    (user-error "Cannot find Typst executable: %s"
                org-typst-preview-executable)))

(defun org-typst-preview--face-color (attribute face)
  "Resolve Org's color option for ATTRIBUTE at FACE.

ATTRIBUTE is either `:foreground' or `:background'."
  (let ((setting (plist-get org-format-latex-options attribute)))
    (cond
     ((eq setting 'auto)
      (face-attribute face attribute nil 'default))
     ((eq setting 'default)
      (face-attribute 'default attribute nil 'default))
     (t setting))))

(defun org-typst-preview--color-hex (color fallback)
  "Convert COLOR to #rrggbb, returning FALLBACK when it cannot be resolved."
  (condition-case nil
      (cond
       ((and (stringp color)
             (string-match-p "\\`#[[:xdigit:]]\\{6\\}\\'" color))
        (downcase color))
       ((and (stringp color)
             (string-match
              "\\`#\\([[:xdigit:]]\\)\\([[:xdigit:]]\\)\\([[:xdigit:]]\\)\\'"
              color))
        (downcase
         (format "#%s%s%s%s%s%s"
                 (match-string 1 color) (match-string 1 color)
                 (match-string 2 color) (match-string 2 color)
                 (match-string 3 color) (match-string 3 color))))
       ((and (stringp color) (color-name-to-rgb color))
        (apply #'color-rgb-to-hex
               (append (color-name-to-rgb color) '(2))))
       (t fallback))
    (error fallback)))

(defun org-typst-preview--transparent-color-p (color)
  "Return non-nil when COLOR requests a transparent background."
  (or (null color)
      (and (stringp color)
           (string-equal (downcase color) "transparent"))))

(defun org-typst-preview--effective-scale ()
  "Return the numeric scale for a Typst preview."
  (float (or org-typst-preview-scale
             (plist-get org-format-latex-options :scale)
             1.0)))

(defun org-typst-preview--render-source (value face)
  "Build a complete Typst document for math fragment VALUE at FACE."
  (let* ((foreground (org-typst-preview--face-color :foreground face))
         (background (org-typst-preview--face-color :background face))
         (fg (org-typst-preview--color-hex foreground "#000000"))
         (bg (unless (org-typst-preview--transparent-color-p background)
               (org-typst-preview--color-hex background "#ffffff")))
         (size (* org-typst-preview-font-size
                  (org-typst-preview--effective-scale)))
         (font (if org-typst-preview-font-family
                   (format ", font: %S" org-typst-preview-font-family)
                 "")))
    (concat
     (format "#set page(width: auto, height: auto, margin: %s, fill: %s)\n"
             org-typst-preview-page-margin
             (if bg (format "rgb(%S)" bg) "none"))
     (format "#set text(size: %.3fpt, fill: rgb(%S)%s)\n" size fg font)
     "#show math.equation: set text(top-edge: \"bounds\", bottom-edge: \"bounds\")\n"
     (unless (string-empty-p org-typst-preview-preamble)
       (concat org-typst-preview-preamble "\n"))
     value
     "\n")))

(defun org-typst-preview--normalize-fragment (value)
  "Convert Org math fragment VALUE to native Typst delimiters.

Native `$...$' input is unchanged.  Org's legacy `$$...$$', `\\(...\\)',
and `\\[...\\]' delimiters are accepted as migration conveniences."
  (cond
   ((and (>= (length value) 4)
         (string-prefix-p "$$" value)
         (string-suffix-p "$$" value))
    (concat "$ " (string-trim (substring value 2 -2)) " $"))
   ((and (>= (length value) 2)
         (string-prefix-p "$" value)
         (string-suffix-p "$" value))
    value)
   ((and (>= (length value) 4)
         (string-prefix-p "\\(" value)
         (string-suffix-p "\\)" value))
    (concat "$" (substring value 2 -2) "$"))
   ((and (>= (length value) 4)
         (string-prefix-p "\\[" value)
         (string-suffix-p "\\]" value))
    (concat "$ " (string-trim (substring value 2 -2)) " $"))
   (t nil)))

(defun org-typst-preview--block-p (value)
  "Return non-nil when normalized Typst math VALUE is display math."
  (let ((length (length value)))
    (and (> length 3)
         (eq (aref value 0) ?$)
         (memq (aref value 1) '(?\s ?\t ?\n ?\r))
         (memq (aref value (- length 2)) '(?\s ?\t ?\n ?\r))
         (eq (aref value (1- length)) ?$))))

(defun org-typst-preview--fragment-from-element (element)
  "Return preview data for Org ELEMENT, or nil when it is unsupported."
  (when (eq (org-element-type element) 'latex-fragment)
    (when-let* ((value (org-element-property :value element))
                (normalized (org-typst-preview--normalize-fragment value)))
      (let* ((begin (org-element-property :begin element))
             (end (save-excursion
                    (goto-char (org-element-property :end element))
                    (skip-chars-backward " \r\t\n")
                    (point))))
        (list :begin begin :end end :value normalized
              :block (org-typst-preview--block-p normalized))))))

(defun org-typst-preview--fragment-at-point ()
  "Return preview data for the Typst math fragment at point, if any."
  (or (org-typst-preview--fragment-from-element (org-element-context))
      (and (> (point) (point-min))
           (save-excursion
             (backward-char)
             (org-typst-preview--fragment-from-element
              (org-element-context))))
      (org-typst-preview--custom-block-containing-point)))

(defun org-typst-preview--in-literal-context-p (position)
  "Return non-nil when POSITION is inside Org content that must stay literal."
  (save-excursion
    (goto-char position)
    (let ((context (org-element-context)))
      (org-element-lineage
       context
       '(code comment comment-block example-block export-block fixed-width
              inline-src-block keyword node-property property-drawer
              src-block verbatim)
       t))))

(defun org-typst-preview--custom-block-at (position &optional limit)
  "Parse native Typst display math beginning at POSITION.

Unlike Org's LaTeX parser, Typst display math has whitespace immediately after
the opening dollar and before the closing dollar.  Search no farther than
LIMIT, which defaults to `point-max'."
  (save-excursion
    (goto-char position)
    (when (and (eq (char-after) ?$)
               (not (eq (char-before) ?\\))
               (memq (char-after (1+ position)) '(?\s ?\t ?\n ?\r))
               (not (org-typst-preview--in-literal-context-p position)))
      (forward-char 2)
      (let (closing)
        (while (and (not closing) (search-forward "$" limit t))
          (let ((delimiter (1- (point))))
            (when (and (> delimiter position)
                       (memq (char-before delimiter) '(?\s ?\t ?\n ?\r))
                       (not (eq (char-before (1- delimiter)) ?\\)))
              (setq closing (point)))))
        (when closing
          (let ((value (buffer-substring-no-properties position closing)))
            (list :begin position :end closing :value value :block t)))))))

(defun org-typst-preview--custom-block-containing-point ()
  "Return native Typst display math containing point, if any."
  (let ((origin (point)) found)
    (save-excursion
      (while (and (not found) (search-backward "$" nil t))
        (when-let* ((fragment
                     (org-typst-preview--custom-block-at (point))))
          (when (<= (plist-get fragment :begin)
                    origin
                    (plist-get fragment :end))
            (setq found fragment)))))
    found))

(defun org-typst-preview--collect-fragments (begin end)
  "Collect Typst math fragments wholly between BEGIN and END."
  (let ((regexp "\\$\\|\\\\[([]")
        fragments
        last-begin)
    (save-excursion
      (goto-char begin)
      (while (re-search-forward regexp end t)
        (when-let* ((fragment
                     (or (and (eq (char-after (match-beginning 0)) ?$)
                              (org-typst-preview--custom-block-at
                               (match-beginning 0) end))
                         (org-typst-preview--fragment-from-element
                          (org-element-context)))))
          (let ((fragment-begin (plist-get fragment :begin))
                (fragment-end (plist-get fragment :end)))
            (when (and (>= fragment-begin begin)
                       (<= fragment-end end)
                       (not (equal fragment-begin last-begin)))
              (setq last-begin fragment-begin)
              (push fragment fragments))
            (goto-char (max (point) fragment-end))))))
    (nreverse fragments)))

(defun org-typst-preview--image-directory ()
  "Return the local absolute cache directory for the current buffer."
  (let* ((configured (or org-typst-preview-image-directory
                         (and (boundp 'org-preview-latex-image-directory)
                              org-preview-latex-image-directory)
                         "ltximg/"))
         (file (buffer-file-name (buffer-base-buffer))))
    (file-name-as-directory
     (cond
      ((file-remote-p configured)
       (expand-file-name "org-typst-preview/" temporary-file-directory))
      ((file-name-absolute-p configured) configured)
      ((or (not file) (file-remote-p file) (file-remote-p default-directory))
       (expand-file-name configured temporary-file-directory))
      (t (expand-file-name configured default-directory))))))

(defun org-typst-preview--source-directory ()
  "Return a writable local directory for a temporary Typst source file."
  (let ((directory (and (not (file-remote-p default-directory))
                        (file-directory-p default-directory)
                        (file-writable-p default-directory)
                        default-directory)))
    (or directory temporary-file-directory)))

(defun org-typst-preview--cache-key (source)
  "Return the cache key for rendered Typst SOURCE."
  (secure-hash
   'sha256
   (prin1-to-string
    (list org-typst-preview--cache-version
          org-typst-preview-executable
          org-typst-preview-process-arguments
          source))))

(defun org-typst-preview--valid-cache-file-p (file)
  "Return non-nil when FILE looks like a readable SVG cache entry."
  (and (file-readable-p file)
       (> (or (file-attribute-size (file-attributes file)) 0) 0)
       (with-temp-buffer
         (insert-file-contents-literally file nil 0 512)
         (search-forward "<svg" nil t))))

(defun org-typst-preview--our-overlays (begin end)
  "Return this package's overlays overlapping BEGIN and END."
  (cl-remove-if-not
   (lambda (overlay) (overlay-get overlay 'org-typst-preview))
   (overlays-in begin end)))

(defun org-typst-preview--remove-overlays (begin end)
  "Remove this package's overlays overlapping BEGIN and END."
  (let ((overlays (org-typst-preview--our-overlays begin end)))
    (mapc #'delete-overlay overlays)
    overlays))

(defun org-typst-preview--remove-org-image-overlays (begin end)
  "Remove Org formula image overlays overlapping BEGIN and END."
  (dolist (overlay (overlays-in begin end))
    (when (eq (overlay-get overlay 'org-overlay-type) 'org-latex-overlay)
      (delete-overlay overlay))))

(defun org-typst-preview--make-image-overlay
    (begin end image key block-p)
  "Display IMAGE over BEGIN through END and associate it with KEY.

BLOCK-P is recorded for clients that want to inspect the overlay."
  (org-typst-preview--remove-overlays begin end)
  (org-typst-preview--remove-org-image-overlays begin end)
  (let ((before (overlays-in begin end)))
    (org--make-preview-overlay begin end image "svg")
    (when-let* ((overlay
                 (cl-find-if
                  (lambda (candidate)
                    (and (not (memq candidate before))
                         (eq (overlay-get candidate 'org-overlay-type)
                             'org-latex-overlay)))
                  (overlays-in begin end))))
      (overlay-put overlay 'org-typst-preview t)
      (overlay-put overlay 'org-typst-preview-key key)
      (overlay-put overlay 'org-typst-preview-block block-p)
      overlay)))

(defun org-typst-preview--make-error-overlay (begin end message)
  "Mark BEGIN through END as failed and attach compiler MESSAGE."
  (org-typst-preview--remove-overlays begin end)
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay 'org-typst-preview t)
    (overlay-put overlay 'org-typst-preview-error t)
    (overlay-put overlay 'face 'org-typst-preview-error)
    (overlay-put overlay 'help-echo message)
    (overlay-put overlay 'evaporate t)
    overlay))

(defun org-typst-preview--waiter-valid-p (waiter)
  "Return non-nil when WAITER still refers to its original source text."
  (let ((buffer (org-typst-preview--waiter-buffer waiter))
        (begin (org-typst-preview--waiter-begin waiter))
        (end (org-typst-preview--waiter-end waiter)))
    (and (buffer-live-p buffer)
         (marker-buffer begin)
         (marker-buffer end)
         (with-current-buffer buffer
           (let ((begin-position (marker-position begin))
                 (end-position (marker-position end)))
             (and begin-position end-position
                  (<= begin-position end-position)
                  (equal (buffer-substring-no-properties
                          begin-position end-position)
                         (org-typst-preview--waiter-text waiter))))))))

(defun org-typst-preview--display-waiter (waiter image key block-p)
  "Display IMAGE for valid WAITER using cache KEY and BLOCK-P metadata."
  (when (org-typst-preview--waiter-valid-p waiter)
    (with-current-buffer (org-typst-preview--waiter-buffer waiter)
      (let ((begin (marker-position (org-typst-preview--waiter-begin waiter)))
            (end (marker-position (org-typst-preview--waiter-end waiter))))
        (when (fboundp 'clear-image-cache)
          (clear-image-cache image))
        (org-typst-preview--make-image-overlay begin end image key block-p)))))

(defun org-typst-preview--error-waiter (waiter message)
  "Show compiler MESSAGE on a still-valid WAITER."
  (when (org-typst-preview--waiter-valid-p waiter)
    (with-current-buffer (org-typst-preview--waiter-buffer waiter)
      (org-typst-preview--make-error-overlay
       (marker-position (org-typst-preview--waiter-begin waiter))
       (marker-position (org-typst-preview--waiter-end waiter))
       message))))

(defun org-typst-preview--release-waiter (waiter)
  "Detach markers owned by WAITER."
  (set-marker (org-typst-preview--waiter-begin waiter) nil)
  (set-marker (org-typst-preview--waiter-end waiter) nil))

(defun org-typst-preview--append-log (job event output)
  "Append JOB diagnostics, process EVENT, and OUTPUT to the log buffer."
  (with-current-buffer (get-buffer-create org-typst-preview-log-buffer)
    (let ((inhibit-read-only t))
      (unless (derived-mode-p 'special-mode)
        (special-mode))
      (goto-char (point-max))
      (insert (format "\n[%s] %s\n%s\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      (string-trim event)
                      (mapconcat #'shell-quote-argument
                                 (org-typst-preview--job-command job) " ")))
      (unless (string-empty-p output)
        (insert output)
        (unless (bolp) (insert "\n"))))))

(defun org-typst-preview--cleanup-job (job)
  "Remove temporary resources associated with JOB."
  (when-let* ((source-file (org-typst-preview--job-source-file job)))
    (when (file-exists-p source-file)
      (delete-file source-file)))
  (when-let* ((render-file (org-typst-preview--job-render-file job)))
    (when (file-exists-p render-file)
      (delete-file render-file)))
  (when-let* ((process-buffer (org-typst-preview--job-process-buffer job)))
    (when (buffer-live-p process-buffer)
      (kill-buffer process-buffer))))

(defun org-typst-preview--complete-job (job success event output)
  "Complete JOB with SUCCESS, process EVENT, and compiler OUTPUT."
  (unless (org-typst-preview--job-finished job)
    (setf (org-typst-preview--job-finished job) t)
    (when (org-typst-preview--job-started job)
      (setq org-typst-preview--running
            (max 0 (1- org-typst-preview--running))))
    (remhash (org-typst-preview--job-key job) org-typst-preview--jobs)
    (let ((waiters (org-typst-preview--job-waiters job))
          (target (org-typst-preview--job-target job))
          (key (org-typst-preview--job-key job)))
      (if (and success (org-typst-preview--valid-cache-file-p target))
          (dolist (waiter waiters)
            (org-typst-preview--display-waiter
             waiter target key (org-typst-preview--waiter-block waiter)))
        (let ((message (format "Typst preview failed; see %s"
                               org-typst-preview-log-buffer)))
          (org-typst-preview--append-log job event output)
          (dolist (waiter waiters)
            (org-typst-preview--error-waiter waiter message))))
      (mapc #'org-typst-preview--release-waiter waiters))
    (org-typst-preview--cleanup-job job)
    (org-typst-preview--drain-queue)))

(defun org-typst-preview--sentinel (process event)
  "Handle Typst PROCESS completion described by EVENT."
  (when (memq (process-status process) '(exit signal failed))
    (when-let* ((job (process-get process 'org-typst-preview-job)))
      (let ((output
             (if-let* ((buffer (process-buffer process)))
                 (with-current-buffer buffer
                   (buffer-substring-no-properties (point-min) (point-max)))
               ""))
            (success (and (eq (process-status process) 'exit)
                          (zerop (process-exit-status process)))))
        (when success
          (condition-case error-data
              (if (org-typst-preview--valid-cache-file-p
                   (org-typst-preview--job-render-file job))
                  (rename-file
                   (org-typst-preview--job-render-file job)
                   (org-typst-preview--job-target job) t)
                (setq success nil
                      output (concat output "\nTypst produced no valid SVG.")))
            (error
             (setq success nil
                   output
                   (concat output "\n"
                           (error-message-string error-data))))))
        (org-typst-preview--complete-job
         job success event output)))))

(defun org-typst-preview--start-job (job)
  "Start asynchronous Typst preview JOB."
  (condition-case error-data
      (let* ((source-directory
              (file-name-as-directory
               (org-typst-preview--job-source-directory job)))
             (temporary-file-directory source-directory)
             (source-file (make-temp-file ".org-typst-preview-" nil ".typ"))
             (process-buffer (generate-new-buffer " *org-typst-preview*")))
        (make-directory
         (file-name-directory (org-typst-preview--job-target job)) t)
        (let* ((temporary-file-directory
                (file-name-directory (org-typst-preview--job-target job)))
               (render-file
                (make-temp-file ".org-typst-preview-render-" nil ".svg"))
               (command
                (append (list org-typst-preview-executable)
                        org-typst-preview-process-arguments
                        (list source-file render-file))))
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file source-file
              (insert (org-typst-preview--job-source job))))
          (setf (org-typst-preview--job-source-file job) source-file
                (org-typst-preview--job-render-file job) render-file
                (org-typst-preview--job-process-buffer job) process-buffer
                (org-typst-preview--job-command job) command
                (org-typst-preview--job-started job) t)
          (cl-incf org-typst-preview--running)
          (let ((process
                 (make-process
                  :name (format "org-typst-preview-%s"
                                (substring
                                 (secure-hash 'sha1
                                              (org-typst-preview--job-key job))
                                 0 10))
                  :buffer process-buffer
                  :command command
                  :coding 'utf-8-unix
                  :connection-type 'pipe
                  :noquery t
                  :sentinel #'org-typst-preview--sentinel)))
            (setf (org-typst-preview--job-process job) process)
            (process-put process 'org-typst-preview-job job))))
    (error
     (org-typst-preview--complete-job
      job nil "failed to start"
      (error-message-string error-data)))))

(defun org-typst-preview--drain-queue ()
  "Start queued jobs while the concurrency limit permits."
  (while (and org-typst-preview--queue
              (< org-typst-preview--running
                 (max 1 org-typst-preview-max-processes)))
    (let ((job (pop org-typst-preview--queue)))
      (unless (org-typst-preview--job-finished job)
        (org-typst-preview--start-job job)))))

(defun org-typst-preview--matching-overlay (begin end key)
  "Return an existing preview between BEGIN and END matching KEY."
  (cl-find-if
   (lambda (overlay)
     (equal (overlay-get overlay 'org-typst-preview-key) key))
   (org-typst-preview--our-overlays begin end)))

(defun org-typst-preview--schedule-fragment (fragment)
  "Schedule asynchronous rendering of FRAGMENT.

Return one of `existing', `cached', `joined', or `queued'."
  (let* ((begin (plist-get fragment :begin))
         (end (plist-get fragment :end))
         (value (plist-get fragment :value))
         (face (face-at-point begin))
         (source (org-typst-preview--render-source value face))
         (hash (org-typst-preview--cache-key source))
         (directory (org-typst-preview--image-directory))
         (target (expand-file-name (format "org-typst-%s.svg" hash)
                                   directory))
         (key target)
         (text (buffer-substring-no-properties begin end))
         (waiter
          (org-typst-preview--make-waiter
           :buffer (current-buffer)
           :begin (copy-marker begin)
           :end (copy-marker end t)
           :text text
           :block (plist-get fragment :block))))
    (cond
     ((org-typst-preview--matching-overlay begin end key)
      (org-typst-preview--release-waiter waiter)
      'existing)
     ((org-typst-preview--valid-cache-file-p target)
      (org-typst-preview--display-waiter
       waiter target key (plist-get fragment :block))
      (org-typst-preview--release-waiter waiter)
      'cached)
     ((gethash key org-typst-preview--jobs)
      (let ((job (gethash key org-typst-preview--jobs)))
        (push waiter (org-typst-preview--job-waiters job)))
      'joined)
     (t
      (org-typst-preview--remove-overlays begin end)
      (let ((job
             (org-typst-preview--make-job
              :key key
              :source source
              :target target
              :source-directory (org-typst-preview--source-directory)
              :waiters (list waiter))))
        (puthash key job org-typst-preview--jobs)
        (setq org-typst-preview--queue
              (nconc org-typst-preview--queue (list job)))
        (org-typst-preview--drain-queue))
      'queued))))

(defun org-typst-preview--preview-region (begin end)
  "Schedule previews for Typst math fragments from BEGIN through END.

Return an alist counting the scheduling outcomes."
  (unless (org-typst-preview--safe-to-run-p)
    (user-error "Typst preview disabled for untrusted Org content"))
  (org-typst-preview--check-executable)
  (let ((counts nil))
    (dolist (fragment (org-typst-preview--collect-fragments begin end))
      (let ((result (org-typst-preview--schedule-fragment fragment)))
        (setf (alist-get result counts) (1+ (alist-get result counts 0)))))
    counts))

(defun org-typst-preview--cancel-waiters (buffer begin end)
  "Cancel pending waiters in BUFFER overlapping BEGIN and END."
  (let (drop)
    (maphash
     (lambda (_key job)
       (setf (org-typst-preview--job-waiters job)
             (cl-remove-if
              (lambda (waiter)
                (let ((waiter-buffer
                       (org-typst-preview--waiter-buffer waiter))
                      (waiter-begin
                       (marker-position
                        (org-typst-preview--waiter-begin waiter)))
                      (waiter-end
                       (marker-position
                        (org-typst-preview--waiter-end waiter))))
                  (when (and (eq waiter-buffer buffer)
                             waiter-begin waiter-end
                             (< waiter-begin end)
                             (< begin waiter-end))
                    (org-typst-preview--release-waiter waiter)
                    t)))
              (org-typst-preview--job-waiters job)))
       (when (and (null (org-typst-preview--job-waiters job))
                  (not (org-typst-preview--job-started job)))
         (push job drop)))
     org-typst-preview--jobs)
    (dolist (job drop)
      (setq org-typst-preview--queue
            (delq job org-typst-preview--queue))
      (remhash (org-typst-preview--job-key job) org-typst-preview--jobs)
      (setf (org-typst-preview--job-finished job) t)
      (org-typst-preview--cleanup-job job))))

;;;###autoload
(defun org-typst-preview-clear (&optional begin end)
  "Remove Typst preview overlays and pending requests.

When BEGIN and END are non-nil, affect only that range.  Return the removed
overlays."
  (interactive)
  (let ((begin (or begin (point-min)))
        (end (or end (point-max))))
    (org-typst-preview--cancel-waiters (current-buffer) begin end)
    (org-typst-preview--remove-overlays begin end)))

(defun org-typst-preview--section-bounds ()
  "Return the bounds of the current Org section."
  (list
   (if (org-before-first-heading-p)
       (point-min)
     (save-excursion
       (org-with-limited-levels
        (org-back-to-heading t)
        (point))))
   (org-with-limited-levels (org-entry-end-position))))

(defun org-typst-preview--message-counts (scope counts)
  "Report scheduling COUNTS for SCOPE."
  (let ((total (apply #'+ (mapcar #'cdr counts)))
        (queued (+ (alist-get 'queued counts 0)
                   (alist-get 'joined counts 0)))
        (cached (alist-get 'cached counts 0))
        (existing (alist-get 'existing counts 0)))
    (if (zerop total)
        (message "No Typst math fragments in %s" scope)
      (message "Typst previews in %s: %d total, %d async, %d cached, %d unchanged"
               scope total queued cached existing))))

;;;###autoload
(defun org-typst-preview (&optional argument)
  "Toggle native Typst preview with `org-latex-preview' semantics.

At a Typst math fragment, toggle that fragment.  With an active region,
preview all fragments in the region.  Otherwise preview the current section.

With one universal prefix ARGUMENT, clear the current section (or active
region).  With two universal prefixes, preview the whole buffer.  With three
universal prefixes, clear the whole buffer."
  (interactive "P")
  (cond
   ((not (display-graphic-p)) nil)
   ((not (org-typst-preview--safe-to-run-p))
    (message "Typst preview disabled for untrusted Org content"))
   ((equal argument '(64))
    (org-typst-preview-clear (point-min) (point-max))
    (message "Typst previews removed from buffer"))
   ((equal argument '(16))
    (org-typst-preview--message-counts
     "buffer"
     (org-typst-preview--preview-region (point-min) (point-max))))
   ((equal argument '(4))
    (pcase-let ((`(,begin ,end)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (org-typst-preview--section-bounds))))
      (org-typst-preview-clear begin end)
      (message "Typst previews removed from %s"
               (if (use-region-p) "region" "section"))))
   ((use-region-p)
    (org-typst-preview--message-counts
     "region"
     (org-typst-preview--preview-region
      (region-beginning) (region-end))))
   ((when-let* ((fragment (org-typst-preview--fragment-at-point)))
      (let ((begin (plist-get fragment :begin))
            (end (plist-get fragment :end)))
        (if (org-typst-preview--our-overlays begin end)
            (progn
              (org-typst-preview-clear begin end)
              (message "Typst preview removed"))
          (org-typst-preview--check-executable)
          (let ((result (org-typst-preview--schedule-fragment fragment)))
            (message (pcase result
                       ('cached "Typst preview loaded from cache")
                       ('existing "Typst preview already current")
                       (_ "Typst preview scheduled")))))
        t)))
   (t
    (pcase-let ((`(,begin ,end) (org-typst-preview--section-bounds)))
      (org-typst-preview--message-counts
       "section" (org-typst-preview--preview-region begin end))))))

;;;###autoload
(defvar org-typst-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-latex-preview] #'org-typst-preview)
    (define-key map [remap org-toggle-latex-fragment] #'org-typst-preview)
    (define-key map [remap org-clear-latex-preview] #'org-typst-preview-clear)
    map)
  "Keymap for `org-typst-preview-mode'.")

;;;###autoload
(define-minor-mode org-typst-preview-mode
  "Use Typst for Org's existing LaTeX preview commands and bindings.

This mode remaps commands only.  It does not alter Org parsing or export."
  :lighter " Typst∑"
  :keymap org-typst-preview-mode-map)

(defun org-typst-preview--auto-run (buffer marker)
  "Render the fragment near MARKER in BUFFER after an idle delay."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq org-typst-preview--auto-timer nil)
      (when (and org-typst-preview-auto-mode
                 (marker-position marker)
                 (org-typst-preview--safe-to-run-p))
        (save-excursion
          (goto-char marker)
          (when-let* ((fragment (org-typst-preview--fragment-at-point)))
            (condition-case error-data
                (org-typst-preview--schedule-fragment fragment)
              (error
               (message "Typst auto-preview: %s"
                        (error-message-string error-data))))))))
    (set-marker marker nil)))

(defun org-typst-preview--after-change (begin end _old-length)
  "Schedule auto-preview after a change between BEGIN and END."
  (when org-typst-preview--auto-timer
    (cancel-timer org-typst-preview--auto-timer))
  (let* ((range-begin (save-excursion
                        (goto-char begin)
                        (line-beginning-position)))
         (range-end (save-excursion
                      (goto-char end)
                      (min (point-max) (1+ (line-end-position))))))
    (org-typst-preview-clear range-begin range-end))
  (setq org-typst-preview--auto-timer
        (run-with-idle-timer
         org-typst-preview-auto-delay nil
         #'org-typst-preview--auto-run
         (current-buffer) (copy-marker begin t))))

;;;###autoload
(define-minor-mode org-typst-preview-auto-mode
  "Automatically render an edited Typst math fragment after an idle delay."
  :lighter " Typst↻"
  (if org-typst-preview-auto-mode
      (progn
        (org-typst-preview-mode 1)
        (add-hook 'after-change-functions
                  #'org-typst-preview--after-change nil t))
    (remove-hook 'after-change-functions
                 #'org-typst-preview--after-change t)
    (when org-typst-preview--auto-timer
      (cancel-timer org-typst-preview--auto-timer)
      (setq org-typst-preview--auto-timer nil))))

;;;###autoload
(defun org-typst-preview-refresh-buffer ()
  "Clear and asynchronously rebuild all Typst previews in this buffer."
  (interactive)
  (org-typst-preview-clear (point-min) (point-max))
  (org-typst-preview--message-counts
   "buffer"
   (org-typst-preview--preview-region (point-min) (point-max))))

;;;###autoload
(defun org-typst-preview-show-errors ()
  "Display the Typst preview diagnostics buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create org-typst-preview-log-buffer)))

(provide 'org-typst-preview)

;;; org-typst-preview.el ends here
