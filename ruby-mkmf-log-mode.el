;; ruby-mkmf-log-mode.el --- Major mode for viewing Ruby mkmf.log files ;; -*- lexical-binding: t -*-
;;
;; Authors: Nobuyoshi Nakada
;; Created: Mon Jan  9 21:58:19 JST 2023

;;; Code:

(require 'cl-lib)
(require 'compile) ; for faces

(defgroup ruby-mkmf-log nil
  "View mode for Ruby mkmf.log"
  :prefix "ruby-mkmf-log-"
  :group 'languages)

(defconst ruby-mkmf-log-methods
  '("convertible_int" "what_type?"  "pkg_config")
  "Method names defined in mkmf.rb and shown at messages in
addition to have_/check_/find_ methods")

(defconst ruby-mkmf-log-methods-re
  (concat
   "^\\(?:"
   "\\(?:"
   "\\(?:test_\\|have_\\|check_\\|find_\\)[_a-zA-Z0-9]*\\??\\|"
   (regexp-opt ruby-mkmf-log-methods)
   "\\)"
   ": \\)?checking .*?\\(?: in \\(.*\\)\\)?\\.\\.\\. \\(--------------------\\) \\(.*\\)")
  "Regexp to match the beginning of each checks")

(defconst ruby-mkmf-log-end-re
  "\n\n--------------------\n$"
  "Regexp to match the end of each checks")

(defconst ruby-mkmf-log-hidden-mark "…" "Mark when hiding check content")
(defconst ruby-mkmf-log-visible-mark "" "Mark when showing check content")

(defcustom ruby-mkmf-log-check-heading-face
  '(:weight bold)
  "Alist of face for check heading"
  :type '(face)
  :group 'ruby-mkmf-log-mode)
(defcustom ruby-mkmf-log-check-content-face
  '(:height 0.8)
  "Alist of face for check content"
  :type '(face)
  :group 'ruby-mkmf-log-mode)

(defun ruby-mkmf-log-toggle-visible ()
  "Toggle visibility of the current check"
  (interactive)
  (save-excursion
    (let ((ovs (overlays-at (point))))
      (mapcar
       (lambda (o)
	 (setq o (overlay-get o 'hide-ruby-mkmf-log))
	 (if (overlayp o)
	     (let ((visible (overlay-get o 'invisible)))
	       (overlay-put o 'invisible (not visible))
	       ;; (overlay-put o 'before-string
	       ;; 		    (if visible ruby-mkmf-log-visible-mark ruby-mkmf-log-hidden-mark))
	       )))
       ovs))))

(defvar ruby-mkmf-log-overlay-keymap (make-keymap) "Keymap to toggle checks")
(define-key ruby-mkmf-log-overlay-keymap "\r" 'ruby-mkmf-log-toggle-visible)
(define-key ruby-mkmf-log-overlay-keymap "\t" 'ruby-mkmf-log-toggle-visible)

(defun ruby-mkmf-log-hide-clear (start end)
  "Clear existing overlays for ruby-mkmf-log"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (mapcar
   (lambda (o)
     (if (overlay-get o 'hide-ruby-mkmf-log)
	 (delete-overlay o)))
   (overlays-in start end)))

(defun ruby-mkmf-log-make-overlay (ov start end &optional front-advance rear-advance no-keymap)
  (let ((o (make-overlay start end nil front-advance rear-advance)))
    (overlay-put o 'category 'ruby-mkmf-log)
    (overlay-put o 'hide-ruby-mkmf-log (or ov o))
    (unless no-keymap
      (overlay-put o 'keymap ruby-mkmf-log-overlay-keymap))
    o))

(defun ruby-mkmf-log-decorate (&optional start end)
  "Decorate Ruby mkmf checks"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (save-excursion
    (or end (setq end (point-max)))
    (goto-char (or start (point-min)))
    (ruby-mkmf-log-hide-clear (point) end)
    (let (s b e b1 e1 b2 e2 bh eh o result ov)
      (while (and (setq b (re-search-forward ruby-mkmf-log-methods-re end t)
			bh (match-beginning 1) eh (match-end 1)
			b1 (match-beginning 2) e1 (match-end 2)
			b2 (match-beginning 3) e2 (match-end 3)
			result (match-string 3)
			s (point-at-bol))
		  (setq e (re-search-forward ruby-mkmf-log-end-re end t)))
	(setq ov (ruby-mkmf-log-make-overlay nil b e t))
	(overlay-put ov 'face ruby-mkmf-log-check-content-face)
	;; (overlay-put ov 'before-string ruby-mkmf-log-hidden-mark)
	(overlay-put ov 'invisible 'hide-ruby-mkmf-log)

	(setq o (ruby-mkmf-log-make-overlay ov s b))
	(overlay-put o 'face ruby-mkmf-log-check-heading-face)
	(when bh
	    (setq o (ruby-mkmf-log-make-overlay t bh eh))
	    (overlay-put o 'before-string ruby-mkmf-log-hidden-mark)
	    (overlay-put o 'help-echo (match-string 1))
	    (overlay-put o 'invisible 'hide-ruby-mkmf-log)
	    )
	(setq o (ruby-mkmf-log-make-overlay ov b1 e1 nil nil t))
	(overlay-put o 'face ruby-mkmf-log-check-content-face)
	(setq o (ruby-mkmf-log-make-overlay ov b2 e2 nil nil t))
	(let ((face
	       (cond
		((string= result "yes") 'compilation-info)
		((string= result "no") 'compilation-error)
		(t 'compilation-warning))))
	 (overlay-put o 'face face))
	))
    (when (re-search-forward "^\\(extconf\\.h\\) is:\n/\\* begin \\*/\n\\(\\(?:.*\n\\)*\\)/\\* end \\*/$" end t)
      (let ((o (ruby-mkmf-log-make-overlay t (match-beginning 1) (match-end 1))))
	(overlay-put o 'face 'compilation-info))
      (let ((b (match-beginning 2)) (e (match-end 2)))
	(goto-char b)
	(while (re-search-forward "^ *[1-9][0-9]*: " e t)
	  (let ((o (ruby-mkmf-log-make-overlay t (match-beginning 0) (match-end 0))))
	    (overlay-put o 'face 'compilation-line-number)))))
    ))

;;;###autoload
(define-derived-mode ruby-mkmf-log-mode text-mode "MKMF"
  "Major mode for Ruby mkmf.log"
  (ruby-mkmf-log-decorate)
  (read-only-mode))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "/mkmf\\.log\\'" 'ruby-mkmf-log-mode))

(provide 'ruby-mkmf-log-mode)
