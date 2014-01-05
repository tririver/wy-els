(defvar wy-txt-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.txt\\'" . wy-txt-mode))


(define-derived-mode wy-txt-mode org-mode "Text document"
  "wy-txt mode is a major mode for txt, derived from org-mode."
  (setq mode-name "wy-txt")

;  (visual-line-mode)
;  (setq-default word-wrap nil)

  (defun wy-region-is-narrow () (> (1+ (buffer-size)) (point-max)) )

  (defun wy-mark-section-at-cursor ()
    "Set mark at beginning of section, and cursor at end of section."
    (re-search-backward "^\\*+ " nil 0)
    (set-mark (point))
    (goto-char (1+ (point)))
    (if (re-search-forward "^\\*+ " nil 0)
	(re-search-backward "^\\*+ " nil 0)))
  
  (defun wy-narrow-to-section () 
    "Narrow edit to current section or subsection, etc, at cursor."
    (interactive)
    (save-excursion
      (wy-mark-section-at-cursor)
      (narrow-to-region (point) (mark))))

  (defun wy-toggle-narrow-to-section ()
    (interactive)
    (if (wy-region-is-narrow) (widen) (wy-narrow-to-section)))

  (defun wy-count-word-section ()
    (interactive)
    (save-excursion
      (wy-mark-section-at-cursor)
      (- (point) (mark))))

  (local-set-key (quote [f1]) 'wy-toggle-narrow-to-section)

  (setq mode-line-format nil)
  (setq frame-title-format 
    '("%b  %*  " 
      (:eval (number-to-string (wy-count-word-section))) 
      " / %i" "%n"
      ))

  (defadvice previous-line (after wy-mode-line-update activate)
    (force-mode-line-update))
  (defadvice next-line (after wy-mode-line-update activate)
    (force-mode-line-update))
  (defadvice scroll-up-command (after wy-mode-line-update activate)
    (force-mode-line-update))
  (defadvice scroll-down-command (after wy-mode-line-update activate)
    (force-mode-line-update))


)

(provide 'wy-txt)
