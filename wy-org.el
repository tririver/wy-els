(setq org-confirm-babel-evaluate nil)

(setq org-export-html-validation-link nil)
(setq org-support-shift-select t)
(setq org-replace-disputed-keys t)
;; (setq org-todo-keywords
;;       '((sequence "?" "➜" "⌛" "|" "✘" "♨" "✔")))

;; (setq org-todo-keyword-faces
;;       (quote (("?" :foreground "red" :weight bold)
;;               ("➜" :foreground "magenta" :weight bold)
;;               ("✔" :foreground "forest green" :weight bold)
;;               ("⌛" :foreground "chocolate" :weight bold)
;;               ("♨" :foreground "dim grey" :weight bold)
;;               ("✘" :foreground "dim grey" :weight bold)
;;               )))

(setq org-todo-keywords
      '((sequence "[TODO]" "[PROG]" "[WAIT]" "|" "[PEND]" "[CANC]" "[DONE]")))

(setq org-todo-keyword-faces
      (quote (("[TODO]" :foreground "red" :weight bold)
              ("[PROG]" :foreground "magenta" :weight bold)
              ("[DONE]" :foreground "forest green" :weight bold)
              ("[WAIT]" :foreground "chocolate" :weight bold)
              ("[PEND]" :foreground "dim grey" :weight bold)
              ("[CANC]" :foreground "dim grey" :weight bold)
              )))


(eval-after-load "org"
    '(progn
       (delete '("\\.pdf\\'" . default) org-file-apps)
       (add-to-list 'org-file-apps '("\\.pdf\\'" . "okular %s"))
       (add-to-list 'org-file-apps '("\\.djvu\\'" . "okular %s"))
       (add-to-list 'org-file-apps '(directory . "dolphin %s"))
       (add-to-list 'org-file-apps '("\\.xls\\'" . "libreoffice %s"))
       (add-to-list 'org-file-apps '("\\.nb\\'" . "Mathematica %s"))
       ))

(setq revert-without-query (quote (".*.org")))

(setq org-src-fontify-natively t)

(add-hook 'org-mode-hook 'wy-soft-wrap-lines)

;; (org-remember-insinuate)

(defun wy-soft-wrap-lines ()
  (setq truncate-lines nil)
  (setq word-wrap t))

(defun wy-org-to-texpdf ()
  "Export org file to latex pdf and view."
  (interactive)
  (org-latex-export-to-pdf)
  ;; assume extension is 3-char long (either txt or org)
  (call-process "okular" nil 0 nil
		(concat (substring (buffer-file-name) 0 -3) "pdf"))
)

;;SET EMACS AS DEFAULT MAJOR MODE TO FOR ALL FILES WITH AN UNSPECIFIED MODE
(setq default-major-mode 'org-mode)

;; ========= work with yasnippet ============

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (make-variable-buffer-local 'yas/trigger-key)
	    (setq yas/trigger-key [tab])
	    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
	    (define-key yas/keymap [tab] 'yas/next-field)))

;; ============ end of package ==============

(provide 'wy-org)
