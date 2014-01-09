;; ============ function keys ===============
(global-set-key (quote [f2]) 'wy-rename-file)
(global-set-key (quote [f3]) (lambda ()   (interactive) (toggle-word-wrap) ))
(global-set-key (quote [f8]) 'ucs-insert)
(global-set-key (quote [f9]) 'flyspell-buffer)
(global-set-key (quote [f10]) 'ispell-word)
(global-set-key (quote [f11]) 'count-words)

;; ================= edit ===================

(global-set-key (kbd "C-=") 'wy-ins-comment)
(global-set-key (kbd "M-/") 'wy-toggle-comment-rl)
(global-set-key (kbd "M-8") 'ucs-insert)
(global-set-key (kbd "M-a") 'mark-whole-buffer)

;; ============== movements =================

(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "M-j") (lambda () (interactive) 
			       (scroll-up 1) (forward-line 1)))
(global-set-key (kbd "M-k") (lambda () (interactive) 
			       (scroll-up -1) (forward-line -1)))

(global-set-key (kbd "<S-menu>") 'point-to-register)
(global-set-key (kbd "<menu>") 'register-to-point)
(global-set-key (kbd "<M-menu>") 'goto-last-change)

;; =============== buffers ==================

(global-set-key (kbd "M--") 'previous-buffer)
(global-set-key (kbd "M-=") 'next-buffer)

;; =============== windows ==================

(global-set-key (kbd "M-]") 'other-window)
;; (global-set-key (kbd "M-[") (lambda ()   (interactive) (other-window -1) ))
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-4") 'balance-windows)
(global-set-key (kbd "M-5") 'make-frame-command)
(global-set-key (kbd "M-6") 'compare-windows)

;; ================ macros ==================

(global-set-key (kbd "M-9") 'kmacro-start-macro)
(global-set-key (kbd "M-0") 'kmacro-end-macro)
(global-set-key (kbd "M-8") 'kmacro-end-and-call-macro)
(global-set-key (kbd "M-7") 'kmacro-edit-macro)

;; =============== register =================

(global-set-key (kbd "M-c") 'copy-to-register)
(global-set-key (kbd "M-v") (lambda (ch) (interactive "c") 
			       (insert-register ch t) ))
;; ================ files ===================

(global-set-key (kbd "<M-SPC>") (lambda () (interactive)
			       (save-buffer) (force-mode-line-update)))
(global-set-key (kbd "M-f") 'find-file-at-point)
(global-set-key (kbd "M-F") 'find-file-other-window)

;; ================ others ==================

(global-set-key (kbd "<M-return>") 'repeat)
(global-set-key (kbd "M-?") 'describe-key)
(global-set-key (kbd "M-h") 'describe-function)
(global-set-key (kbd "M-r") 'eval-last-sexp)
(global-set-key (kbd "M-R") 'eval-region)
(global-set-key (kbd "M-\\") 'wy-math2)
(global-set-key (kbd "M-p") 'wy-screenshot)

(add-hook 'org-mode-hook (lambda ()
      (local-set-key (kbd "M-p") 'wy-screenshot)
      (local-set-key (quote [f5]) 'wy-org-to-texpdf)
      (local-set-key (kbd "<f6>") 'org-beamer-export-to-pdf)))

(defun wy-latex-compile ()
  (interactive)
  (save-buffer)
  (call-process "/home/wangyi/Copy/Code/cctex.sh" nil  0 nil buffer-file-name))

(defun wy-latex-checklog ()
  (interactive)
  (find-file (concat "/tmp/cctex_output" (substring buffer-file-name 0 -4) ".log")) )


(add-hook 'LaTeX-mode-hook (lambda ()
      (local-set-key (quote [f5]) 'wy-latex-compile)
      (local-set-key [(shift f5)] 'wy-latex-checklog) ))


;; ================= face ===================

;; (global-set-key (kbd "M-7") (lambda () (interactive) (set-default-font "WenQuanYi Micro Hei") ))
;; (global-set-key (kbd "M-&") (lambda () (interactive) (set-default-font "WenQuanYi Micro Hei Mono") ))

;; ============ mode spetific ===============

(add-hook 'tex-mode-hook (lambda ()
;      (local-set-key (kbd "M-t") 'TeX-command-master)
;      (local-set-key (kbd "M-p") 'preview-document)
      (local-set-key (quote [f5]) (lambda () (interactive)
	(save-buffer)
        (call-process "/home/wangyi/Copy/Code/cctex.sh" nil  0 nil buffer-file-name)
      ))
      (local-set-key [(shift f5)] (lambda () (interactive)
	(find-file (concat "/tmp/cctex_output" (substring buffer-file-name 0 -4) ".log")  )
      ))
))

;; ===============  num pad =================

(global-set-key (kbd "<kp-enter>") 'eval-last-sexp)
(global-set-key (kbd "<kp-delete>") 'wy-math2)

(global-set-key (kbd "<kp-home>") 'kmacro-start-macro)
(global-set-key (kbd "<kp-end>") 'kmacro-end-macro)
(global-set-key (kbd "<kp-insert>") 'kmacro-end-and-call-macro)
(global-set-key (kbd "<kp-left>") 'kmacro-edit-macro)

(global-set-key (kbd "<kp-subtract>") 'previous-buffer)
(global-set-key (kbd "<kp-add>") 'next-buffer)
(global-set-key (kbd "<kp-multiply>") 'other-window)
(global-set-key (kbd "<kp-divide>") (lambda ()   (interactive) (other-window -1) ))

(global-set-key (kbd "<kp-prior>") 'delete-other-windows)
(global-set-key (kbd "<kp-down>") 'split-window-vertically)
(global-set-key (kbd "<kp-next>") 'split-window-horizontally)
(global-set-key (kbd "<kp-up>") 'balance-windows)
(global-set-key (kbd "<kp-begin>") 'make-frame-command)
(global-set-key (kbd "<kp-right>") 'compare-windows)

;; =========== end of package =============

(provide 'wy-keys)
