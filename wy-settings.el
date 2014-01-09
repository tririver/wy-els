;; ================= elpa ===================

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; ================= yas ====================

(require 'yasnippet)
(yas/global-mode 1)

;; ============= emacs chrome ===============

(require 'edit-server)
(edit-server-start)

(add-hook 'edit-server-start-hook (lambda () 
    (org-mode)
    ;; (if (string= (wy-get-rl) "<br>") kill-whole-line)
    (local-set-key (kbd "C-d") '(lambda () 
        (interactive)
    	(mark-whole-buffer)
    	(org-html-convert-region-to-html)
	(goto-char (point-max))
	(insert "Best,<br>Yi")
    	(edit-server-done)))
))



;; ================ backup ==================

(defconst emacs-tmp-dir "//home//wangyi//Others//Tmp//")
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))

;; ================= edit ===================

(show-paren-mode t)

(setq ediff-split-window-function 'split-window-horizontally)

(put 'narrow-to-region 'disabled nil)

;; ============= spell check ================

(setq ispell-program-name "aspell") 
(setq-default ispell-dictionary "en")
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(autoload 'flyspell-delay-command "flyspell" "Delay on command." t) 
(autoload 'tex-mode-flyspell-verify "flyspell" "" t) 
(add-hook 'tex-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

;; =================  etc ===================

(setq visible-bell t)
(add-to-list 'auto-mode-alist '("\\.m\\'" . mma-mode))

;; ============ end of package ==============

(provide 'wy-settings)
