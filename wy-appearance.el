;; ========= hide unuseful things ===========

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; ============== appearance ================

(setq frame-title-format "%b  %*  %i %n")
;; (blink-cursor-mode (- (*) (*) (*)))

(blink-cursor-mode nil)

;; (add-to-list 'default-frame-alist (cons 'font "WenQuanYi Micro Hei Mono"))
(column-number-mode)

;; ================ colors ==================

(set-face-background 'region "pink")
(set-face-foreground 'minibuffer-prompt "#777777")

;; (setq wy-background-color "#d6d2d0")
;; (set-face-foreground 'fringe "#b4b2b1")

(setq wy-background-color "#ededed")
(set-face-foreground 'fringe "#cacaca")

(set-face-background 'fringe wy-background-color)
(set-face-background 'mode-line wy-background-color)
(set-face-background 'mode-line-inactive wy-background-color)
(add-to-list 'default-frame-alist (cons 'background-color wy-background-color))

;; =========== end of package =============

(provide 'wy-appearance)
