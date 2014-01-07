(defvar wy-tex-mode-hook nil)
(add-to-list 'auto-mode-alist '("\\.tex\\'" . wy-tex-mode))

(define-derived-mode wy-tex-mode latex-mode "LaTeX document"
  "wy-tex mode is a major mode for LaTeX, derived from LaTeX-mode."
  (setq mode-name "wyTeX")

;; =========== start of package =============

;; ========== set mode and vars =============

  (reftex-mode)
  (visual-line-mode)
  (setq frame-title-format "%b  %*  %i %n")
  (setq mode-line-format nil)

;; ============= key settings ===============

  (defun wy-tex-ins (s)
    (if mark-active
	(progn (kill-region (region-beginning) (region-end))
	       (insert (concat s "{" (current-kill 0) "}")))
        (progn (insert (concat s "{}"))
	       (left-char 1)) ))

  (local-set-key (kbd "C--") (lambda () (interactive) (wy-tex-ins "_")))
  (local-set-key (kbd "C-6") (lambda () (interactive) (wy-tex-ins "^")))
  (local-set-key (kbd "C-2") (lambda () (interactive) (wy-tex-ins "\\sqrt")))
  (local-set-key (kbd "C-b") (lambda () (interactive) (wy-tex-ins "\\mathbf")))
  (local-set-key (kbd "C-t") (lambda () (interactive) (wy-tex-ins "\\mathrm")))

;; =========== insertion setup ==============

  (defun wy-tex-key (utf8-char tex-command)
        (local-set-key utf8-char 
		       `(lambda () (interactive) (insert ,tex-command))) 
	;; add font-lock like \alpha x -> αx (note have space)
	(font-lock-add-keywords
	 'wy-tex-mode `((,(concat "\\(\\" tex-command " \\)[a-zA-Z0-9]")
			(0 (progn (compose-region (match-beginning 1)
						  (match-end 1) ,utf8-char)
				  nil)))))
	;; add font-lock like \alpha\beta -> αβ (note do not have space)
	(font-lock-add-keywords
	 'wy-tex-mode `((,(concat "\\(\\" tex-command "\\)[^a-zA-Z0-9]")
			(0 (progn (compose-region (match-beginning 1)
						  (match-end 1) ,utf8-char)
				  nil)))))
  )

  ;; One trick is that, font-lock use regexp instead of string
  ;; and the additional \\ for \ is prepared in concat.
  (wy-tex-key "𝕋" "\\title")  
  (wy-tex-key "𝕓" "\\mathbf")
  (wy-tex-key "𝕣" "\\mathrm")
  (wy-tex-key "𝕝" "\\label")
  (wy-tex-key "§" "\\section")
  (wy-tex-key "◯" "\\subsection")
  (wy-tex-key "⟦" "\\begin{equation}")
  (wy-tex-key "⟧" "\\end{equation}")
  (wy-tex-key "⟦" "\\begin{align}")
  (wy-tex-key "⟧" "\\end{align}")



  (wy-tex-key "‴" "\\dddot")
  (wy-tex-key "″" "\\ddot")
  (wy-tex-key "°" "\\dot")

  (wy-tex-key "γ" "\\gamma")
  (wy-tex-key "Ξ" "\\Xi")
  (wy-tex-key "Λ" "\\Lambda")
  (wy-tex-key "Γ" "\\Gamma")
  (wy-tex-key "Φ" "\\Phi")
  (wy-tex-key "Δ" "\\Delta")
  (wy-tex-key "Σ" "\\Sigma")
  (wy-tex-key "Π" "\\Pi")
  (wy-tex-key "∇" "\\nabla")
  (wy-tex-key "Ψ" "\\Psi")
  (wy-tex-key "Ω" "\\Omega")
  (wy-tex-key "µ" "\\mu")
  (wy-tex-key "ν" "\\nu")
  (wy-tex-key "β" "\\beta")
  (wy-tex-key "χ" "\\chi")
  (wy-tex-key "ξ" "\\xi")
  (wy-tex-key "ζ" "\\zeta")
  (wy-tex-key "λ" "\\lambda")
  (wy-tex-key "κ" "\\kappa")
  (wy-tex-key "η" "\\eta")
  (wy-tex-key "φ" "\\phi")
  (wy-tex-key "δ" "\\delta")
  (wy-tex-key "σ" "\\sigma")
  (wy-tex-key "α" "\\alpha")
  (wy-tex-key "π" "\\pi")
  (wy-tex-key "∂" "\\partial")
  (wy-tex-key "∫" "\\int")
  (wy-tex-key "ψ" "\\psi")
  (wy-tex-key "τ" "\\tau")
  (wy-tex-key "ρ" "\\rho")
  (wy-tex-key "ε" "\\epsilon")
  (wy-tex-key "ω" "\\omega")
  (wy-tex-key "θ" "\\theta")
  (wy-tex-key "→" "\\rightarrow")
  (wy-tex-key "←" "\\leftarrow")
  (wy-tex-key "⇒" "\\Rightarrow")
  (wy-tex-key "⇔" "\\Leftrightarrow")
  (wy-tex-key "⁰" "^0")
  (wy-tex-key "¹" "^1")
  (wy-tex-key "²" "^2")
  (wy-tex-key "³" "^3")
  (wy-tex-key "⁴" "^4")
  (wy-tex-key "⁵" "^5")
  (wy-tex-key "⁶" "^6")
  (wy-tex-key "⁷" "^7")
  (wy-tex-key "⁸" "^8")
  (wy-tex-key "⁹" "^9")
;  (wy-tex-key "⁻" "^-")
;  (wy-tex-key "⁺" "^+")
  (wy-tex-key "†" "\\dagger")
  (wy-tex-key "½" "\\frac{1}{2}")
  (wy-tex-key "⅓" "\\frac{1}{3}")
  (wy-tex-key "¼" "\\frac{1}{4}")
  (wy-tex-key "⟨" "\\langle")
  (wy-tex-key "⟩" "\\rangle")
  (wy-tex-key "∞" "\\infty")
  (wy-tex-key "±" "\\pm")
  (wy-tex-key "∑" "\\sum")
  (wy-tex-key "∏" "\\prod")
  (wy-tex-key "⊥" "\\perp")
  (wy-tex-key "‖" "\\parallel")
  (wy-tex-key "∙" "\\cdot")
  (wy-tex-key "⋯" "\\cdots")
  (wy-tex-key "≥" "\\geq")
  (wy-tex-key "≤" "\\leq")
  (wy-tex-key "≪" "\\ll")
  (wy-tex-key "≫" "\\gg")
  (wy-tex-key "∝" "\\propto")
  (wy-tex-key "⊃" "\\supset")
  (wy-tex-key "ħ" "\\hbar")
  (wy-tex-key "≡" "\\equiv")
  (wy-tex-key "≠" "\\neq")
  (wy-tex-key "⅟" "\\frac{1}")
  (wy-tex-key "❨" "\\left(")
  (wy-tex-key "❩" "\\right)")
  (wy-tex-key "⎡" "\\left\\[")
  (wy-tex-key "⎦" "\\right\\]")
  (wy-tex-key "⦃" "\\left\\\\{")
  (wy-tex-key "⦄" "\\right\\\\}")
  (wy-tex-key "√" "\\sqrt")
  (wy-tex-key "⨯" "\\times")


;; ============== narrowing =================

  (save-excursion
    (goto-char (point-min))
    (search-forward "\\begin{document}\n")
    (set-mark (point))
    (search-forward "\\end{document}")
    (goto-char (- (point) 15))
    (narrow-to-region (point) (mark))
    )



;; ============ end of package ==============
)
(provide 'wy-tex)
