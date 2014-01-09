;; ============= positioning ================

(defun wy-rl-beginning () ; "rl" denotes "region or line"
  (if mark-active (region-beginning) (line-beginning-position)))

(defun wy-rl-end () ; "rl" denotes "region or line"
  (if mark-active (region-end) (line-end-position)))

(defun wy-get-rl ()
  (buffer-substring-no-properties (wy-rl-beginning) (wy-rl-end)) )

;; ================= edit ===================

(defun wy-ins-comment (s) 
  "insert a comment line with separators" 
  (interactive "sName of separator:") 
  (let ((str (if (> (length s) 30) (substring s 0 30) s ) ) ;; crop if too long
	(len) (len-s1) (len-s2) (spc-filler)
	(filler "=") (tot-len 40)) ;; default symbol and length
    (setq len (length str)
	  len-s1 (/ (- tot-len len 2) 2)
	  len-s2 (- tot-len (length str) len-s1)
	  spc-filler (if (> len 0) " " filler))
    (dotimes (i len-s1) (insert filler))
    (insert spc-filler)
    (insert str)
    (insert spc-filler)
    (dotimes (i len-s2) (insert filler))
    (comment-region (line-beginning-position) (line-end-position))
    (insert "\n")
    ))


(defun wy-toggle-comment-rl ()
  (interactive)
  (copy-region-as-kill (wy-rl-beginning) (wy-rl-end))
  (comment-or-uncomment-region (wy-rl-beginning) (wy-rl-end))
  (next-line))


(defun wy-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun wy-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun wy-trim (s)
  "Remove whitespace at the beginning and end of S."
  (wy-trim-left (wy-trim-right s)))

(defun wy-remove-comment (s)
  "Remove comment symbol (on the left only)."
  (if (string-match (concat "\\`[ \t\n\r]*" comment-start "+") s)
      (replace-match "" t t s)
    s))

;; ================= file ===================

(defun wy-rename-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
	(save-buffer)
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

;; ========== eval external code ============

(defun wy-math ()
  (interactive)
  (let ((res-str) (code (concat "/home/wangyi/Copy/Code/Meval.sh '" 
			    (wy-get-rl) "'")))
    (setq res-str (shell-command-to-string code))
    (kill-new res-str)
    (princ res-str) ))

(defun wy-math2 ()
  (interactive)
  (let ((expr) (code) (res))
    (setq expr (wy-remove-comment (wy-get-rl)))
    (setq code (concat "echo 'Print[" expr "]'>/tmp/Meval2.tmp.m;"
		       "MathematicaScript -script /tmp/Meval2.tmp.m"))
    (setq res (wy-trim (shell-command-to-string code)))
    (kill-new res)
    (princ res)))

(defun wy-screenshot (s)
  "Input file name and take a screen shot with this file name. An org link is inserted into the current buffer."
  (interactive "sName of file:")
  (let ((fn (if (string= s "") 
		(concat (buffer-file-name) "_" (format-time-string "%Y%m%d_%H%M%S") ".png") 
	      (concat s ".png"))))
    (call-process "import" nil nil nil fn)
    (insert (concat "[[file:" fn "]]"))))

;; =========== end of package =============

(provide 'wy-utils)
