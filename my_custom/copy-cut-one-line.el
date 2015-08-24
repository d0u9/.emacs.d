;; copy-cut-one-line.el

; quick-copy-file
(defun quick-copy-line ()
   "Copy the whole line that point is on and move to the beginning of the next line.
Consecutive calls to this command append each line to the
kill-ring."
   (interactive)
   (let ((beg (line-beginning-position 1))
         (end (line-beginning-position 2)))
     (if (eq last-command 'quick-copy-line)
         (kill-append (buffer-substring beg end) (< end beg))
       (kill-new (buffer-substring beg end))))
   (beginning-of-line 2))
(global-set-key (kbd "C-c y") 'quick-copy-line)

(defun quick-cut-line ()
  "Cut the whole line that point is on.  Consecutive calls to this command append each line to the kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
	(end (line-beginning-position 2)))
    (if (eq last-command 'quick-cut-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end)))
    (delete-region beg end))
  (beginning-of-line 1)
  (setq this-command 'quick-cut-line))
(global-set-key (kbd "C-c d") 'quick-cut-line)

(provide 'copy-cut-one-line)
