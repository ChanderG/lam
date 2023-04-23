;; -*- lexical-binding:t -*-

;; obtained from the "Buffer local abbrevs" section from https://www.emacswiki.org/emacs/AbbrevMode
(defun set-local-abbrevs (abbrevs)
  "Add ABBREVS to `local-abbrev-table' and make it buffer local.
    ABBREVS should be a list of abbrevs as passed to `define-abbrev-table'.
    The `local-abbrev-table' will be replaced by a copy with the new abbrevs added,
    so that it is not the same as the abbrev table used in other buffers with the
    same `major-mode'."
  (let* ((bufname (buffer-name))
         (prefix (substring (md5 bufname) 0 (length bufname)))
         (tblsym (intern (concat prefix "-abbrev-table"))))
    (set tblsym (copy-abbrev-table local-abbrev-table))
    (dolist (abbrev abbrevs)
      (define-abbrev (eval tblsym)
        (cl-first abbrev)
        (cl-second abbrev)
        (cl-third abbrev)))
    (setq-local local-abbrev-table (eval tblsym))))

; the third arg is not used - only for the hook function which is a window
(defun lam/reload (lambuffer sourcebuffer arg)
  (with-current-buffer lambuffer
    (let* ((buf (buffer-substring-no-properties (point-min) (point-max)))
	   (at (read (concat "(" buf ")"))))
      (with-current-buffer sourcebuffer
	(set-local-abbrevs at)))))

(defun lam/open ()
  (interactive)
  (abbrev-mode)
  (let ((lambuffer (get-buffer-create "*lam*"))
        (sourcebuffer (current-buffer)))
    (display-buffer-in-side-window lambuffer nil)
    (with-current-buffer lambuffer
      ; to make the hook local to buffer
      (add-hook 'window-selection-change-functions (apply-partially #'lam/reload lambuffer sourcebuffer) 0 t)
      )))

(provide 'lam)
