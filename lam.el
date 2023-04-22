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


(defun lam/open ()
  (interactive)
  (abbrev-mode)
  (set-local-abbrevs '(("lam" "Local Abbrev Manager"))))

(provide 'lam)
