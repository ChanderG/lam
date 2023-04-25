;; -*- lexical-binding:t -*-

;; base code obtained from the "Buffer local abbrevs" section from https://www.emacswiki.org/emacs/AbbrevMode
;; then modified to:
;; 1. use a basetable so that it resets local abbrev everytime
;; 2. Wrap the 3rd arg into a lambda function
(defun set-local-abbrevs (abbrevs basetable)
  "Add ABBREVS to `local-abbrev-table' and make it buffer local.
    ABBREVS should be a list of abbrevs as passed to `define-abbrev-table'.
    The `local-abbrev-table' will be replaced by a copy with the new abbrevs added,
    so that it is not the same as the abbrev table used in other buffers with the
    same `major-mode'."
  (let* ((bufname (buffer-name))
         (prefix (substring (md5 bufname) 0 (length bufname)))
         (tblsym (intern (concat prefix "-abbrev-table"))))
    (set tblsym (copy-abbrev-table basetable))
    (dolist (abbrev abbrevs)
      (define-abbrev (eval tblsym)
        (cl-first abbrev)
        (cl-second abbrev)
        `(lambda () ,(cl-third abbrev))))
    (setq-local local-abbrev-table (eval tblsym))))

; the fourth arg is not used - only for the hook function which is a window
(defun lam/reload (lambuffer sourcebuffer basetable arg)
  (with-current-buffer lambuffer
    (let* ((buf (buffer-substring-no-properties (point-min) (point-max)))
	   (at (read (concat "(" buf ")"))))
      (with-current-buffer sourcebuffer
	(set-local-abbrevs at basetable)))))

(defun lam/open ()
  (interactive)
  (abbrev-mode)
  (let ((lambuffer (get-buffer-create "*lam*"))
        (sourcebuffer (current-buffer))
	(basetable local-abbrev-table))
    (display-buffer-in-side-window lambuffer nil)
    (with-current-buffer lambuffer
      ; to make the hook local to buffer
      (add-hook 'window-selection-change-functions
		(apply-partially #'lam/reload lambuffer sourcebuffer basetable) 0 t)
      )))

;; Simple macro to make key bindings easier
(defmacro lam/kbd (arg)
  `(execute-kbd-macro (read-kbd-macro ,arg)))

(provide 'lam)
