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

;; Advice approach is not working
; not to mention - it seems to fire a lot!!

;; (advice-add 'select-window :before 'lam-update)
;; (advice-remove 'select-window 'lam-update)

;; (defun lam-update (window &optional norecord)
;;   "Called when window focus is changed. Advice added to select-window."
;;   (message "Entering lam-update: " (buffer-name (window-buffer window)))
;;   (if (eq (buffer-name) "*lam*")
;;       (message "Will update Local Abbrev Manager.")))

;; Similarly hooking onto 'after-change-functions is also very wasteful

;; (defun lam/reload (beg end pre)
;;   (message "Hi"))

;; But - this is the best option so far!
(defun lam/reload1 (arg)
  (message "Win Change"))

(defun lam/open ()
  (interactive)
  (abbrev-mode)
  (let ((lambuffer (get-buffer-create "*lam*")))
    (display-buffer-in-side-window lambuffer nil)
    (with-current-buffer lambuffer
        ; to make the hook local to buffer
      (add-hook 'window-selection-change-functions 'lam/reload1 0 t)
      )))

(provide 'lam)
