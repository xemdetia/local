;;; local-keys.el --- local key bindings

;;; Commentary:
;; Loading all of the keys

;;; Code:

;; core emacs:
(global-set-key (kbd "<f3>") 'grep-find)

;; flycheck:
(global-set-key (kbd "<f2>") 'flycheck-list-errors)

;; magit:
(global-set-key (kbd "<f6>") 'magit-status)

;; ace-jump-mode:
(global-set-key (kbd "<f9>") 'ace-jump-mode)
(global-set-key (kbd "<C-f9>") 'ace-jump-word-mode)

(provide 'local-keys)
;;; local-keys ends here
