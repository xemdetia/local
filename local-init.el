;;; local-init.el --- Initialization Code for Local

;;; Commentary:
;; This is the initialization library, which adds packages to the path
;; on load.
;; To use, add to load-path and then (require 'local-init).

;;; Code:
(let ((default-directory  "~/.emacs.d/local/"))
  (normal-top-level-add-to-load-path '("local-emacs-startup" "local-keys" "local-windows-nt"))
  (defvar flycheck-emacs-lisp-load-path 'inherit))

;;; Require sub-packages:
(require 'local-emacs-startup)
(require 'local-keys)
(if (eq system-type 'windows-nt)
    (require 'local-windows-nt))

(provide 'local-init)
;;; local-init.el ends here
