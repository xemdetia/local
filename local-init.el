;;; local-init.el --- Initialization Code for Local

;;; Commentary:
;; This is the initialization library, which adds packages to the path
;; on load.
;; To use, add to load-path and then (require 'local-init).

;;; Code:
(let ((default-directory  "~/.emacs.d/local/"))
  (normal-top-level-add-to-load-path '("local-emacs-startup" "local-keys" "local-windows-nt" "local-visual-studio"))
  (defvar flycheck-emacs-lisp-load-path 'inherit))

;;; Require sub-packages:
(require 'local-emacs-startup)
(require 'local-keys)
(when (eq system-type 'windows-nt)
  (require 'local-windows-nt)
  (require 'local-visual-studio))

(defun local-init-install ()
  "Install core packages."
  (interactive)
  (require 'package)
  (package-refresh-contents)
  (package-install 'ace-jump-mode)
  (package-install 'magit)
  (package-install 'git-gutter)
  (package-install 'rainbow-delimiters)
  (package-install 'flycheck))

(if (boundp 'global-git-gutter-mode)
    (global-git-gutter-mode +1)
  (message "local-init: global-git-gutter-mode does not exist, did you run local-init-install?"))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (flycheck-mode)
	    (rainbow-delimiters-mode)))

(provide 'local-init)
;;; local-init.el ends here
