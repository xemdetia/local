(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(use-package rustic)
(tool-bar-mode -1)
(setq visible-bell 1)

;; After trying custom variables they were set after theme was
;; activated.
(setq tron-legacy-theme-softer-bg t)
(setq tron-legacy-theme-vivid-cursor t)
(defvar org-startup-indented t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tron-legacy))
 '(custom-safe-themes
   '("0d09f49e81c811f4a661ff63b82d2b812ce034aed336903922b06362b8cb0bbe" default))
 '(package-selected-packages
   '(typescript-mode tide flycheck-rust lsp-ui rainbow-delimiters flycheck tron-legacy-theme magit lsp-mode company cargo-mode rustic markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#17181b" :foreground "#B0CCDC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "outline" :family "Consolas")))))

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(set-language-environment "utf-8")
(require 'org-tempo)
(defvar rustic-format-trigger nil)
(add-hook 'rustic-mode-hook
	  (lambda ()
	    (rainbow-delimiters-mode)
	    (flycheck-mode)
	    (company-mode)
	    (git-gutter-mode)
	    (local-set-key (kbd "M-.") #'lsp-find-definition)
	    (local-set-key (kbd "C-M-.") #'lsp-find-implementation)))

(defvar company-idle-delay 0.1)
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (rainbow-delimiters-mode)
	    (flycheck-mode)
	    (company-mode)))

(use-package lsp-ui
	     :ensure t
	     :commands (lsp-ui-mode))


(global-set-key (kbd "<f2>") 'open-flycheck-or-close)
(global-set-key (kbd "<f6>") 'magit)

(defun open-flycheck-or-close ()
  "Open with flycheck-list-errors and if it is open, quit that window."
  (interactive)
  ;; featurep tests if provide for a package was called.
  (unless (featurep 'flycheck)
    (user-error "Flycheck feature not loaded"))
  ;; grab buffer name from flycheck var, then grab its window.  If the
  ;; window is nil then we know we can call flycheck-list-errors.
  (let* ((flycheck-buffer (get-buffer flycheck-error-list-buffer))
	 (flycheck-window (when flycheck-buffer
			    (get-buffer-window flycheck-buffer 'visible))))
    (if flycheck-window
	(quit-window nil flycheck-window)
      (flycheck-list-errors))))

;; typescript
(use-package tide :ensure t)
 (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(provide '.emacs)
;;; .emacs ends here
