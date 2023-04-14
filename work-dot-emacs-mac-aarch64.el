(package-initialize)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; After trying custom variables they were set after theme was
;; activated.
(setq tron-legacy-theme-softer-bg t)
(setq tron-legacy-theme-vivid-cursor t)

;; GNU coreutils on Mac
(setq insert-directory-program "gls")

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tron-legacy))
 '(custom-safe-themes
   '("170af23ddc34cdb52c40c5bd27dcf26642a5174a023e3912aad41f5c066ceb16" "1244f129cbad816d45e1c80cea6a77e2cc94606e122edb44a34bc3d45f52af5d" "e80b1078c4ce2225f6f8d7621a55d3b675c86cad505b22b20243d4d075f491f5" default))
 '(lsp-pylsp-plugins-flake8-enabled nil)
 '(org-indent-mode-turns-on-hiding-stars nil)
 '(org-startup-indented t)
 '(package-selected-packages
   '(flycheck-pyflakes company-rtags flycheck-rtags rtags rtags-xref yaml-mode magit projectile go-dlv git-gutter rainbow-delimiters ace-jump-mode ace-window company flycheck go-mode lsp-ui lsp-mode direnv tron-legacy-theme))
 '(safe-local-variable-values
   '((lsp-pylsp-plugins-pyflakes-enabled . t)
     (lsp-pylsp-plugins-flake8-enabled . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'org-tempo)
(require 'lsp-mode)

;; go settings
(defvar gofmt-command (concat (getenv "HOME") "/go/bin/goimports"))

(add-hook 'go-mode-hook
	  (lambda ()
	    (rainbow-delimiters-mode)
	    (direnv-mode)
	    (set-fill-column 120)
	    (git-gutter-mode)
	    (lsp-mode)
	    (local-set-key (kbd "M-.") #'lsp-find-definition)
	    (local-set-key (kbd "C-M-.") #'lsp-find-implementation)
	    (add-hook 'before-save-hook 'gofmt-before-save)))

(use-package lsp-ui
	     :ensure t
	     :commands (lsp-ui-mode))

(setq git-gutter:update-interval 2)
	    
(defvar org-daily-todo:dir (concat (getenv "HOME") "/org/todo/daily/")
(defun org-daily-todo ()
  "Open a 'org-mode' TODO with today's date.
Uses org-daily-todo:dir to set the path."
  (interactive)
  (let* ((orgfile-name (concat (format-time-string "%Y-%m-%d") ".org"))
	 (daily-file (concat (expand-file-name org-daily-todo:dir) orgfile-name)))
    (find-file daily-file)))

(global-set-key (kbd "<f5>") 'org-daily-todo)
(global-set-key (kbd "<f6>") 'magit)
(global-set-key (kbd "<f9>") 'ace-jump-mode)

(global-set-key (kbd "C-x w") 'ace-window)
(global-set-key (kbd "<f2>") 'open-flycheck-or-close)
(global-set-key (kbd "C-<f2>") 'flycheck-next-error)
(global-set-key (kbd "<f3>") 'grep-find)

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

;; from
;; https://gist.github.com/Ladicle/119c57fc97439c1b103f7847aa03be52,
;; take advantage of bigger laptop for lsp-mode perf
(setq read-process-output-max 1048576)
(setq gc-cons-threshold 100000000)

(add-hook 'python-mode-hook
	  (lambda ()
	    (rainbow-delimiters-mode)
	    (git-gutter-mode)
	    (lsp-mode)
    	    (local-set-key (kbd "M-.") #'lsp-find-definition)
	    (local-set-key (kbd "C-M-.") #'lsp-find-implementation)))

(provide '.emacs)
;;; .emacs ends here

