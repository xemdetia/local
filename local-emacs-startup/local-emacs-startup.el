;;; local-emacs-startup.el --- Top level startup stuff

;;; Commentary:
;; This is designed to resolve some of the more base settings.  There
;; should be no package specific stuff here.

;;; Code:

;;; UI Section:
;; Disable Tool Bar
(tool-bar-mode -1)
(setq inhibit-startup-message t)
(setq x-select-enable-clipboard t) ;; use clipboard on kill

;;; Encoding Section:
;; Always prefer UTF-8. This resolves DOS file endings for new files
;; when filesystems are shared. 'buffer-file-coding-system replaced
;; 'default-buffer-file-coding-system in Emacs 23.2.
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;;; Dired Section:
(defvar dired-dwim-target t) ;; guess target directory on copy

(provide 'local-emacs-startup)
;;; local-emacs-startup.el ends here
