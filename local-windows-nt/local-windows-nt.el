;;; local-windows-nt.el --- Windows specific settings

;;; Commentary:
;; Emacs works generally quite well on Windows but there are some
;; quality of life tunings to make it work with other packages that
;; are unique to Windows.  By loading this file you are checking that
;; you are on a Windows NT system first.

;;; Code:
(defun load-windows-nt--install-shell-file ()
  "Copy a shell file to emacs.d for CMD.exe.
In normal operation the HOME environment variable points into
AppData and not the Windows-centric home directory.  To resolve
this you need to install a shell file handler to cause this
action to occur on start."
    (if (and (not (file-exists-p "~/.emacs.d/init_cmdproxy.exe.sh"))
	     (file-exists-p "~/.emacs.d/local/local-windows-nt/init_cmdproxy.exe.sh"))
	(copy-file "~/.emacs.d/local/local-windows-nt/init_cmdproxy.exe.sh" "~/.emacs.d/")))

(provide 'local-windows-nt)
;;; local-windows-nt.el ends here
