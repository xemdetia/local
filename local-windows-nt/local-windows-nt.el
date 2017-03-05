;;; local-windows-nt.el --- Windows specific settings

;;; Commentary:
;; Emacs works generally quite well on Windows but there are some
;; quality of life tunings to make it work with other packages that
;; are unique to Windows.  By loading this file you are checking that
;; you are on a Windows NT system first.

(defvar load-windows-nt:visual-studio-version "14.0"
  "Determine what version of Visual Studio to extract from the registry.

Defaults to '14.0' which is equivalent to Visual Studio 2015.")

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

(defun load-windows-nt--get-registry-value (path key)
  "Using reg.exe get the registry value for key.
The PATH includes the rootkey of HKLM HKCU and so on.  If no KEY
is found, nil is returned.  Because this is tied to startup a
failed key lookup writes to *Messages*"
  (let ((inhibit-message t)
	(exe (concat (getenv "windir") "\\system32\\reg.exe "))
	(buffer (get-buffer-create "*local-reg*"))
	(arg (concat "query "
		     "\"" path "\" "
		     "/v "
		     key)))
    ;; This is easiest to debug by using `with-current-buffer' and
    ;; then erasing it with `erase-buffer' before the insert.
    (with-current-buffer buffer
      (erase-buffer)
      (insert (shell-command-to-string
	       (concat exe arg)))
      (goto-char (point-min)) ; critical: leave point here for later regex
      (flush-lines "^$")
      (if (re-search-forward "^ERROR: The system was unable to find the specified registry key or value" nil t)
	  (progn
	    (message (concat "local-windows-nt: Could not find (" path " . " key ")"))
	    nil)
	(when (search-forward "REG_" nil nil 1) ; only take first occurance
	  (nth 2 (split-string (thing-at-point 'line) "    " t "\r?\n")))))))

;;; Initialization Section:
(load-windows-nt--install-shell-file)

(provide 'local-windows-nt)
;;; local-windows-nt.el ends here
