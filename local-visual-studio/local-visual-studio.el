;;; local-visual-studio.el --- load Visual Studio environment

;;; Commentary:
;; This expands heavily on the the .bat files from the Visual Studio
;; SDK.  The primary goal is to provide enough information so that
;; clang mode and company-clang work properly.

(require 'local-windows-nt)

;;; Configuration:
;; vcvarsqueryregistry.bat
(defvar local-visual-studio:framework-40-version "v4.0"
  "Defaults to 'v4.0'.
Maps to environment variable Framework40Version")

(defvar local-visual-studio:visual-studio-version "14.0"
  "Determine what version of Visual Studio to extract from the registry.

Defaults to '14.0' which is equivalent to Visual Studio
2015. Maps to enviroment variable VisualStudioVersion after
init.")

(defvar local-visual-studio:windows-sdk-dir ""
  "Maps to environment variable WindowsSdkDir.")

(defvar local-visual-studio:windows-lib-path ""
  "Maps to environment variable WindowsLibPath.")

(defvar local-visual-studio:windows-sdk-version "10.0"
  "Maps to environment variable WindowsSDKVersion.
By default it is '10.0' to match Windows 10")

(defvar local-visual-studio:windows-sdk-version-detailed "10.0.10586.0"
  "Set the detailed requested SDK version.
Should match `local-visual-studio:windows-sdk-version''s major version.  Currently matches a Windows 10 version")

(defvar local-visual-studio:windows-sdk-lib-version "winv6.3\\"
  "Maps to environment variable WindowsSDKLibVersion.
Defaults to 'winv6.3\' which is default in Visual Studio 2015.")

(defvar local-visual-studio:windows-sdk-exec-path-x86 ""
  "Maps to environment variable WindowsSDK_ExecutablePath_x86.")

(defvar local-visual-studio:windows-sdk-exec-path-x64 ""
  "Maps to environment variable WindowsSDK_ExecutablePath_x64.")

(defvar local-visual-studio:vc-install-dir ""
  "Maps to environment variable VCINSTALLDIR.")

(defvar-local local-visual-studio--prefix-list
  '("HKLM\\SOFTWARE\\Wow6432Node"
    "HKCU\\SOFTWARE\\Wow6432Node"
    "HKLM\\SOFTWARE"
    "HKCU\\SOFTWARE")
  "This is a common list copied with `copy-sequence' and is extracted from the visual studio batch files. It seems necessary to handle registry complexity on various versions of windows, and is provided in the order in `local-visual-studio.el'. It is not recommended to modify this list.")

(defvar-local local-visual-studio--prefix-list-2
  '("HKLM\\SOFTWARE"
    "HKCU\\SOFTWARE"
    "HKLM\\SOFTWARE\\Wow6432Node"
    "HKCU\\SOFTWARE\\Wow6432Node")
  "This is a common list copied with `copy-sequence' and is
  extracted from the visual studio batch files. It seems
  necessary to handle registry complexity on various versions of
  windows, and is provided in the order in
  `local-visual-studio.el'. It is a slightly different order from
  `local-visual-studio--prefix-list' and seems isolated to the
  visual studio install dir. It is not recommended to modify this
  list.")

;;; Code:
(defun local-visual-studio--setq-registry-prefix-list (var path key)
  "Directly set VAR from registry PATH and KEY.

This uses the local prefix-list to find the first match of a PATH KEY using the visual studio specific prefix list for paths.  This is duplicated constantly through the batch files as helpers for various subroutines, and would have to be done here as well if not factored out."
  (let ((prefix-list (copy-sequence local-visual-studio--prefix-list))
	(seeking t))
    (while (and (> (length prefix-list) 0)
		seeking)
      (when (set (intern-soft (symbol-name var))
		 (load-windows-nt--get-registry-value
		  (concat (pop prefix-list) path)
		  key))
	(setq seeking nil)))
    (not seeking))) ; not looking? found one

(defun local-visual-studio--setq-registry-prefix-list-2 (var path key)
  "Directly set VAR from registry PATH and KEY.

This uses the local prefix-list to find the first match of a PATH
KEY using the visual studio specific prefix list for paths.  This
is duplicated constantly through the batch files as helpers for
various subroutines, and would have to be done here as well if
not factored out.  Nearly identical to
`local-visual-studio--setq-registry-prefix-list' and likely
candidate for refactoring."
  (let ((prefix-list (copy-sequence local-visual-studio--prefix-list-2))
	(seeking t))
    (while (and (> (length prefix-list) 0)
		seeking)
      (when (set (intern-soft (symbol-name var))
		 (load-windows-nt--get-registry-value
		  (concat (pop prefix-list) path)
		  key))
	(setq seeking nil)))
    (not seeking))) ; not looking? found one

(defun local-visual-studio--init-windows-sdk-dir ()
  "Initialize `local-visual-studio:windows-sdk-dir'.  This is based on `local-visual-studio:windows-sdk-version'.  Use the prefix-list strategy."
  (let ((reg-path ; registry key has 'v'
	 (concat "\\Microsoft\\Microsoft SDKs\\Windows\\v"
		 local-visual-studio:windows-sdk-version)))
    (local-visual-studio--setq-registry-prefix-list
     'local-visual-studio:windows-sdk-dir
     reg-path
     "InstallationFolder")))

(defun local-visual-studio--init-vc-install-dir ()
  "Initialize `local-visual-studio:vc-install-dir'.
Uses the prefix-list strategy, but in reverse order to match the batch
files."
  (let ((reg-path "\\Microsoft\\VisualStudio\\SxS\\VC7"))
    (local-visual-studio--setq-registry-prefix-list-2
     'local-visual-studio:vc-install-dir
     reg-path
     "14.0")))

(defun local-visual-studio-install ()
  "Install visual studio environment into Emacs session."
  (setenv "WindowsSdkDir" local-visual-studio:windows-sdk-dir)
  ;; PATH
  (setenv "PATH" (concat (concat local-visual-studio:windows-sdk-dir "bin\\x86;") (getenv "PATH")))
  (add-to-list 'exec-path (replace-regexp-in-string "\\\\" "/" (concat local-visual-studio:windows-sdk-dir "bin\\x86")))
  ;; INCLUDE, separated like batch file indicates and in order of
  ;; actual appearance of evaluated batch file.
  (let ((include-vc-a (concat local-visual-studio:vc-install-dir "INCLUDE"))
	(include-vc-b (concat local-visual-studio:vc-install-dir "ATLMFC\\INCLUDE"))
	(include-sdk-a (concat local-visual-studio:windows-sdk-dir "include\\" local-visual-studio:windows-sdk-version-detailed "\\shared"))
	(include-sdk-b (concat local-visual-studio:windows-sdk-dir "include\\" local-visual-studio:windows-sdk-version-detailed "\\um"))
	(include-sdk-c (concat local-visual-studio:windows-sdk-dir "include\\" local-visual-studio:windows-sdk-version-detailed "\\winrt")))
    (setenv "INCLUDE" (concat
		       (mapconcat 'identity (list include-vc-a
						  include-vc-b
						  include-sdk-a
						  include-sdk-b
						  include-sdk-c) ";")
		       (getenv "INCLUDE"))))
  t)

;;; Initialization:
(local-visual-studio--init-windows-sdk-dir)
(local-visual-studio--init-vc-install-dir)
(local-visual-studio-install)

(provide 'local-visual-studio)
;;; local-visual-studio.el ends here
