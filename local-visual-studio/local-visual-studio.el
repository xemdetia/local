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

(defvar local-visual-studio:windows-sdk-lib-version "winv6.3\\"
  "Maps to environment variable WindowsSDKLibVersion.
Defaults to 'winv6.3\' which is default in Visual Studio 2015.")

(defvar local-visual-studio:windows-sdk-exec-path-x86 ""
  "Maps to environment variable WindowsSDK_ExecutablePath_x86.")

(defvar local-visual-studio:windows-sdk-exec-path-x64 ""
  "Maps to environment variable WindowsSDK_ExecutablePath_x64.")

(defvar-local local-visual-studio--prefix-list
  '("HKLM\\SOFTWARE\\Wow6432Node"
    "HKCU\\SOFTWARE\\Wow6432Node"
    "HKLM\\SOFTWARE"
    "HKCU\\SOFTWARE")
  "This is a common list copied with `copy-sequence' and is extracted from the visual studio batch files. It seems necessary to handle registry complexity on various versions of windows, and is provided in the order in `local-visual-studio.el'. It is not recommended to modify this list.")

;;; Code:
(defun local-visual-studio--init-windows-sdk-dir ()
  "Initialize `local-visual-studio:windows-sdk-dir'.  This is based on `local-visual-studio:windows-sdk-version'.  Use the prefix-list strategy."
  (let ((reg-path (concat "\\Microsoft\\Microsoft SDKs\\Windows\\v" ; registry key has 'v'
			  local-visual-studio:windows-sdk-version))
	(prefix-list (copy-sequence local-visual-studio--prefix-list))
	(seeking t))
    (while (and (> (length prefix-list) 0)
		seeking)
      (when (setq local-visual-studio:windows-sdk-dir
		  (load-windows-nt--get-registry-value
		   (concat (pop prefix-list) reg-path)
		   "InstallationFolder"))
	(setq seeking nil)
      )
    (not seeking)))) ; not looking? found one

(provide 'local-visual-studio)
;;; local-visual-studio.el ends here
