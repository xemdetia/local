;;; local-visual-studio.el --- load Visual Studio environment

;;; Commentary:
;; This expands heavily on the the .bat files from the Visual Studio
;; SDK.  The primary goal is to provide enough information so that
;; clang mode and company-clang work properly.

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

(defvar local-visual-studio:windows-sdk-version ""
  "Maps to environment variable WindowsSDKVersion.")

(defvar local-visual-studio:windows-sdk-lib-version "winv6.3\\"
  "Maps to environment variable WindowsSDKLibVersion.
Defaults to 'winv6.3\' which is default in Visual Studio 2015.")

;;; Code:

(provide 'local-visual-studio)
;;; local-visual-studio.el ends here
