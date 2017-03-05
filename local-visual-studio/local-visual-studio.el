;;; local-visual-studio.el --- load Visual Studio environment

;;; Commentary:
;; This expands heavily on the the .bat files from the Visual Studio
;; SDK.  The primary goal is to provide enough information so that
;; clang mode and company-clang work properly.

;;; Configuration:
(defvar load-windows-nt:visual-studio-version "14.0"
  "Determine what version of Visual Studio to extract from the registry.

Defaults to '14.0' which is equivalent to Visual Studio 2015.")

;;; Code:

(provide 'local-visual-studio)
;;; local-visual-studio.el ends here
