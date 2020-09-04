;;emacs load path

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; to load use-package
;; use-package is used to declaratively specified a
;; package to be used, so that it is installed
;; automatically at startup if it does not exist
;;
;; https://emacs.stackexchange.com/questions/28932/how-to-automatically-install-all-packages-loaded-by-my-emacs-file-in-the-minimu

(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

;; packages needed
;;(use-package gruvbox-theme
;;  :ensure t)

(use-package pdf-tools
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package ess
  :ensure t)

(use-package ess-view
  :ensure t)

(use-package elfeed
  :ensure t)

(use-package elfeed-goodies
  :ensure t)

(use-package ggtags
  :ensure t)

;;commented out due to build errors on fedora
;;(use-package elfeed-org
;;  :ensure t)

(setq load-path (cons "~/emacs.d/elisp" load-path))

;;user customizations

;;modules
;(require 'quack)

;;theme
(load-theme 'manoj-dark)
;;(load-theme 'gruvbox t)
;;(load-theme 'zenburn)
;;(load-theme 'solarized-dark)
;;(load-theme 'solarized-light)

(if (eq system-type 'windows-nt)
    (set-frame-font "Consolas-10"))

(if (eq system-type 'gnu/linux)
    (set-frame-font "Inconsolata-12"))

(if (eq system-type 'darwin)
    (set-frame-font "Menlo-14"))

;; Prevent the cursor from blinking
(blink-cursor-mode 0)
;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; Don't let Emacs hurt your ears
(setq visible-bell t)

;; no scrollbar
;; (scroll-bar-mode 0)

;; no toolbar/no menubar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Set font size to 100
(set-face-attribute 'default (selected-frame) :height 100)

;; c programming 
(setq c-default-style "linux"
      c-basic-offset 4)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "6ac7c0f959f0d7853915012e78ff70150bfbe2a69a1b703c3ac4184f9ae3ae02" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(package-selected-packages
   (quote
    (ggtags elfeed-org elfeed-goodies elfeed use-package pdf-tools magit gruvbox-theme ess-view csv-mode)))
 '(quack-default-program "racket")
 '(quack-programs
   '("racket" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme -M errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; User-defined functions

;; Functions from the Learning GNU Emacs book
(defun count-words-buffer ()
  "Count the number of words in the current buffer;
print a message in the minibuffer with the result."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(forward-word 1)
	(setq count (1+ count)))
      (message "buffer contains %d words." count))))


;; Org mode
;; see org mode guide section 1.3 - Activation

;; The following lines are always needed.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; see for default org mode file

(if (eq system-type 'gnu/linux)
    (setq am-home-directory "/home/abhishek"))

(if (eq system-type 'darwin)
    (setq am-home-directory  "/Users/abhishekmishra"))

(setq am-org-directory (concat am-home-directory "/Documents/notes"))

(setq org-default-notes-file (concat am-org-directory "/journal-2019.org"))

;; macos keybindings
;; see https://www.emacswiki.org/emacs/EmacsForMacOS#toc26
;; and http://lists.gnu.org/archive/html/help-gnu-emacs/2011-02/msg00019.html
(setq mac-right-option-modifier 'control)
(setq mac-right-command-modifier 'meta)

(pdf-tools-install)
