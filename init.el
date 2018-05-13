;;emacs load path

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
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
(use-package gruvbox-theme
  :ensure t)

(use-package pdf-tools
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package magit
  :ensure t)

(setq load-path (cons "~/emacs" load-path))
(setq load-path (cons "/usr/local/share/emacs/site-lisp" load-path))

;;user customizations

;;modules
;(require 'quack)

;;theme
;;(load-theme 'manoj-dark)
(load-theme 'gruvbox t)
;;(load-theme 'zenburn)
;;(load-theme 'solarized-dark)
;;(load-theme 'solarized-light)

(if (eq system-type 'windows-nt)
    (set-default-font "Consolas-10"))

(if (eq system-type 'gnu/linux)
    (set-default-font "Inconsolata-12"))

(if (eq system-type 'darwin)
    (set-default-font "Menlo-14"))

;; Prevent the cursor from blinking
(blink-cursor-mode 0)
;; Don't use messages that you don't read
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
;; Don't let Emacs hurt your ears
(setq visible-bell t)

;; no scrollbar
(scroll-bar-mode 0)

;; no toolbar/no menubar
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Set font size to 100
(set-face-attribute 'default (selected-frame) :height 140)

;; c programming 
(setq c-default-style "linux"
      c-basic-offset 4)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "6ac7c0f959f0d7853915012e78ff70150bfbe2a69a1b703c3ac4184f9ae3ae02" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (magit csv-mode gruvbox-theme zenburn-theme helm-system-packages solarized-theme org-edna)))
 '(quack-default-program "racket")
 '(quack-programs
   (quote
    ("racket" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -il r6rs" "mzscheme -il typed-scheme" "mzscheme -M errortrace" "mzscheme3m" "mzschemecgc" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))))
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

;; macos keybindings
;; see https://www.emacswiki.org/emacs/EmacsForMacOS#toc26
;; and http://lists.gnu.org/archive/html/help-gnu-emacs/2011-02/msg00019.html
(setq mac-right-option-modifier 'control)
(setq mac-right-command-modifier 'meta)

(pdf-tools-install)
