;;; .emacs --- Foo

;;; Commentary:
;;; Bar

;;; Code:
(setq user-full-name "Francisco Meza")
(setq user-mail-address "francisco@evisort.com")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(use-package base16-theme :ensure t)
(use-package company
  :ensure t
  :hook (emacs-lisp-mode . company-mode))
(use-package counsel
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))
(use-package deft
  :ensure t
  :config
  (setq deft-directory "~/Dropbox/deft")
  ;; (setq deft-use-filename-as-title t)
  (setq deft-extension "org")
  (setq deft-text-mode 'org-mode))
(use-package editorconfig :ensure t)
(use-package flycheck :ensure t)
(use-package js2-mode :ensure t)
(use-package magit :ensure t)
(use-package markdown-mode :ensure t)
(use-package nvm
  :ensure t
  :config (nvm-use "v12.18.3"))
(use-package paredit
  :ensure t
  :hook (emacs-lisp-mode . paredit-mode))
(use-package persistent-scratch :ensure t)
(use-package repl-toggle
  :ensure t
  :config
  (setq rtog/fullscreen t)
  (setq rtog/mode-repl-alist '((typescript-mode . run-ts))))
(use-package tide
  :ensure t
  :after (company flycheck js2-mode nvm repl-toggle typescript-mode)
  :config
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  :hook ((typescript-mode . company-mode)
	 (typescript-mode . editorconfig-mode)
	 (typescript-mode . flycheck-mode)
	 (typescript-mode . repl-toggle-mode)
	 (typescript-mode . tide-setup)
	 (typescript-mode . tide-hl-identifier-mode)
	 (before-save . tide-format-before-save)))
(use-package ts-comint :ensure t)

;;; initial window
;; position and size
(setq initial-frame-alist
      '((left . 50)
	(top . 50)
        (width . 280)
        (height . 70)))
;; no splash, empty initial scratch
(setq inhibit-splash-screen t
      initial-scratch-message nil)
;; keyboard-only navigation
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)
(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper-isearch)

;;; other defaults
(setq-default show-trailing-whitespace t)
(setq column-number-mode t)
;; empty lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
;; spaces not tabs, short tabs
(setq tab-width 2
      indent-tabs-mode nil)
;; no ~ files
(setq make-backup-files nil)
;; no bell
(setq use-dialog-box nil
      visible-bell t)
;; visual cue parens)
(show-paren-mode t)
;; support colors in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
;; keyboard shortcuts
(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)

(if window-system
    (load-theme 'base16-railscasts t)
  (load-theme 'wombat t))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;;; .emacs ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(counsel use-package ts-comint tide repl-toggle persistent-scratch paredit nvm markdown-mode magit js2-mode editorconfig deft company base16-theme))
 '(persistent-scratch-autosave-interval 5)
 '(persistent-scratch-save-file "/home/francisco/Dropbox/.persistent-scratch")
 '(ts-comint-program-command
   "/home/francisco/code/tsplayground/node_modules/.bin/ts-node"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; persistent scratch
(persistent-scratch-autosave-mode 1)
(persistent-scratch-restore)
