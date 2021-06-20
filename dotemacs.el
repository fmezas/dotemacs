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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq ;;use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package base16-theme)
(use-package company
  :hook ((emacs-lisp-mode . company-mode)
	 (scala-mode . company-mode))
  :config (setq lsp-completion-provider :capf))
(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))
(use-package deft
  :config
  (setq deft-directory "~/Dropbox/deft")
  ;; (setq deft-use-filename-as-title t)
  (setq deft-extension "org")
  (setq deft-text-mode 'org-mode))
(use-package editorconfig)
(use-package flycheck)
(use-package scala-mode
  :after (flycheck)
  :interpreter ("scala" . scala-mode)
  :hook (scala-mode . flycheck-mode))
(use-package sbt-mode
  :after (flycheck)
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  :hook (scala-mode . flycheck-mode))
(use-package js2-mode)
;; set prefix for lsp-command-keymap (few alternatives - "C-l", "s-l")
(setq lsp-keymap-prefix "C-c l")
(use-package lsp-mode
  :hook ((scala-mode . lsp)
	 (python-mode . lsp)
	 (lsp-mode . lsp-lens-mode))
  :config
  (setq lsp-pyls-plugins-pylint-enabled nil
        lsp-auto-configure t
        lsp-pyls-configuration-sources ["flake8"])
  (setq lsp-prefer-flymake nil)
  :commands lsp)
(use-package lsp-metals)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
(use-package magit)
(use-package markdown-mode)
(use-package nvm
;;  :config (nvm-use "v14.17.0")
  )
(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))
(use-package persistent-scratch
  :custom
  ;; (persistent-scratch-save-file
  ;;  "/home/francisco/Dropbox/.persistent-scratch-personal")
  (persistent-scratch-autosave-interval 5))
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package pyvenv)
(use-package repl-toggle
  :config
  (setq rtog/fullscreen t)
  (setq rtog/mode-repl-alist '((typescript-mode . run-ts))))
(use-package tide
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
(use-package ts-comint
  ;; :custom (ts-comint-program-command "/home/francisco/code/tsplayground/node_modules/.bin/ts-node")
  )

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
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; persistent scratch
(persistent-scratch-autosave-mode 1)
(persistent-scratch-restore)
