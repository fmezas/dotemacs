;; https://github.com/abedra/emacs.d

;; System prereqs:
;; nvm
;; tern - (`npm install tern -g` in the node env to be used by Emacs)

(setq user-full-name "Francisco Meza")
(setq user-mail-address "fmezas@gmail.com")

(require 'cl)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar fmezas/packages '(auto-complete
                          autopair
                          base16-theme
                          clojure-mode
                          deft
                          editorconfig
                          elisp-slime-nav
                          exec-path-from-shell
                          flycheck
                          gist
                          graphviz-dot-mode
                          js2-mode
                          magit
                          markdown-mode
                          nvm
                          paredit
                          persistent-scratch
                          restclient
                          tern
                          tern-auto-complete
                          web-mode
                          yaml-mode)
  "Default packages")

(persistent-scratch-setup-default)

(defun fmezas/packages-installed-p ()
  (loop for pkg in fmezas/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (fmezas/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg fmezas/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; initial window
(setq initial-frame-alist
      '(
	(left . 50)
	(top . 50)
        (width . 160) ; character
        (height . 54) ; lines
        ))

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq tab-width 2
      indent-tabs-mode nil)

(setq make-backup-files nil)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)

(setq use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "orange" :weight bold))))
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))

(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-show-log t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines t)
(setq org-agenda-files (list "~/Dropbox/org/personal.org"
                             "~/Dropbox/org/invitae.org"))

(require 'org)
(require 'org-install)
(require 'org-habit)
(add-to-list 'org-modules "org-habit")
(setq org-habit-preceding-days 7
      org-habit-following-days 1
      org-habit-graph-column 80
      org-habit-show-habits-only-for-today t
      org-habit-show-all-today t)

(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   (ruby . t)
   (js . t)
   (C . t)))

(add-to-list 'org-src-lang-modes (quote ("dot". graphviz-dot)))
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

(defvar org-babel-default-header-args:clojure
  '((:results . "silent") (:tangle . "yes")))

(defun org-babel-execute:clojure (body params)
  (lisp-eval-string body)
  "Done!")

;; (provide 'ob-clojure)
(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (python . t)))

(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook (lambda ()
                                          (condition-case nil
                                              (org-display-inline-images)
                                            (error nil)))
          'append)

(setq org-ditaa-jar-path "~/.emacs.d/vendor/ditaa0_9.jar")
(setq org-plantuml-jar-path "~/.emacs.d/vendor/plantuml.jar")

(setq deft-directory "~/Dropbox/deft")
;; (setq deft-use-filename-as-title t)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)

(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

(setq column-number-mode t)

(editorconfig-mode 1)
(global-hl-line-mode 1)
(global-flycheck-mode 1)
(set-face-foreground 'highlight nil)
(set-face-background 'hl-line "#e5e5e5")

(require 'autopair)
(require 'auto-complete-config)
(ac-config-default)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(setq-default show-trailing-whitespace t)

(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
(setq markdown-css-paths `(,(expand-file-name "markdown.css" "~/.emacs.d/vendor")))

(require 'nvm)
(nvm-use "v10.15.3")

(when (eq system-type 'darwin) ;; mac specific settings
  (exec-path-from-shell-initialize))

(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
(add-hook 'js2-mode-hook (lambda () (setq ac-sources (append '(ac-source-tern-completion) ac-sources))))

(setq ac-sources (append '(ac-source-tern-completion) ac-sources))

(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
(add-hook hook 'elisp-slime-nav-mode))

(if window-system
    (load-theme 'base16-railscasts t)
  (load-theme 'wombat t))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; key bindings for OSX
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'control)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)) ;; sets fn-delete to be right-delete

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
