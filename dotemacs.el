(defun my-turn-modes (param &rest modes)
  ;;; Applies the parameter to the specified modes
  (mapcar #'(lambda (mode)
              (funcall mode param)) modes))

;; package management
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

;; autocomplete for emacs lisp mode
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; some defaults
(my-turn-modes 1
 'global-hl-line-mode
 ;; 'which-key-mode
 'whole-line-or-region-mode
 'ido-mode
 'paredit-mode
 'show-paren-mode
 'rainbow-delimiters-mode
 'column-number-mode
 'global-auto-complete-mode)

(setq-default
 which-key-idle-delay 0.2)

(my-turn-modes 0
 'scroll-bar-mode
 'tool-bar-mode
 'menu-bar-mode)

(setq-default
 inhibit-startup-screen t
 indent-tabs-mode nil)

(global-flycheck-mode)


(exec-path-from-shell-initialize)
(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(json-reformat:indent-width 2)
 '(markdown-command "/opt/local/bin/multimarkdown"))



;; key bindings for OSX
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'control)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char)) ;; sets fn-delete to be right-delete

(global-set-key (kbd "C-x O") 'previous-multiframe-window)

(load-theme 'zenburn t)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
