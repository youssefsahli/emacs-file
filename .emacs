(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; (straight-use-package
;;  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
;; (require 'nano)



;; (use-package omnisharp
;;   :straight t)

;; (use-package pollen-mode
;;   :straight t
;;   :config
;;   (global-set-key "\M-\\" "◊"))

(use-package all-the-icons :ensure t :straight t)

(use-package projectile :ensure t :straight t
  :config
  (projectile-mode t)
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package corral
  :straight t
  :config
  (global-set-key (kbd "M-9") 'corral-parentheses-backward)
  (global-set-key (kbd "M-0") 'corral-parentheses-forward)
  (global-set-key (kbd "M-[") 'corral-brackets-backward)
  (global-set-key (kbd "M-]") 'corral-brackets-forward)
  (global-set-key (kbd "M-{") 'corral-braces-backward)
  (global-set-key (kbd "M-}") 'corral-braces-forward)
  (global-set-key (kbd "M-\"") 'corral-double-quotes-backward))

(use-package dashboard
  :straight t
  :ensure t
  :config
  (set-face-attribute 'dashboard-heading-face nil
		      :family "Fira Mono for Powerline-11"
		      :weight 'normal
		      :foreground "#6c71c4")

  (set-face-attribute 'dashboard-items-face nil
		      :family "Fira Mono for Powerline-11"
		      :weight 'normal
		      :foreground "#93a1a1")
  
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-footer nil)
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
	`(;; line1
          ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Homepage"
            "Browse homepage"
            (lambda (&rest _) (browse-url "homepage"))))))
  (setq dashboard-set-heading-icons nil)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  (setq dashboard-startup-banner "~/.emacs.d/image.png")
  (dashboard-setup-startup-hook))

(use-package solarized-theme  :straight t
  :config
  (load-theme 'solarized-dark t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#002b36")))

(use-package moody :straight t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package centaur-tabs
  :straight t
  :config
  (centaur-tabs-headline-match)
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-bar 'under)
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-show-navigation-buttons t)
  (setq centaur-tabs-set-icons t)
  (centaur-tabs-change-fonts "hack" 109)
  (setq centaur-tabs--buffer-show-groups nil)
  (centaur-tabs-enable-buffer-reordering)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-plain-icons t)
  (centaur-tabs-mode 1))

(use-package pdf-tools :straight t)

;; (use-package powerline :straight t
;;   :config (powerline-default-theme))

;; (use-package doom-modeline :straight t
;;   :config
;;   (setq doom-modeline-height 12)
;;   (add-hook 'after-init-hook #'doom-modeline-mode))

(use-package racket-mode
  :straight t
  :config
  (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable))

(use-package lua-mode :straight t)

;; (use-package robe
;;   :straight t
;;   :config
;;   (add-hook 'ruby-mode-hook 'robe-mode))

;; (use-package love-minor-mode :straight t)

(use-package scribble-mode
  :straight t)

(use-package vterm
  :straight t)

(use-package rainbow-mode
  :straight t
  :config
  (add-hook 'after-init-hook #'rainbow-mode))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(global-visual-line-mode)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(display-line-numbers-mode t)
(tool-bar-mode -1)
(setq show-paren-style 'expression)
(show-paren-mode t)
(add-to-list 'default-frame-alist
	     '(font . "Fira Mono for Powerline-11"))

(recentf-mode 1)
(setq recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
  '("Common"))
