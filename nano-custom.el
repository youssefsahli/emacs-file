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

(use-package racket-mode
  :straight t
  :config
  (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable))

(use-package scribble-mode
  :straight t)

(use-package good-scroll
  :straight t
  :config
  (good-scroll-mode 1))

(use-package vterm
  :straight t)

(use-package scrollkeeper
  :straight t
  :config
  (global-set-key (kbd "C-v") #'scrollkeeper-down)
  (global-set-key (kbd "M-v") #'scrollkeeper-up))

(use-package rainbow-mode
  :straight t)

(visual-line-mode t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(display-line-numbers-mode t)
(tool-bar-mode -1)
(setq show-paren-style 'expression)
(show-paren-mode t)

(recentf-mode 1)
(setq recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

