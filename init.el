;; init.el --- My personal GNU Emacs configuration.

(let ((minver "26.3"))
      (when (version< emacs-version minver)
	(error "GNU Emacs v%s or higher is required." minver)))

;; Turn off mouse interface
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Don't use messages that you don't read
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; Setup the package management
(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; activate all the packages
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package-list '(use-package magit))

  (unless (package-installed-p package-list)
    (package-install package-list)))

(require 'use-package)

;; Install packages
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package highlight-defined
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))

(require 'magit)

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)
   ("C-x C-g" . magit-status)))

;; Auto commit and push init.el
(defun commit-and-push-init ()
  (interactive)
  (let* ((default-directory (expand-file-name ".emacs.d" "~"))
	 (init-file (expand-file-name "init.el" default-directory)))
    (when (magit-anything-modified-p t init-file)
      (magit-call-git "add" init-file)
      (magit-call-git "commit" "-S" "-m" (concat init-file ": " (format-time-string "%s")))
      (magit-call-git "push" "origin")
      (magit-refresh))))

(add-hook 'kill-emacs-hook #'commit-and-push-init)

;; Auto commit and push .gitignore
(defun commit-and-push-gitignore ()
  (interactive)
  (let* ((default-directory (expand-file-name ".emacs.d" "~"))
	 (gitignore-file (expand-file-name ".gitignore" default-directory)))
    (when (magit-anything-modified-p t gitignore-file)
      (magit-call-git "add" gitignore-file)
      (magit-call-git "commit" "-S" "-m" (concat gitignore-file ": " (format-time-string "%s")))
      (magit-call-git "push" "origin")
      (magit-refresh))))

(add-hook 'kill-emacs-hook #'commit-and-push-gitignore)

;; Auto commit and push README.md
(defun commit-and-push-readme ()
  (interactive)
  (let* ((default-directory (expand-file-name ".emacs.d" "~"))
	 (readme-file (expand-file-name "README.md" default-directory)))
    (when (magit-anything-modified-p t readme-file)
      (magit-call-git "add" readme-file)
      (magit-call-git "commit" "-S" "-m" (concat readme-file ": " (format-time-string "%s")))
      (magit-call-git "push" "origin")
      (magit-refresh))))

(add-hook 'kill-emacs-hook #'commit-and-push-readme)

(require 'org)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'org-config)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (org-bullets zenburn-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
