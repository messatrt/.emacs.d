;; Backup directory
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))

;; Custom settings file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; UI tweaks
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Relative line numbers globally
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Font
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 100)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Package management
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/") ;; okay to keep org repo, harmless
        ("gnu-devel" . "https://elpa.gnu.org/devel/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Rust
(use-package rust-mode
  :config
  (setq rust-format-on-save t))

;; Zig
(use-package zig-mode
  :mode ("\\.zig\\'" . zig-mode))

;; VTerm
(use-package vterm)

;; Shell path on macOS
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Disable line numbers in some modes
(dolist (mode '(vterm-mode-hook
                dired-mode-hook
                eshell-mode-hook
                term-mode-hook
		org-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Escape key to quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Theme setup
;; (use-package autothemer)

;; (mapc #'disable-theme custom-enabled-themes)

;; (use-package rose-pine-theme
;;   :load-path "~/.emacs.d/lisp"
;;   :config
;;   (load-theme 'rose-pine t))

;; ;;Company mode for auto-completion
;; (use-package company
;;   :ensure t
;;   :hook (after-init . global-company-mode)
;;   :config
;;   (setq company-idle-delay 0.2
;;         company-minimum-prefix-length 1))

;; ;; Eglot for LSP support
;; (use-package eglot
;;   :ensure t
;;   :hook ((prog-mode . eglot-ensure))
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '(zig-mode . ("zls")))) ;; example with Rust

;; (use-package yasnippet
;;   :hook (prog-mode . yas-minor-mode)
;;   :config
;;   (yas-reload-all))

;; (use-package yasnippet-snippets
;;   :after yasnippet)

;; ;; Optional: use company with yasnippet
;; (defun company-backend-with-yas (backend)
;;   "Add :with company-yasnippet to a BACKEND."
;;   (if (and (listp backend) (member 'company-yasnippet backend))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))


(load-theme 'doom-tokyo-night)
