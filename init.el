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
(package-initialize)
(eval-when-compile
  (require 'use-package))

(setq indent-tabs-mode nil
      tab-width 4)

(use-package zerodark-theme
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))
  :init
  (global-flycheck-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package irony
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package clang-format
  :ensure t
  :init
  (setq clang-format-style "llvm")
  :config
  (add-hook 'c++-mode-hook (lambda ()
			     (add-hook 'before-save-hook 'clang-format-buffer nil t))))
(use-package flycheck-clang-tidy
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))
  

(use-package meson-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

;(use-package helm
;  :ensure t
;  :bind (("M-x" . helm-M-x)
;	 ("C-x C-f" . helm-find-files))
;  :init
;  (require 'helm-config)
;  (helm-mode 1))
;
;(use-package helm-descbinds
;  :ensure t
;  :bind (("C-h b" . helm-descbinds)
;	 ("C-h w" . helm-descbinds)))

(use-package markdown-mode
  :ensure t)
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map "  " 'save-buffer)
  (define-key evil-normal-state-map " eb" 'eval-buffer)
  (define-key evil-normal-state-map " jb" 'switch-to-buffer))

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t)
  (setq ivy-use-virtual-buffers t)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (define-key evil-normal-state-map " jr" 'counsel-rg)
  (define-key evil-normal-state-map " jf" 'counsel-find-file)
  (define-key evil-normal-state-map " ir" 'counsel-rhythmbox))


(use-package projectile
  :ensure t
  :init
  (evil-define-key 'normal projectile-mode-map " po" 'projectile-find-other-file))

(use-package counsel-projectile
  :ensure t
  :init
  (evil-define-key 'normal projectile-mode-map " pf" 'counsel-projectile-find-file)
  (evil-define-key 'normal projectile-mode-map " pr" 'counsel-projectile-rg))


(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t)

(add-hook 'org-mode-hook (lambda ()
			   (setq fill-column 78)
			   (auto-fill-mode t)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)

(org-babel-do-load-languages
 'org-babel-load-languages
  '((dot . t)))
(setq org-confirm-babel-evaluate nil)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(c-add-style "llvm.org"
	     '("gnu"
	       (fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)))))
(add-hook 'c-mode-common-hook (lambda () (c-set-style "llvm.org")))

(load-theme 'zerodark t)
(zerodark-setup-modeline-format)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-clang-tidy counsel-projectile projectile rainbow-delimiters ox-reveal markdown-mode evil-magit helm-descbinds helm meson-mode clang-format company evil-collection zerodark-theme evil use-package)))
 '(safe-local-variable-values
   (quote
    ((eval set
	   (make-local-variable
	    (quote my-project-path))
	   (file-name-directory
	    (let
		((d
		  (dir-locals-find-file ".")))
	      (if
		  (stringp d)
		  d
		(car d)))))
     (eval setq flycheck-clang-include-path
	   (list
	    (concat my-project-path "include")))
     (eval set
	   (make-local-variable
	    (file-name-directory
	     (let
		 ((d
		   (dir-locals-find-file ".")))
	       (if
		   (stringp d)
		   d
		 (car d))))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282c34" :foreground "#abb2bf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "fsdf" :family "Iosevka")))))
