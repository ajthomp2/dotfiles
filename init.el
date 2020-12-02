; ---------------------- ;
; SETUP
; ---------------------- ;

; load package.el
(require 'package)

; enable installing packages from MELPA and org
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")
))
(package-initialize)

; Fetch the list of packages available only if the list is empty
(unless package-archive-contents (package-refresh-contents))

; Install use-package
(setq package-list '(use-package))
(dolist (package package-list)
  (unless (package-installed-p package) (package-install package)))

; ---------------------- ;
; EVIL MODE
; ---------------------- ;

; install and initialize evil mode
(require 'evil)
(evil-mode 1)

; I think these were programatically added when I installed the evil plugin
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(quickrun which-key helm-descbinds helm projectile use-package auctex evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; ---------------------- ;
; BASIC SETTINGS
; ---------------------- ;

; Disable annoying ring-bell when backspace key is pressed in certain situations
(setq ring-bell-function 'ignore)

; disable menu bar
(menu-bar-mode -1)

; disable toolbar
(tool-bar-mode -1)

; disable scrollbar
(scroll-bar-mode -1)

; Set language environment to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

; enable spell check for text mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

; Enable soft-wrap
(global-visual-line-mode 1)

; Maintain a list of recent files opened
(recentf-mode 1)            
(setq recentf-max-saved-items 50)

; ---------------------- ;
; CODING SETTINGS
; ---------------------- ;

; Automatically add ending brackets and braces
(electric-pair-mode 1)

; Make sure tab-width is 4 and not 8
(setq-default tab-width 4)

; Highlight matching brackets and braces
(show-paren-mode 1) 

; projectile
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

; helm
(use-package helm
  :ensure t
  :init 
  (helm-mode 1)
  (progn (setq helm-buffers-fuzzy-matching t))
  :bind
  (("C-c h" . helm-command-prefix))
  (("M-x" . helm-M-x))
  (("C-x C-f" . helm-find-files))
  (("C-x b" . helm-buffers-list))
  (("C-c b" . helm-bookmarks))
  (("C-c f" . helm-recentf)) ; Add new key to recentf
  (("C-c g" . helm-grep-do-git-grep))) ; Search using grep in a git project

; helm descbinds
; brings up a window to discover keyboard shortcuts for modes that are currently active
(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds))

; which key visually guids you through available options for some prefix commands
(use-package which-key 
  :ensure t 
  :init
  (which-key-mode))

; quickrun allows you to execute most programs (if there is language support) in the current buffer
(use-package quickrun 
  :ensure t
  :bind ("C-c r" . quickrun))

; company ("complete anything") is a text completion framework for emacs
(use-package company
  :ensure t)
