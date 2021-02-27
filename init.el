;; ----- ;;
;; SETUP ;;
;; ----- ;;

;; load package.el
(require 'package)

;; initialize package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
						 ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; Fetch the list of packages available only if the list is empty
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not already installed on newer version of emacs
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

;; diminish allows you to remove minor modes from the mode line with :diminish in use-package
(use-package diminish)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons doom-themes rustic rainbow-delimiters helm-lsp lsp-ui treemacs-evil lsp-java lsp-mode flycheck yasnippet-snippets yasnippet company visual-fill-column diminish quickrun which-key helm-descbinds helm projectile use-package auctex evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; --------- ;;
;; EVIL MODE ;;
;; --------- ;;

;; install and initialize evil mode
(use-package evil
  :config
  (evil-mode 1))

;; -------------- ;;
;; BASIC SETTINGS ;;
;; -------------- ;;

;; disable startup screen
(setq inhibit-startup-message t)

;; Disable annoying ring-bell when backspace key is pressed in certain situations
(setq ring-bell-function 'ignore)

;; disable menu bar
(menu-bar-mode -1)

;; disable toolbar
(tool-bar-mode -1)

;; disable scrollbar
(scroll-bar-mode -1)

;; when something changes a file, auto refresh the buffer containing that file
(global-auto-revert-mode t)

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Set language environment to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; enable spell check for text mode and in comments for prog mode
(setq ispell-program-name "/usr/bin/aspell")
(add-hook 'text-mode-hook 'flyspell-mode)
;; (diminish 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; (diminish 'flyspell-prog-mode)

;; Maintain a list of recent files opened
(recentf-mode 1)
(setq recentf-max-saved-items 50)

;; enable line numbers. Non-buggy mode was added in Emacs 26
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode t))
(column-number-mode t)

;; disable line numbers for some mode
(dolist (mode '(term-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; move backup files to custom directory to eliminate clutter
;; "." is a regex that matches everything
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp-files/backup-files/" user-emacs-directory))))

;; auto-save-mode doesn't create the path automatically
(make-directory (expand-file-name "tmp-files/auto-save-files/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp-files/auto-save-files/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp-files/auto-save-files/" user-emacs-directory) t)))

;; ----- ;;
;; THEME ;;
;; ----- ;;

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)

  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)

  (doom-themes-org-config))

;; NOTE: the fist time you load your configuration on a new machine, you'll need to run
;; the following command interactively so that the mode line icons display correctly
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; --------------- ;;
;; CODING SETTINGS ;;
;; --------------- ;;

;; Automatically add ending brackets and braces
(electric-pair-mode 1)

;; comment without having to select line
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region-or-line)

;; brackets become different colors to see which ones match more easily
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Make sure tab-width is 4 and not 8
(setq-default tab-width 4)

;; use spaces to indent
(setq-default indent-tabs-mode nil)

;; Highlight matching brackets and braces
(show-paren-mode 1)

;; projectile
(use-package projectile
  ;; :diminish projectile-mode
  :init (projectile-mode +1)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; helm
(use-package helm
  ;; :diminish helm-mode
  :config
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

;; helm descbinds
;; brings up a window to discover keyboard shortcuts for modes that are currently active
(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds))

;; which key visually guids you through available options for some prefix commands
(use-package which-key
  ;; :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; quickrun allows you to execute most programs (if there is language support) in the current buffer
(use-package quickrun
  :bind
  ("C-c r" . quickrun)
  :config
  (setq quickrun-focus-p nil ;; disable focusing output buffer after running
        quickrun-timeout-seconds 180))
(setq
   display-buffer-alist
   `(("^\\(\\*quickrun\\*\\|\\*rustfmt\\*\\|\\*rustic-compilation\\*\\)$"
      (display-buffer-reuse-window display-buffer-in-previous-window display-buffer-in-side-window)
      (side . bottom)
      (window-width . 0.2)
      (reusable-frames . visible))))

;; company ("complete anything") is a text completion framework for emacs
(use-package company)

;; Yasnippet is a template system for emacs. yasnippet-snippets are the official collection of snippets
(use-package yasnippet
  ;; :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode))
(use-package yasnippet-snippets
  :after yasnippet)

;; FlyCheck checks for errors in code for a bunch of programming languages
(use-package flycheck
  ;; :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode))

;; LSP
(use-package lsp-mode
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (java-mode . lsp))
  :commands lsp
  :config
  ;; (setq lsp-completion-enable-additional-text-edit nil)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :init
  (setq lsp-keymap-prefix "C-c l" ;; this is for which-key integration documentation
        lsp-completion-provider :capf))

;; LSP Java
(use-package lsp-java
  :after lsp-mode)

;; Rust
(use-package rustic
  :after lsp-mode
  :init
  (setq rustic-lsp-server 'rls))
(add-hook 'before-save-hook (lambda () (when (eq 'rustic-mode major-mode)
                                           (rustic-format-buffer))))

;; DAP is the debug adapter protocol for Emacs
;; (use-package dap-mode
;;   :after lsp-mode
;;   ;; :config
;; ;;  (require 'dap-java)
;;   (dap-auto-configure-mode)
;;   :hook (dap-mode . dap-ui-mode))

;; (use-package dap-java
;;   :ensure nil
;;   :after dap-mode)

;; treemacs is a layout file explorer for emacs
(use-package treemacs
  :commands treemacs)
(use-package treemacs-evil
  :after treemacs evil)
;; (use-package lsp-treemacs
;;   :after lsp-mode treemacs
;;   :commands lsp-treemacs-errors-list)

;; LSP UI is used by various packages that require UI elements in LSP
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-position 'bottom))

;; Helm LSP
(use-package helm-lsp
  :after lsp-mode
  :commands helm-lsp-workspace-symbol)
