;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Tom Klaver
;; Homepage: https://github.com/tommos0

;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Personal Emacs configuration
;;
;;; Code:

(provide 'init)

(setq gc-cons-threshold (* 50 1000 1000))

;; add local lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'private)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq native-comp-async-report-warnings-errors nil)

;; needs to be early to avoid loading built-in org mode by some other packages
(use-package org)

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-minibuffer t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-redo)
  :bind
  (:map evil-insert-state-map ("C-k" . nil))
  :config
  ;; use evil normal mode in 'rcirc' mode
  (evil-set-initial-state 'rcirc-mode 'normal)
  ;; (evil-set-initial-state 'rcirc-mode 'emacs)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package magit
  :custom
  (magit-diff-refine-hunk t)
  :config
  (require 'magit-extras))

(use-package forge
  :after magit
  :config
  ;; Token is stored in 'auth-sources
  (add-to-list 'forge-alist '("ahold" "api.github.com" "github.com" forge-github-repository)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-count 20)
  (vertico-cycle t)
  :bind
  (:map vertico-map
    ("C-j" . 'vertico-next)
    ("C-k" . 'vertico-previous)
    ("C-S-K" . 'vertico-scroll-down)
    ("C-S-J" . 'vertico-scroll-up)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :after (evil)
  :init
  (defun eshell/new ()
    "Create a new eshell buffer."
    (interactive)
    (eshell 'N))

  (defun consult-ripgrep/here ()
    "ripgrep in default-directory"
    (interactive)
    (consult-ripgrep default-directory (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))))

  (defun consult-ripgrep/project ()
    "ripgrep in project root"
    (interactive)
    (when (project-current)
      (consult-ripgrep (project-root (project-current)) (when (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))))))

  (defun consult-find/here ()
    "Find file by name in default-directory"
    (interactive)
    (consult-find default-directory))

  :config
  (evil-define-key 'insert eshell-mode-map (kbd "C-r") 'consult-history)

  :bind
  ("C-x / /" . consult-ripgrep/here)
  ("C-x p /" . consult-ripgrep/project)
  ("C-x / f" . consult-find/here)
  ("C-x / s" . consult-line)
  ("C-x b" . consult-buffer)
  ("C-x C-r" . consult-recent-file))

(use-package which-key
  :init
  (which-key-mode))

(use-package company
  :init
  (global-company-mode)
  :hook
  (eshell-mode . (lambda () (setq-local company-backends '(company-capf))))
  :config
  (evil-define-key 'insert eshell-mode-map (kbd "TAB") 'company-complete)
  :bind
  (:map company-active-map
    ("<return>" . nil)
    ("RET" . nil)
    ("<tab>" . company-complete))
  (:map evil-insert-state-map
    ("C-SPC" . company-complete)))

(use-package helpful
  :bind
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ("C-h o" . helpful-symbol))


(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-act)
   ("C-h B" . embark-bindings)))

(use-package vterm
  :init
  (defun vterm/new ()
    "Create a new vterm buffer."
    (interactive)
    (vterm 'N))
  :bind
  ("C-x / t" . vterm/new))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;(load-theme 'modus-vivendi t)

(use-package auto-dim-other-buffers
  :after doom-themes
  :init
  (auto-dim-other-buffers-mode)

  (defun darken (color amount)
    (let* ((rgb (color-name-to-rgb color))
           (rgb-darkened (mapcar (lambda(x) (* x amount)) rgb)))
          (apply 'color-rgb-to-hex rgb-darkened)))
  :config
  (set-face-attribute 'auto-dim-other-buffers-face nil
              :background (darken (face-attribute 'default :background) .7)))

(use-package markdown-mode)
(use-package embark-consult)
(use-package wgrep)
(use-package yaml-mode)
(use-package flymake-eslint)
(use-package elfeed)

(use-package git-gutter
  :custom
  (global-git-gutter-mode t))

(use-package nvm-switch
  :straight (nvm-switch
	     :type git
	     :host github
	     :repo "tommos0/nvm-switch.el"
	     :branch "main"))

(use-package jest-ts
  :straight (jest-ts
	     :type git
             :host github
	     :repo "tommos0/jest-ts.el"
	     :branch "master"))

(use-package gptel
  :straight (gptel
	     :type git
	     :host github
	     :repo "karthink/gptel"
	     :branch "master"))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package copilot
  :straight (copilot
         :type git
         :host github
         :repo "zerolfx/copilot.el"
         :files ("dist" "*.el"))
  :after (company)
  :config
  (defun company-copilot-accept ()
    "Accept copilot suggestion if company is not active."
    (interactive)
    (unless (company--active-p)
      (copilot-accept-completion)))
  (defun copilot-show-or-accept ()
    "Show or accept copilot suggestion."
    (interactive)
    (if (copilot--overlay-visible)
    (copilot-accept-completion)
    (copilot-complete)))
  :bind
  ("C-c C-l C-l" . copilot-mode)
  (:map copilot-mode-map
    ("C-c C-l TAB" . copilot-show-or-accept)
    ("C-c C-l C-j" . copilot-previous-completion)
    ("C-c C-l C-k" . copilot-next-completion))
  (:map copilot-completion-map
	("TAB" . company-copilot-accept)))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package diredfl
  :init
  (diredfl-global-mode))

(use-package browse-at-remote
  :config
  (add-to-list 'browse-at-remote-remote-type-regexps
               '(:host "ahold" :type "github" :actual-host "github.com")))

(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package graphql-mode
  :config
  (add-hook 'graphql-mode-hook 'eglot-ensure))


(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 25000)
  (recentf-max-menu-items 25))

(setq
  backup-directory-alist '(("." . "~/.emacs.saves"))
  backup-by-copying t
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(push (expand-file-name "~/.yarn/bin") exec-path)

(set-face-attribute 'font-lock-comment-face nil :foreground "#8F8F8F")
(set-face-attribute 'line-number nil :foreground "#777777")
(setq indent-tabs-mode nil)
(setq org-startup-indented t)
(save-place-mode 1)
(global-prettify-symbols-mode 1)
(global-display-line-numbers-mode 1)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(setq global-auto-revert-non-file-buffers 1)
(column-number-mode t)
(menu-bar-mode -1)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(defun flymake-eslint-enable--delayed ()
  "Seems necessary to make flymake-eslint actually enable."
  (run-at-time "0.01 sec" nil (lambda () (flymake-eslint-enable) (flymake-start))))

(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'flymake-eslint-enable--delayed)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'flymake-eslint-enable--delayed)

(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x C-k" #'kill-current-buffer)
(bind-key "C-x / p" #'yank-from-kill-ring)
(bind-key "M-p" #'yank-from-kill-ring)

(use-package eglot
  :ensure nil
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size 0)
  :config
  (add-to-list 'eglot-server-programs '(graphql-mode . ("graphql-lsp" "-m" "stream" "server")))
  (add-to-list 'eglot-server-programs `((typescript-ts-mode typescript-mode) .
				      ,(eglot-alternatives '(("typescript-language-server" "--stdio")
							     ("deno" "lsp" :initializationOptions (:enable t :lint t))))))
  :bind
  (:map eglot-mode-map ("C-c a" . eglot-code-actions)))

(use-package eshell
  :config
  (defun eshell-append-history ()
    "Call `eshell-write-history' with the `append' parameter set to `t'."
    (when eshell-history-ring
	(let ((newest-cmd-ring (make-ring 1)))
	(ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
	(let ((eshell-history-ring newest-cmd-ring))
	    (eshell-write-history eshell-history-file-name t)))))
  :custom
  (eshell-save-history-on-exit nil)
  (eshell-history-size 25000)

  :hook
  ;; Updates history after each command (instead of at exit).
  (eshell-pre-command . eshell-append-history)
  :bind
  ("C-x / e" . eshell/new))


;; (use-package prettier
;;   :hook
;;   (typescript-ts-mode . prettier-mode)
;;   (graphql-mode . prettier-mode)
;;   (json-mode . prettier-mode))

(use-package prettier-js
   :hook
   (typescript-ts-mode . prettier-js-mode)
   (tsx-ts-mode . prettier-js-mode)
   (graphql-mode . prettier-js-mode)
   (json-mode . prettier-js-mode)
   (js-json-mode . prettier-js-mode))

(defun prettier-js--post-ah-fix ()
  (save-excursion
    (beginning-of-buffer)
    (while (string-match-p "^\\[prettier\\].*" (thing-at-point 'line t))
        (kill-whole-line))))

(advice-add 'prettier-js :after 'prettier-js--post-ah-fix)

(use-package flymake
  :ensure nil
  :bind
  (:map flymake-mode-map
	("C-c ! l" . flymake-show-buffer-diagnostics)
	("C-c ! p" . flymake-goto-prev-error)
	("C-c ! n" . flymake-goto-next-error)))

(use-package git-timemachine
  :straight (git-timemachine
	     :type git
	     :host github
	     :repo "emacsmirror/git-timemachine"
	     :branch "master"))

(use-package elf-mode)

(defun xdg-open () (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (shell-command (concat "xdg-open " file)))))

(defun yank-buffer-path (&optional root)
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or (buffer-file-name (buffer-base-buffer))
                        (bound-and-true-p list-buffers-directory)))
      (let ((path (abbreviate-file-name
                   (if root
                       (file-relative-name filename root)
                     filename))))
        (kill-new path)
        (if (string= path (car kill-ring))
            (message "Copied path: %s" path)
          (user-error "Couldn't copy filename in current buffer")))
    (error "Couldn't find filename in current buffer")))

(bind-key "C-x / y" #'yank-buffer-path)
(defun dired-copy-file-path-as-kill () (interactive) (dired-copy-filename-as-kill 0))
(bind-key "C-c C-y" #'dired-copy-file-path-as-kill 'dired-mode-map)

;; So that language=typescript works in org mode
(define-derived-mode typescript-mode typescript-ts-mode "typescript")
(define-derived-mode json-mode json-ts-mode "json")

(require 'headphone)
(require 'cross-eval)

(load-file "/home/tomk/.doom.d/private.el")

;; font settings
(set-face-attribute 'default nil :family "SourceCodeVS" :height 105)
;(set-face-attribute 'default nil :family "Fira Code" :height 105)
;(set-face-attribute 'default nil :family "SourceCodeVS" :height 140)

(use-package eglot-java
  :custom
  (eglot-java-eclipse-jdt-args '(
    "-noverify"
    "-Xmx1G"
    "-XX:+UseG1GC"
    "-XX:+UseStringDeduplication"
    "-javaagent:/home/tomk/Downloads/lombok.jar"
    "-Xbootclasspath/a:/home/tomk/Downloads/lombok.jar"))
  :hook
  (java-mode . eglot-java-mode))

;(use-package eshell-git-prompt)
(setenv "EDITOR" "emacsclient")
(tool-bar-mode -1)
(setq frame-resize-pixelwise t)
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package sudo-edit)
(use-package avy
  :bind
  ("C-a" . avy-goto-char-timer))

(require 'ob-shell)

;(setq eglot-events-buffer-size 400000000)


(use-package htmlize)
;(use-package combobulate)

;; (defun toggle-maximize-buffer () "Maximize buffer"
;;   (interactive)
;;   (if (= 1 (length (window-list)))
;;       (jump-to-register '_)
;;     (progn
;;       (window-configuration-to-register '_)
;;       (delete-other-windows))))


(setq vc-follow-symlinks nil)
(use-package string-inflection)

(defvar project-start-command)

(put 'project-start-command 'safe-local-variable
     (lambda (x) t))

(defun project-start ()
  (interactive)
  (when project-start-command
    (let ((buffer-name (concat "Run *" (project-name (project-current)) "*")))
      ;; if the buffer doesn't exist, run the command
      (if (get-buffer buffer-name)
	(switch-to-buffer buffer-name)
	(let ((default-directory (project-root (project-current))))
	  (async-shell-command project-start-command buffer-name))))))

(bind-key "<f5>" #'project-start)

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun open-in-vscode ()
  (interactive)
  (let* ((file (buffer-file-name))
         (line (number-to-string (line-number-at-pos)))
         (col (number-to-string (+ 1 (current-column))))
         (filestr (concat file ":" line ":" col)))
    (start-process "code" nil "/usr/bin/code" "--goto" filestr)))

(setq create-lockfiles nil)

(use-package org-download)
(use-package sly)
(use-package lispyville)
(use-package smartparens)
(use-package dtrt-indent)
(use-package org-roam)
(use-package command-log-mode)
(use-package docker
  :bind
  ("C-x d" . docker))

(setenv "KUBECONFIG" "/home/tomk/.kube/ah.yaml")
(use-package kubernetes)

(use-package ob-mermaid
  :custom
  (ob-mermaid-cli-path "/home/tomk/.nvm/versions/node/v18.13.0/bin/mmdc")
  :config
  (org-babel-do-load-languages
	'org-babel-load-languages
	'((mermaid . t))))

(defun clear-ahgraphql-turbo-cache () (interactive) (async-shell-command "rm -rf /home/tomk/git/ah/ah-graphql/node_modules/.cache/"))

(defun insert-shell-command-as-comment (command)
  (interactive)
  (save-excursion
    (end-of-line)
    (let* ((output (shell-command-to-string command))
	  (commented-string (concat "\n;; " (replace-regexp-in-string "\n" "\n;; " output))))
      (insert commented-string))))

(put 'dired-find-alternate-file 'disabled nil)

(defvar org-auto-redisplay-after-eval t)
(defun org-redisplay-when-auto-redisplay ()
  "Redisplay when `org-auto-redisplay-after-eval' is non-nil."
  (when org-auto-redisplay-after-eval
    (org-redisplay-inline-images)))

(advice-add 'org-babel-execute-src-block
	    :after 'org-redisplay-when-auto-redisplay)

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(setq-default indent-tabs-mode nil)

(setq use-short-answers t)

;;; init.el ends here
