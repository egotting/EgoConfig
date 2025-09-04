(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;; Ativa integração com use-package
(elpaca elpaca-use-package
  (elpaca-use-package-mode))


;; Configurações básicas da interface
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
;(show-paren-mode 1)
(global-display-line-numbers-mode t)

(setq custom-file "~/.emacs.custom.el")

(add-to-list 'default-frame-alist '(font . "Iosevka-16"))

(eval-after-load 'zenburn
  (set-face-attribute 'line-number nil :inherit 'default))

;; Auto insert
(auto-insert-mode 1)
(setq auto-insert-query nil)

;; ===========================
;; Configurações externas (rc)
;; ===========================
(load "~/.emacs.rc/rc.el")
(load "~/.emacs.rc/misc-rc.el")
(load "~/.emacs.rc/org-mode-rc.el")
(load "~/.emacs.rc/autocommit-rc.el")

;; ===========================
;; Ajustes de split de janela
;; ===========================
(with-eval-after-load "window"
  (defcustom split-window-below nil
    "If non-nil, vertical splits produce new windows below."
    :group 'windows
    :type 'boolean)
  (defcustom split-window-right nil
    "If non-nil, horizontal splits produce new windows to the right."
    :group 'windows
    :type 'boolean)
  (fmakunbound #'split-window-sensibly)
  (defun split-window-sensibly (&optional window)
    (setq window (or window (selected-window)))
    (or (and (window-splittable-p window t)
             (split-window window nil (if split-window-right 'left  'right)))
        (and (window-splittable-p window)
             (split-window window nil (if split-window-below 'above 'below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil (if split-window-right 'left 'right))))))))
(setq-default split-height-threshold  4
              split-width-threshold   160)

;; ===========================
;; Whitespace mode
;; ===========================
(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(dolist (hook '(tuareg-mode-hook c++-mode-hook c-mode-hook simpc-mode-hook
                 emacs-lisp-mode java-mode-hook lua-mode-hook rust-mode-hook
                 scala-mode-hook markdown-mode-hook haskell-mode-hook
                 python-mode-hook erlang-mode-hook asm-mode-hook fasm-mode-hook
                 go-mode-hook nim-mode-hook yaml-mode-hook porth-mode-hook))
  (add-hook hook 'rc/set-up-whitespace-handling))


;; ===========================
;; Pacotes (use-package)
;; ===========================
(use-package try :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package company
  :ensure t
  :init (global-company-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((python-mode java-mode js-mode typescript-mode c-mode c++-mode
                      go-mode rust-mode ruby-mode) . lsp)
  :config
  (setq lsp-prefer-flymake nil
        lsp-enable-snippet t
        lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t
        lsp-ui-imenu-enable t
        lsp-ui-peek-enable t))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package ace-window :ensure t)
(use-package zoom-window
  :ensure t)
(use-package projectile :ensure t)
(use-package yasnippet :ensure t :config (yas-global-mode))
(use-package hydra :ensure t)
(use-package helm-lsp :ensure t)
(use-package helm
  :ensure t
  :init
  (helm-mode 1))
(use-package lsp-treemacs :ensure t)

(use-package autoinsert
  :ensure nil
  :config (auto-insert-mode 1))

(use-package lsp-java
  :ensure t
  :after lsp
  :hook (java-mode . lsp)
  :config
  (setq lsp-java-format-enabled t)
  (add-hook 'java-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
(defun my-java-package-name ()
  "Gera o package Java a partir do diretório do arquivo atual."
  (let* ((file (file-name-directory (buffer-file-name)))
         (src-root (expand-file-name "~/projetos/app/src/main/java/"))
         (rel (file-relative-name file src-root)))
    (when (not (string-match-p "\\.\\." rel))
      (replace-regexp-in-string "/" "." (directory-file-name rel)))))


(define-auto-insert
  '("\\.java\\'" . "Java skeleton")
  '((lambda ()
      (let* ((package (my-java-package-name))
             (class-name (file-name-base (buffer-file-name))))
        (when package
          (insert "package " package ";\n\n"))
        (insert "public class " class-name " {\n\n")
        (insert "}\n")))))

(use-package dap-java
  :ensure nil
  :after (lsp-java)
  :config
  (global-set-key (kbd "<f7>") 'dap-step-in)
  (global-set-key (kbd "<f8>") 'dap-next)
  (global-set-key (kbd "<f9>") 'dap-continue))

(use-package transient :ensure t)
(use-package magit :ensure t)

;; ===========================
;; Keymaps
;; ===========================
(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "C-S-w") 'kill-region)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-e") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "C-4") 'zoom-window-zoom)
(defun apagar-linha ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)))
(global-set-key (kbd "C-d") 'apagar-linha)

(global-set-key (kbd "C-v") 'yank)

(defun save-and-exit ()
  "save and quit Emacs"
  (interactive)
  (save-some-buffers t)
  (kill-emacs))
(global-set-key (kbd "C-S-s") 'save-and-exit)


(global-set-key (kbd "C-c b") 'helm-bookmarks)
(global-set-key (kbd "C-c c") 'bookmark-set)
(global-set-key (kbd "C-S-e") 'dired-jump)
(global-set-key (kbd "C-S-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-<tab>") 'ace-window)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-r") 'lsp-rename)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-\"") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:") 'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-.") 'lsp-java-add-import)

;; ===========================
;; Custom
;; ===========================
(custom-set-variables
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd" default))
 '(package-selected-packages nil))

(custom-set-faces)
