(setq package-enable-at-startup nil)

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

(use-package almost-mono-themes)


(setq gc-cons-threshold (* 200 1024 1024)) ; 200 MB
;; Configurações básicas da interface
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)
;(global-display-line-numbers-mode t)


(add-to-list 'default-frame-alist '(font . "Iosevka-18"))

(eval-after-load 'zenburn
  (set-face-attribute 'line-number nil :inherit 'default))
(set-face-attribute 'default nil :height 150)
;; Auto insert
(auto-insert-mode 1)
(setq auto-insert-query nil)


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
;; Mostra apenas o final de linha com $
(setq whitespace-style '(lines-tail newline-mark))

;; Define como o fim de linha deve aparecer
(setq whitespace-display-mappings
      '((newline-mark 10 [?$ 10]))) ;; mostra $ no final

(global-whitespace-mode 1) ;; ou só (whitespace-mode 1) se quiser por buffer


(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-mouse-drag-files t)

;; ===========================
;; Keymaps
;; ===========================
(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "C-S-w") 'kill-region)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-d") 'delete-window)
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

(setq helm-ff-transformer-show-only-basename nil)

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
;; (custom-set-variables
;;  '(custom-enabled-themes '(gruber-darker))
;;  '(custom-safe-themes
;;    '("01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd" default))
;;  '(package-selected-packages nil))

;; (custom-set-faces)



;; Mostrar atalhos disponíveis
(use-package which-key
  :diminish
  :hook (after-init . which-key-mode))


(use-package lsp-mode
  :ensure t
  :hook
  ((java-mode
    python-mode
    typescript-mode
    js-mode
    go-mode
    c-mode
    c++-mode
    rust-mode) . lsp)
  :commands (lsp lsp)
  :config
  (setq lsp-idle-delay 0.6
        lsp-log-io nil
        lsp-enable-symbol-highlighting t
        lsp-headerline-breadcrumb-enable t
        lsp-completion-provider :capf
        lsp-prefer-flymake nil))



;; ===========================
;; Company (auto-complete)
;; ===========================
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.2))


;; ===========================
;; Java LSP (JDTLS)
;; ===========================
;; (use-package lsp-java
;;   :ensure t
;;   :after lsp
;;   :config
;;   (add-hook 'java-mode-hook #'lsp))


;; (with-eval-after-load 'lsp-mode
;;   (require 'lsp-intellij)
;;   (add-hook 'java-mode-hook #'lsp-intellij-enable))

;; Debug para Java
(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package dap-java :after lsp-java
  :config
  (global-set-key (kbd "<f7>") 'dap-step-in)
  (global-set-key (kbd "<f8>") 'dap-next)
  (global-set-key (kbd "<f9>") 'dap-continue))

;; Auto-insert para arquivos Java
(use-package autoinsert
  :init (auto-insert-mode 1))

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

;; ===========================
;; Extras
;; ===========================

(use-package helm
  :init (helm-mode 1))

(use-package helm-lsp :after (helm lsp-mode))

(use-package lsp-treemacs :after lsp-mode)

(use-package projectile :defer t)

(use-package ace-window :defer t)

(use-package zoom-window :defer t)

(use-package hydra :defer t)

(use-package transient :defer t)

(use-package magit :defer t)

(use-package try :defer t)


(require 'elcord)
  (elcord-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(almost-mono-white))
 '(custom-safe-themes
   '("cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2"
     "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae"
     "01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd"
     default))
 '(package-selected-packages
   '(almost-mono-themes auto-complete auto-yasnippet company consult
                        dash-functional eglot elcord flycheck
                        gruber-darker-theme helm-lsp
                        ido-completing-read+ lsp-intellij lsp-java
                        lsp-ui magit multiple-cursors neotree
                        org-cliplink projectile quelpa smex try)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
