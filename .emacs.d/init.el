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
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(overwrite-mode 1)


;; Força Emacs a abrir buffer de compilação na vertical (lado direito)
(setq display-buffer-alist
      '(("\\*compilation\\*"
         (display-buffer-in-side-window) ;; usa side-window
         (side . right)                   ;; lado direito
         (window-width . 80)              ;; largura inicial
         (slot . 1)
         (window-parameters . ((no-delete-other-windows . t)
                               (no-other-window . t))))))

;; Mantém scroll automático
(setq compilation-scroll-output t)

;; Não fecha o buffer ao terminar
(setq compilation-finish-functions nil)




(set-face-attribute 'default nil :font "Iosevka-12")
(add-to-list 'default-frame-alist '(font . "Iosevka-12"))



(defun my/refresh-dired-after-file-create ()
  "Se o arquivo foi criado com helm-find-files, atualiza o dired."
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name)))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (eq major-mode 'dired-mode)
                     (string= (expand-file-name default-directory)
                              (expand-file-name dir)))
            (revert-buffer nil t)))))))

(add-hook 'find-file-hook #'my/refresh-dired-after-file-create)


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




;
;    WHITESPACES
;

(require 'whitespace)
(global-whitespace-mode 1)

;; Mostrar espaços e fim de linha
(setq whitespace-style '(face spaces trailing space-mark newline-mark))

;; Configuração visual
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])   ;; espaço simples como ·
        ;(newline-mark 10 [36 10])    ;; fim de linha como $ + newline
        ))

;; Configurar cores
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#1E1E1E"))) t)
 '(company-scrollbar-fg ((t (:background "#555555"))) t)
 '(company-tooltip ((t (:background "#2E2E2E" :foreground "#CCCCCC"))))
 '(company-tooltip-annotation ((t (:foreground "#AAAAAA"))))
 '(company-tooltip-common ((t (:foreground "#BBBBBB"))))
 '(company-tooltip-selection ((t (:background "#3A3A3A" :foreground "#FFFFFF"))))
 '(whitespace-space ((t (:foreground "gray18"))))
 '(whitespace-trailing ((t (:background "light coral")))))






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

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-d") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "C-4") 'zoom-window-zoom)

(global-set-key (kbd "C-s") 'save-buffer)
(defun save-and-exit ()
  "save and quit Emacs"
  (interactive)
  (save-some-buffers t)
  (kill-emacs))
(global-set-key (kbd "C-S-s") 'save-and-exit)



(defun apagar-linha ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(global-set-key (kbd "C-v") 'yank)


(setq helm-ff-transformer-show-only-basename nil)

;; HELM
(global-set-key (kbd "C-h b") 'helm-bookmarks)
(global-set-key (kbd "C-h h") 'helm-recentf)
(global-set-key (kbd "C-h f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "C-c c") 'bookmark-set)
(global-set-key (kbd "C-c d") 'bookmark-delete)
(global-set-key (kbd "C-S-e") 'dired-jump)


(global-set-key (kbd "M-<tab>") 'ace-window)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-\"") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:") 'mc/skip-to-previous-like-this)

;; java Keymaps
(global-set-key (kbd "C-.") 'lsp-java-add-import)
(global-set-key (kbd "C-r") 'lsp-rename)
(global-set-key (kbd "C-c f") 'lsp-format-buffer)
(global-set-key (kbd "C-c g") 'lsp-goto-implementation)

(global-set-key (kbd "S-`") 'shell-command)

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
    ts-mode
    js-mode
    go-mode
    c-mode
    c++-mode
    rust-mode
    yaml-mode
    prog-mode) . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-server-install-dir "/usr/local/share/lsp-servers")


  (defun my/lsp-auto-install ()
    "Instala automaticamente o servidor LSP se não estiver presente."
    (dolist (server (lsp--suggest-server-downloads))
      (unless (lsp-server-present-p server)
        (lsp-install-server server))))

  (add-hook 'lsp-mode-hook #'my/lsp-auto-install))


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))






;; typescript
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp))

(use-package web-mode
  :ensure t
  :mode ("\\.tsx\\'" . web-mode)
  :hook (web-mode . lsp)
  :config
  (setq web-mode-enable-auto-quoting nil) ;; desativa auto-quoting, opcional
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(use-package flyspell
  :ensure t
  :defer t)


;;yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :hook (yaml-mode . my/yaml-mode-setup))

(defun my/yaml-mode-setup ()
  "Configurações adicionais para YAML."
  (setq yaml-indent-offset 2)
  (flycheck-mode 1)
  (company-mode 1))

;; Java
(use-package lsp-java
  :after lsp
  :config
  (add-hook 'java-mode-hook 'lsp))
(setq company-backends '(company-capf company-dabbrev company-files))



;; lsp-idle-delay 0.6
;;         lsp-log-io nil
;;         lsp-enable-symbol-highlighting t
;;         lsp-headerline-breadcrumb-enable t
;;         lsp-completion-provider :capf

;; -------------------------------
;; Company-mode
;; -------------------------------
(use-package company
  :hook (after-init . global-company-mode)   ;; ativa globalmente
  :config
  (setq company-idle-delay 0.2)             ;; tempo antes de aparecer sugestão
  (setq company-minimum-prefix-length 1)    ;; começa a sugerir depois de 1 char
  (setq company-tooltip-limit 10)           ;; max de sugestões na lista
  (setq company-show-numbers t)             ;; mostra números para completar com M-1, M-2...
  (setq company-tooltip-align-annotations t)
  (setq company-require-match 'never)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-downcase nil)
  ;; Atalhos
  (global-set-key (kbd "M-/") 'company-complete) ;; manualmente disparar
  )

;; Navegação dentro do menu
(with-eval-after-load 'company

  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "<return>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))


;; -------------------------------
;; Backends recomendados
;; -------------------------------
(setq company-backends '(
                         company-capf       ;; integração com lsp, tree-sitter
                         company-files      ;; caminhos de arquivos
                         company-keywords   ;; palavras-chave da linguagem
                         company-dabbrev    ;; autocomplete de palavras do buffer
                         ))

;; -------------------------------
;; Visual de tooltip
;; -------------------------------
(setq company-tooltip-minimum-width 20)  ; largura mínima
(setq company-tooltip-maximum-width 80)  ; largura máxima



;; -------------------------------
;; Optional: mostrar número de sugestões
;; -------------------------------
(setq company-show-numbers t)

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
(require 'autoinsert)
(auto-insert-mode 1)

(setq auto-insert-query nil) ;; não perguntar antes de inserir

(defun my/java-package-name ()
  "Gera o nome do pacote Java baseado no caminho do arquivo."
  (let* ((file (buffer-file-name))
         (src-root (locate-dominating-file file "src/main/java"))
         (rel-path (file-relative-name file (concat src-root "src/main/java/")))
         (pkg (file-name-directory rel-path)))
    (when pkg
      (replace-regexp-in-string "/" "." (directory-file-name pkg)))))

(defun my/java-template ()
  "Insere template Java com package e escolha de tipo pelo número."
  (interactive)
  (let* ((filename (file-name-base (buffer-file-name))) ;; Nome do arquivo sem extensão
         (classname filename)
         (package (my/java-package-name))
         (choice (read-number "Escolha (1=class, 2=interface, 3=record, 4=enum): ")))
    ;; Inserir package se encontrado
    (when package
      (insert "package " package ";\n\n"))
    ;; Inserir tipo baseado na escolha
    (cond
     ((= choice 1)
      (insert "public class " classname " {\n\n}\n"))
     ((= choice 2)
      (insert "public interface " classname " {\n\n}\n"))
     ((= choice 3)
      (insert "public record " classname " () {\n\n}\n"))
     ((= choice 4)
      (insert "public enum " classname " {\n\n}\n"))
     (t
      (insert "// Tipo inválido, escolha 1=class, 2=interface, 3=record, 4=enum\n")))))

(define-auto-insert "\\.java\\'" #'my/java-template)



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
 '(custom-enabled-themes '(gruber-darker))
 '(custom-safe-themes
   '("9aa753a939d3e81b95f06680656d0d57b22296cbe67a0e938a008b20d2811abb"
     "c5801b68568b59976a8e58104c40c9b052d46cca72e367c2e43c1f36a9e79abb"
     "2614a89f7e54cd9512343a3efba0e084fb9568c38e11165d7109a887e924a970"
     "227dd4519dd40777533a58f905dccb9ca61928aef9f52761c8194e2e8bbb5f8b"
     "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c"
     "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7"
     "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2"
     "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae"
     "01a9797244146bbae39b18ef37e6f2ca5bebded90d9fe3a2f342a9e863aaa4fd"
     default))
 '(elcord-editor-icon "doom_cute_icon")
 '(elcord-mode t nil (elcord))
 '(elcord-refresh-rate 3)
 '(elcord-show-small-icon t)
 '(package-selected-packages nil))

