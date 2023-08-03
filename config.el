
;; (setq doom-theme 'doom-tomorrow-night)
;; (setq doom-theme 'afternoon)

(setq enable-local-variables t)

;; Doom uses dtrt-indent-mode, which doesn't work well in web-mode when multiple
;; indentation styles are present (i.e. for code vs. JSX)
(add-to-list 'doom-detect-indentation-excluded-modes 'web-mode)
(add-to-list 'doom-detect-indentation-excluded-modes 'typescript-tsx-mode)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(90 90))))

;; Help
(global-set-key (kbd "C-?") 'help-command)

;; Editing
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-M-h") 'backward-kill-sexp)
(global-set-key (kbd "C-j") 'newline-and-indent)
(global-set-key (kbd "C-e") 'move-end-of-line)

;; ace-jump-mode
(global-set-key (kbd "C-0") 'ace-jump-mode)
(global-set-key (kbd "C-9") 'ace-jump-line-mode)

;; Scratch
(defun new-scratch ()
  "open up a guaranteed new scratch buffer"
  (interactive)
  (switch-to-buffer (cl-loop for num from 0
                             for name = (format "scratch-%03i" num)
                             while (get-buffer name)
                             finally return name)))
(global-set-key (kbd "<f7>") 'new-scratch)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; magit
(global-set-key (kbd "C-m") 'magit-status)

;; next-error and previous-error
(global-set-key (kbd "C-x C-n") 'flycheck-next-error)
(global-set-key (kbd "C-x C-p") 'flycheck-previous-error)

;; modes
(whole-line-or-region-global-mode)
(global-subword-mode)

;; projectile-helm-ag
(defun projectile-helm-ag (arg)
  "Run helm-do-ag relative to the project root.  Or, with prefix arg ARG, relative to the current directory."
  (interactive "P")
  (if arg
      (progn
        ;; Have to kill the prefix arg so it doesn't get forwarded
        ;; and screw up helm-do-ag
        (set-variable 'current-prefix-arg nil)

        (if dired-directory
            (helm-do-ag dired-directory)
          (helm-do-ag (file-name-directory (buffer-file-name)))
          )
        )
    (helm-do-ag (projectile-project-root))
    ))
(global-set-key (kbd "C-x C-r") 'projectile-helm-ag)

;; Disable line numbers
(setq display-line-numbers-type nil)

;; expand-region
(global-set-key "\M-s" 'er/expand-region)

;; projectile and dired
(global-set-key (kbd "C-t") 'projectile-find-file)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(after! dired
  (define-key dired-mode-map (kbd "C-t") 'projectile-find-file)
  (setq! dired-listing-switches "-ahl -v")
  )
(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "*.stack-work")
  (add-to-list 'projectile-globally-ignored-directories "*test_runs")
  (add-to-list 'projectile-globally-ignored-directories "*node_modules")
  )

;; (after! lsp
  (setq! lsp-enable-file-watchers nil)
  ;; )

;; comment
(global-set-key (kbd "M-;") 'whole-line-or-region-comment-dwim)

(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; iflipb
(global-set-key (kbd "M-j") 'iflipb-next-buffer)
(global-set-key (kbd "M-k") 'iflipb-previous-buffer)

(global-set-key (kbd "<S-left>") 'windmove-left)
(global-set-key (kbd "<S-right>") 'windmove-right)
(global-set-key (kbd "<S-up>") 'windmove-up)
(global-set-key (kbd "<S-down>") 'windmove-down)

(after! haskell
  (setq haskell-interactive-popup-errors nil)
  (setq haskell-process-suggest-remove-import-lines nil)
  (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)
  (define-key haskell-indentation-mode-map (kbd "C-m") 'magit-status)
  )

;; (after! nix
;;   (message "SETTING UP NIX LSP STUFF")
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
;;                     :major-modes '(nix-mode)
;;                     :server-id 'nix)))

(after! haskell
  (defun custom-sort-haskell-imports()
    (interactive)
    (save-excursion
      (goto-char (point-min))

      ;; Sort the initial import block
      (when (search-forward "import" nil t)
        (beginning-of-line)
        (haskell-sort-imports)
        )

      ;; Sort any following import blocks
      (while (search-forward "
        import" nil t)
        (beginning-of-line)
        (custom-sort-haskell-imports)
        )
      )
    )

  ;; (add-hook! 'haskell-mode-hook
  ;;   (add-hook! 'before-save-hook #'custom-sort-haskell-imports))

  (defun my-haskell-mode-before-save-hook ()
    (when (eq major-mode 'haskell-mode)
      (custom-sort-haskell-imports)))
  (add-hook 'before-save-hook #'my-haskell-mode-before-save-hook)

  ;; (set-formatter! 'haskell-format-imports 'custom-sort-haskell-imports :modes '(haskell-mode))
  )

(after! ibuffer-vc
  (add-hook! 'ibuffer-hook
    (lambda ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(after! smartparens
  (sp-use-paredit-bindings)

  ;; Custom keys
  (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

  ;; Clear certain keys we want to use for other things
  (define-key smartparens-mode-map (kbd "M-j") nil)
  (define-key smartparens-mode-map (kbd "M-s") nil)
  (define-key smartparens-mode-map (kbd "C-<left>") nil)
  (define-key smartparens-mode-map (kbd "C-<right>") nil)
  )

(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Put some Doom defaults back to normal
(global-set-key (kbd "C-_") 'undo)
(global-set-key (kbd "C-a") 'beginning-of-line)

(global-visual-line-mode)

;; (speedbar-add-supported-extension ".hs")

(after! helm
  ;; Default green selection color is hideous
  (custom-set-faces
   '(helm-selection ((t :background "gray25" :distant-foreground "black" :foreground "white smoke")))
   )

  (defun helm-find-files-in-root ()
    "Open helm-find-files in the current project root."
    (interactive)
    (helm-find-files-1 (doom-project-root))
    )
  (global-set-key (kbd "C-c C-f") 'helm-find-files-in-root)

  ;; (add-to-list 'completion-styles 'flex)
  ;; (setq completion-styles '(flex))
  ;; (setq helm-completion-style 'helm-fuzzy)
  )

(after! (:and helm helm-buffers)
  (setq! helm-buffers-sort-fn #'helm-fuzzy-matching-sort-fn-preserve-ties-order)
  )

(after! projectile
  ;; (setq! projectile-indexing-method 'alien)
  (setq! projectile-require-project-root t)
  )

(after! company
  (setq company-bg-color (face-attribute 'default :background))

  (add-to-list 'company-backends 'company-files)

  (setq company-idle-delay 0)

  (custom-set-faces
   '(company-preview-common ((t (:background "#21e824bc35b0"))))
   '(company-scrollbar-bg ((t (:background "#2bd12f784561"))))
   '(company-scrollbar-fg ((t (:background "#21e824bc35b0"))))
   '(company-tooltip ((t (:inherit default :background "#1bf61e4b2c46"))))
   '(company-tooltip-annotation ((t (:foreground "deep sky blue"))))
   '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :foreground "deep sky blue" :weight bold))))
   '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
   '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   )
  )

(add-hook! 'json-mode-hook
  (lambda ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2)))

;; (add-hook! 'typescript-tsx-mode-hook
;;   (unless (locate-dominating-file default-directory ".prettierrc")
;;     (format-all-mode -1)))

(after! (:or typescript-mode typescript-tsx-mode)
  (setq typescript-indent-level 2)
  )

(after! web-mode
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 4)
  (define-key web-mode-map (kbd "M-/") nil)
  (define-key web-mode-map (kbd "TAB") 'indent-for-tab-command)
  (define-key emmet-mode-keymap (kbd "TAB") 'indent-for-tab-command)
  )

(after! helm-swoop
  (global-set-key (kbd "C-c C-o") 'helm-swoop)
  )

(defun nautilus ()
  "Open nautilus in the current directory."
  (interactive)
  (if (use-region-p)
      (let ((path (buffer-substring (region-beginning) (region-end))))
        (if (file-name-absolute-p path)
            (shell-command (concat "nautilus " path))
          (shell-command "nautilus .")
          ))
    (shell-command "nautilus .")
    ))

(define-key global-map "\M-*" 'pop-tag-mark)

(global-set-key (kbd "C-<left>") 'back-button-global-backward)
(global-set-key (kbd "C-<right>") 'back-button-global-forward)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "M-=") 'doom/increase-font-size)
(global-set-key (kbd "M--") 'doom/decrease-font-size)

(global-set-key (kbd "M-o") 'hs-toggle-hiding)

(add-hook! 'before-save-hook 'delete-trailing-whitespace)

(setq indent-tabs-mode nil)

(use-package! dhall-mode
  :mode "\\.dhall$")
