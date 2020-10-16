
;; (setq doom-theme 'doom-tomorrow-night)
(setq doom-theme 'afternoon)

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

;; ace-jump-mode
(global-set-key (kbd "C-0") 'ace-jump-mode)
(global-set-key (kbd "C-9") 'ace-jump-line-mode)

;; Scratch
(defun new-scratch ()
  "open up a guaranteed new scratch buffer"
  (interactive)
  (switch-to-buffer (loop for num from 0
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
(global-set-key (kbd "<f8>") 'magit-status)

;; next-error and previous-error
(global-set-key (kbd "C-x C-n") 'flycheck-next-error)
(global-set-key (kbd "C-x C-p") 'flycheck-previous-error)

;; modes
(whole-line-or-region-mode)
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

;; projectile-find-file
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
  )

(add-hook! 'haskell-mode-hook
  (defun haskell-sort-imports()
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      ;; Sort the initial import block
      (when (search-forward "import" nil t)
        (beginning-of-line)
        (haskell-sort-imports)
        )
      ;; Sort any following import blocks
      (while (search-forward "
import" nil t)
        (beginning-of-line)
        (haskell-sort-imports)
        ))
    )
  )

(after! ibuffer-vc
  (add-hook! 'ibuffer-hook
    (lambda ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(after! smartparens
  (load "/home/tom/doom-config/my-smartparens.el")
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

(after! company
  (setq company-bg-color (face-attribute 'default :background))

  (add-to-list 'company-backends 'company-files)

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

(after! typescript-mode
  (setq typescript-indent-level 2)
  (setq js-indent-level 2)
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

(after! smartparens-mode
  (define-key sp-keymap (kbd "C-<left>") 'back-button-global-backward)
  (define-key sp-keymap (kbd "C-<right>") 'back-button-global-forward)
  )

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
