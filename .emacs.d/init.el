(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(better-defaults
                      find-file-in-project
                      smex
                      groovy-mode
                      soft-charcoal-theme
                      color-theme-sanityinc-tomorrow 
                      buffer-move
                      markdown-mode
                      highlight-symbol
                      htmlize
                      org))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defvar ffip-project-file '(".hg" ".git"))
(defvar ffip-limit 4096)
(defvar ffip-patterns '("*.java" "*.cpp" "*.h" "*.xml" "*.gradle" "*.txt"))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "<f6>") 'compile)
(global-set-key (kbd "<f7>") 'recompile)
(global-set-key (kbd "C-x f") 'ffip)
(global-set-key (kbd "C-c o") 'ff-find-other-file)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; F3 highlight symbols like vim's *
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-on-navigation-p t)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)

;; Change size of markdown headers
(add-hook 'markdown-mode-hook
          (lambda ()
              (message "Entering markdown mode")
              (set-face-attribute 'markdown-header-delimiter-face nil :underline t :weight 'bold)
              (set-face-attribute 'markdown-header-face-1 nil  :height 1.5)
              (set-face-attribute 'markdown-header-face-2 nil  :height 1.3)
              (set-face-attribute 'markdown-header-face-3 nil  :height 1.2 :underline t)
              (set-face-attribute 'markdown-header-face-4 nil  :height 1.1 :underline t)
              (set-face-attribute 'markdown-header-face-5 nil  :underline t)
              (set-face-attribute 'markdown-header-face-6 nil  :underline t)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))

(setq column-number-mode t
      inhibit-startup-screen t
      safe-local-variable-values '((eval highlight-regexp "	")
                                   (c-default-style "linux"))
      custom-safe-themes t
      vc-follow-symlinks nil
      
      org-src-fontify-natively t)
      
(autoload 'hg-status "mercurial" "Entry point into hg-status mode." t)
(autoload 'git-status "git" "Entry point into git-status mode." t)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)
(autoload 'cmake-mode "cmake-mode" "A major mode for CMake" t)


;; TODO move to a separate configuration file

(autoload 'mu4e "mu4e" "mu4e email reader" t)

;; these are actually the defaults
(setq mu4e-maildir       "~/Mail"      ;; top-level Maildir
      mu4e-sent-folder   "/sent"       ;; folder for sent messages
      mu4e-drafts-folder "/drafts"     ;; unfinished messages
      mu4e-trash-folder  "/trash"      ;; trashed messages
      mu4e-refile-folder "/archive")   ;; saved messages

(setq mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
      mu4e-update-interval 300)             ;; update every 5 minutes

;; smtp mail setting; these are the same that `gnus' uses.
(setq send-mail-function           'smtpmail-send-it
      message-send-mail-function   'smtpmail-send-it
      smtpmail-default-smtp-server "orin.kylewm.com"
      smtpmail-smtp-server         "orin.kylewm.com"
      smtpmail-smtp-service        587
      smtpmail-local-domain        "kylewm.com")

     ;; general emacs mail settings; used when composing e-mail
     ;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "kyle@kylewm.com"
      user-mail-address "kyle@kylewm.com"
      user-full-name  "Kyle Mahan")

(if window-system
    (progn
      (load-theme 'sanityinc-tomorrow-night)
      (set-face-font 'default (if (eq window-system 'w32)
                                  "Consolas-10" "Liberation Mono-10"))))
