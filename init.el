(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(better-defaults
                      smex
                      groovy-mode
                      soft-charcoal-theme
                      color-theme-sanityinc-tomorrow
                      buffer-move
                      markdown-mode
                      highlight-symbol
                      ctags))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(autoload 'find-file-in-project "find-file-in-project" "Quickly jump to files in the current project" t)
(defvar ffip-project-file '(".hg" ".git"))
(defvar ffip-limit 4096)
(defvar ffip-patterns '("*.java" "*.cpp" "*.h" "*.xml" "*.gradle" "*.txt"))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command);; This is your old M-x.

(global-set-key (kbd "<f6>")         'compile)
(global-set-key (kbd "<f7>")         'recompile)
(global-set-key (kbd "C-x f")        'ffip)
(global-set-key (kbd "C-c o")        'ff-find-other-file)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(global-set-key (kbd "C-c C-t")    'toggle-truncate-lines)

;; ctags
(global-set-key (kbd "<f8>") 'ctags-create-or-update-tags-table)
;;(global-set-key (kbd "M-.")  'ctags-search)


;; F3 highlight symbols like vim's *
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-on-navigation-p t)
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "<S-f3>") 'highlight-symbol-prev)

;; F5 to refresh dired buffer
(eval-after-load 'dired '(define-key dired-mode-map (kbd "<f5>") 'revert-buffer))
          
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

;;(add-hook 'c-mode-common-hook
;;          (lambda ()
;;            (c-set-style "linux")))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))

(setq column-number-mode t
      inhibit-startup-screen t
      safe-local-variable-values '((eval highlight-regexp "	"))
      custom-safe-themes t
      vc-follow-symlinks nil
      org-src-fontify-natively t
      c-default-style '((java-mode . "java")
                        (c++-mode . "linux")
                        (other . "gnu"))
      ;; by default python removes '' from the sys.path when starting
      ;; which prevents us from loading stuff from the current directory
      python-remove-cwd-from-path nil)

(autoload 'hg-status "mercurial" "Entry point into hg-status mode." t)
(autoload 'git-status "git" "Entry point into git-status mode." t)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)
(autoload 'cmake-mode "cmake-mode" "A major mode for CMake" t)
(autoload 'ctags-create-or-update-tags-table "ctags" "Ctags generation and navigation" t)


;; TODO move to a separate configuration file
(autoload 'mu4e "mu4e")

(setq
 mu4e-maildir       "~/Mail/"
 mu4e-sent-folder   "/kylewm/Sent"
 mu4e-drafts-folder "/kylewm/Drafts"
 mu4e-trash-folder  "/kylewm/Trash"
 mu4e-refile-folder "/kylewm/Archive"
 user-mail-address "kyle@kylewm.com"
 smtpmail-default-smtp-server "orin.kylewm.com"
 smtpmail-local-domain "kylewm.com"
 smtpmail-smtp-server "orin.kylewm.com"
 smtpmail-stream-type 'starttls
 smtpmail-smtp-service 587)


(if window-system
    (progn
      (load-theme 'sanityinc-tomorrow-night)
      (set-face-font 'default (if (eq window-system 'w32)
                                  "Consolas-10" "Liberation Mono-10"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-enabled-themes (quote (soft-charcoal)))
 '(fci-rule-color "#383838")
 '(safe-local-variable-values (quote ((c-default-style "linux") (eval highlight-regexp "	"))))
 '(send-mail-function (quote smtpmail-send-it))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
