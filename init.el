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
                      ctags
                      smart-tabs-mode
                      smart-mode-line))

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


(global-set-key (kbd "C-x f")        'ffip)
(global-set-key (kbd "C-c o")        'ff-find-other-file)

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; Shift-Arrow keys to move between windows
(windmove-default-keybindings)

(global-set-key (kbd "C-c C-t")    'toggle-truncate-lines)

;; ctags
(global-set-key (kbd "<f8>") 'ctags-create-or-update-tags-table)
;;(global-set-key (kbd "M-.")  'ctags-search)

;; scroll to the bottom of *compilation* window while compiling
(setq compilation-scroll-output t)

;; F3 highlight symbols like vim's *
(add-hook 'prog-mode-hook
          (lambda () (highlight-symbol-mode)))
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

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))

(setq column-number-mode t
      inhibit-startup-screen t
      safe-local-variable-values '((eval highlight-regexp "	")
                                   (smart-tabs-mode . nil)
                                   (c-default-style . ("linux")))
      custom-safe-themes t
      vc-follow-symlinks nil
      org-src-fontify-natively t
      c-default-style '((java-mode . "java")
                        (c-mode . "linux")
                        (c++-mode . "linux")
                        (other . "gnu"))
      ;; by default python removes '' from the sys.path when starting
      ;; which prevents us from loading stuff from the current directory
      python-remove-cwd-from-path nil)

(add-to-list 'load-path "/usr/share/doc/mercurial-common/examples")

(autoload 'hg-status "mercurial" "Entry point into hg-status mode." t)
(autoload 'git-status "git" "Entry point into git-status mode." t)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)
(autoload 'cmake-mode "cmake-mode" "A major mode for CMake" t)
(autoload 'ctags-create-or-update-tags-table "ctags" "Ctags generation and navigation" t)

(smart-tabs-insinuate 'c 'java 'c++)
(electric-indent-mode)

(require 'auto-complete-config)
(global-auto-complete-mode t)

;; C++ compilation stuff

;; auto-hide on successful compilation
(add-to-list 'compilation-finish-functions
             (lambda (buf str)
               (unless (string-match "exited abnormally" str)
                 (run-at-time "2 sec" nil 'delete-windows-on
                              (get-buffer-create "*compilation*"))
                 (message "No compilation errors!"))))

(defcustom aurora-compile-target-debug "Debug_x64"
  "Debug compile target for aurora-compile")

(defcustom aurora-compile-target-release "RelWithDebInfo_x64"
  "Release compile target for aurora-compile")

(defun aurora-compile (target)
  "Aurora projects have a 'makefiles' directory at their
root. Automatically finds the location of the makefiles directory
and compiles the target defined by aurora-compile-target "
  (interactive)
  (let*
      ((filename (buffer-file-name))
       (root-dir (and filename (locate-dominating-file filename "makefiles")))
       (make-dir (and root-dir (format "%s/makefiles/%s" root-dir (or target aurora-compile-target)))))
    (if make-dir
        (compile (format "make -j8 -C %s" make-dir))
      (message "Could not find 'makefiles' for %s" (buffer-name)))))

(defun aurora-compile-debug ()
  (interactive)
  (aurora-compile aurora-compile-target-debug))

(defun aurora-compile-release ()
  (interactive)
  (aurora-compile aurora-compile-target-release))


(add-hook 'cmake-mode-hook
          (lambda ()
            (local-set-key (kbd "<f6>") 'aurora-compile-debug)
            (local-set-key (kbd "<f7>") 'aurora-compile-release)))

(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key (kbd "<f6>") 'aurora-compile-debug)
            (local-set-key (kbd "<f7>") 'aurora-compile-release)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (semantic-mode t)
            (semantic-idle-summary-mode t)))

(sml/setup)

;; theme and font

(defcustom default-light-color-theme 'solarized-light
  "default light theme")

(defcustom default-dark-color-theme 'solarized-dark
  "default dark theme")

(defun toggle-dark-light-theme ()
  (interactive)

  (let ((is-light (find default-light-color-theme custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme (if is-light default-dark-color-theme default-light-color-theme))))

(if window-system
    (progn
      (global-set-key (kbd "<f11>") 'toggle-dark-light-theme)
      (load-theme 'solarized-light)
      (set-face-font 'default (if (eq window-system 'w32)
                                  "Consolas-10" "Liberation Mono-10"))))
