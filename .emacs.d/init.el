(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages '(better-defaults
                      find-file-in-project smex
                      soft-charcoal-theme))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(defvar ffip-project-file '(".hg" ".git"))
(defvar ffip-limit 4096)
(defvar ffip-patterns '("*.java" "*.cpp" "*.h" "*.xml" "*.gradle" "*.txt"))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "<f7>") 'recompile)
(global-set-key (kbd "C-x f") 'ffip)

(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))

(setq column-number-mode t)
(setq inhibit-startup-screen t)

(setq load-path
      (cons (expand-file-name "C:\\Apps\\CMake 2.8\\share\\cmake-2.8\\editors\\emacs")
            (cons (expand-file-name "/usr/share/cmake-2.8/editors/emacs/") load-path)))

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

(setq custom-safe-themes t)

(if window-system
    (progn
      (load-theme 'soft-charcoal)
      (set-face-font 'default (if (eq window-system 'w32)
                                  "Consolas-10" "Monospace-10"))))
