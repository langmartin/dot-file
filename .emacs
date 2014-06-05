;;;; Bootstrapping

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defun package-require (package)
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH") ":" (getenv "HOME") "/bin"))


;;;; Osx

(defun rc-osx ()
  (fringe-mode '(1 . 1))

  (setenv "EMACS" "/usr/local/bin/emacs")
  (setenv "EDITOR" "/usr/local/bin/emacsclient")
  (setenv "GIT_EDITOR" (getenv "EDITOR"))
  (setenv "PAGER" "head -n100")
  (setenv "GIT_PAGER" "")
  (setenv "MANPAGER" "cat")

  (defun rc-font-lg ()
    (interactive)
    (set-frame-font "Monaco-14"))

  (defun rc-font-sm ()
    (interactive)
    (set-frame-font "Monaco-12"))

  (custom-set-variables
   '(ns-alternate-modifier (quote hyper))
   '(ns-command-modifier (quote meta))
   '(Info-additional-directory-list (quote ("/usr/share/info"))))

  (global-set-key (kbd "H-SPC") 'just-one-space)

  (defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))
  (global-set-key (kbd "M-`") 'switch-to-last-buffer))


;;;; Clojure

(defun rc-clojure-mode ()
  (package-require 'cider)

  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  (define-key clojure-mode-map (kbd "C-x `") 'cider-jump-to-compilation-error)

  (setq cider-repl-pop-to-buffer-on-connect nil
        cider-popup-stacktraces t
        cider-repl-popup-stacktraces t
        cider-repl-result-prefix ";; => "
        cider-auto-select-error-buffer t
        cider-repl-display-in-current-window t)

  (setenv "JVM_OPTS" "-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n"))

(defun turn-on-paredit-mode () (interactive) (paredit-mode 1))

(defun rc-paredit ()
  (package-require 'paredit-mode)

  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-]") 'paredit-close-square-and-newline)
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "M-}") 'paredit-close-curly-and-newline)

  (add-hook 'lisp-mode-hook 'turn-on-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-paredit-mode)
  (add-hook 'clojure-mode-hook 'turn-on-paredit-mode)
  (add-hook 'scheme-mode-hook 'turn-on-paredit-mode))


;;;; Miscellaneous emacs settings

(defun rc-emacs-miscellany ()
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'global-subword-mode) (global-subword-mode 1))

  (global-set-key (kbd "C-x C-b") 'switch-to-buffer)
  (global-set-key (kbd "C-x x b") 'ibuffer)
  (require 'smooth-scrolling)
  (blink-cursor-mode -1)

  (setq inhibit-trace nil)              ; trace needs this in emacs 24
  (global-auto-revert-mode 1)
  (mouse-avoidance-mode 'jump)

  (add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-unix-mode))
  (add-to-list 'auto-mode-alist '("\\.git/config$" . conf-unix-mode)))

(defun rc-ido ()
  (package-require 'find-file-in-repository)

  (progn
    (require 'ido)
    (custom-set-variables
     '(ido-enable-flex-matching t)
     '(ido-everywhere t)
     '(ido-mode 'both))
    (custom-set-faces
     '(ido-incomplete-regexp ((t (:foreground "grey40"))))
     '(ido-indicator ((t (:foreground "yellow4"))))
     '(ido-subdir ((t (:foreground "blue3")))))
    (global-set-key (kbd "C-x f") 'find-file-in-repository)))

(defun rc-winner ()
  (require 'winner)
  (define-key winner-mode-map (kbd "C-c <C-right>") 'winner-undo)
  (define-key winner-mode-map (kbd "C-c <C-left>") 'winner-undo)
  (winner-mode 1))

(defun backup-buffer-force ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(defun rc-backups-and-autosave-directory (backup)
  "Set all the variables to move backups & autosave files out of
the working directory"
  (let ((backup (if (eql "/" (aref backup (- (length backup) 1)))
                    backup
                  (concat backup "/"))))
   (make-directory backup t)
   (setq backup-by-copying t
         delete-old-versions t
         kept-new-versions 10
         kept-old-versions 2
         version-control t
         backup-directory-alist `(("." . ,backup))
         tramp-backup-directory-alist backup-directory-alist
         auto-save-list-file-prefix (concat backup ".auto-saves-")
         auto-save-file-name-transforms `(("(.*)" ,(concat backup "\\1") t))
         vc-make-backup-files t))

  (add-hook 'before-save-hook 'backup-buffer-force))


;;;; Start everything up

(rc-osx)
(rc-backups-and-autosave-directory "~/.emacs.d/backup")
(rc-emacs-miscellany)
(rc-ido)
(rc-winner)
(rc-clojure-mode)
(rc-paredit)


;;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "brown3"))))
 '(erc-prompt-face ((t (:weight bold))))
 '(eshell-prompt ((((class color) (background light)) (:foreground "Red4" :weight bold))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "grey50"))))
 '(font-lock-warning-face ((((class color) (min-colors 88) (background light)) (:foreground "Red3" :weight bold))))
 '(ido-incomplete-regexp ((t (:foreground "grey40"))))
 '(ido-indicator ((t (:foreground "yellow4"))))
 '(ido-subdir ((t (:foreground "blue3"))))
 '(jabber-chat-prompt-foreign ((t (:foreground "red4"))))
 '(jabber-chat-prompt-local ((t (:foreground "blue4"))))
 '(jabber-chat-prompt-system ((t (:foreground "green4" :weight bold))))
 '(message-cited-text ((t (:foreground "red4"))))
 '(org-mode-line-clock-overrun ((t (:inherit modeline :background "grey" :foreground "red3"))) t)
 '(org-todo ((((class color) (min-colors 16) (background light)) (:foreground "Red4" :weight bold))))
 '(rcirc-server ((t (:foreground "grey"))))
 '(rcirc-timestamp ((t (:inherit default :foreground "grey"))))
 '(rcirc-track-nick ((t (:foreground "purple" :inverse-video nil))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "LemonChiffon"))))
 '(show-paren-match ((t (:background "grey85"))))
 '(show-paren-mismatch ((t (:background "MediumPurple2" :foreground "white"))))
 '(whitespace-line ((t (:background "gray90"))))
 '(whitespace-space-after-tab ((t (:background "lightyellow" :foreground "firebrick")))))
