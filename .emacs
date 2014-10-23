;;;; Bootstrapping

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defun package-require (package)
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH") ":" (getenv "HOME") "/bin"))
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (expand-file-name "~/bin") t)


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

  (defun rc-slides ()
    (interactive)
    (set-frame-font "Monaco-22")
    (setq mode-line-format nil)
    (eldoc-mode -1))

  (custom-set-variables
   '(dired-listing-switches "-alh")
   '(ns-alternate-modifier (quote hyper))
   '(ns-command-modifier (quote meta))
   '(Info-additional-directory-list (quote ("/usr/share/info"))))

  (defun switch-to-last-buffer ()
    (interactive)
    (switch-to-buffer nil))
  (global-set-key (kbd "M-`") 'switch-to-last-buffer))


;;;; Modes

(defun rc-clojure-mode ()
  (package-require 'cider)

  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  (define-key clojure-mode-map (kbd "C-x `") 'cider-jump-to-compilation-error)
  (define-key clojure-mode-map (kbd "H-l") 'clojure-insert-lambda)
  (define-key clojure-mode-map (kbd "H-t") 'clojure-insert-trace)
  (define-key clojure-mode-map (kbd "H-c") 'clojure-insert-clear-ns)

  (setq cider-repl-pop-to-buffer-on-connect nil
        cider-popup-stacktraces t
        cider-repl-popup-stacktraces t
        cider-repl-result-prefix ";; => "
        cider-auto-select-error-buffer t
        cider-repl-display-in-current-window t)

  (setenv "JVM_OPTS" "-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n"))

(defun clojure-insert-lambda ()
  (interactive)
  (let ((before "(fn []")
        (after  ")"))
    (insert before)
    (insert after)
    (backward-char (length after))))

(defun clojure-insert-trace ()
  (interactive)
  (insert "(use 'clojure.tools.trace)"))

(defun clojure-insert-clear-ns ()
  (interactive)
  (insert "(doseq [[x _] (ns-map *ns*)] (ns-unmap *ns* x))"))

(defun rc-paredit ()
  (package-require 'paredit)

  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
  (define-key paredit-mode-map (kbd "M-]") 'paredit-close-square-and-newline)
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "M-}") 'paredit-close-curly-and-newline)
  (define-key paredit-mode-map (kbd "C-x C-s") 'cleanup-untabify-save)
  (define-key paredit-mode-map (kbd "<return>") 'paredit-newline)

  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'paredit-mode))

(defun cleanup-untabify-save ()
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (save-buffer))

(defun cleanup-tabify-save ()
  (interactive)
  (delete-trailing-whitespace)
  (tabify (point-min) (point-max))
  (save-buffer))

(defun turn-off-electric-indent ()
  (electric-indent-mode -1))

(defun rc-java-mode ()
  (eval-after-load "cc-mode"
    '(progn
       (add-to-list 'java-mode-hook 'set-tab-width-4)
       (define-key java-mode-map (kbd "C-x C-s") 'cleanup-tabify-save)
       (define-key java-mode-map (kbd "C-c C-t") 'git-make-tags))))

(defun rc-javascript-mode ()
  (package-require 'js2-mode)
  (package-require 'skewer-mode)
  (custom-set-variables
   '(js2-bounce-indent-p t)
   '(js2-init-hook (quote (set-tab-width-4))))

  (defun javascript-insert-lambda ()
    (interactive)
    (let ((before "function () {")
          (after  "}"))
      (insert before)
      (insert after)
      (backward-char (length after))))

  (define-key js2-mode-map (kbd "H-l") 'javascript-insert-lambda)
  (define-key js2-mode-map (kbd "C-x C-s") 'cleanup-untabify-save)
  (add-hook 'js2-mode-hook 'turn-off-tabs)
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'js2-mode-hook 'turn-off-electric-indent)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  (add-hook 'js-mode-hook 'set-tab-width-4)
  (add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))

  (eval-after-load "sgml-mode"
    '(progn
       (package-require 'zencoding-mode)
       (add-to-list 'html-mode-hook 'zencoding-mode))))

(defun rc-markdown-mode ()
  (interactive)
  (package-require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))


;;;; Miscellaneous emacs settings

(defun rc-emacs-miscellany ()
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'global-subword-mode) (global-subword-mode 1))
  (fset 'yes-or-no-p 'y-or-n-p)

  (global-set-key (kbd "C-x C-b") 'switch-to-buffer)
  (global-set-key (kbd "C-x x b") 'ibuffer)
  (blink-cursor-mode -1)

  (setq inhibit-trace nil)              ; trace needs this in emacs 24
  (global-auto-revert-mode 1)
  (mouse-avoidance-mode 'jump)
  (require 'uniquify nil t)

  (custom-set-variables
   '(column-number-mode t)
   '(font-lock-maximum-decoration nil)
   '(line-number-mode t)
   '(sentence-end-double-space nil)
   '(hippie-expand-try-functions-list
     '(try-expand-all-abbrevs
       try-expand-dabbrev
       try-expand-dabbrev-all-buffers
       try-expand-dabbrev-from-kill
       try-complete-file-name-partially
       try-complete-file-name
       try-complete-lisp-symbol-partially
       try-complete-lisp-symbol))
   '(uniquify-buffer-name-style 'forward)
   '(uniquify-strip-common-suffix t))

  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "H-SPC") 'just-one-space)
  (global-set-key (kbd "H-i") 'imenu)
  (global-set-key (kbd "H-s") 'shell)
  (global-set-key (kbd "H-r") 'revert-buffer))

(defun yyyymmdd ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun yyyymmdd-pretty ()
  (interactive)
  (insert (format-time-string "%a, %b %d %Y")))

(defun hhmmss ()
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun rc-emacs-slides ()
  (defun narrow-to-next-page ()
    (interactive)
    (goto-char 0)
    (narrow-to-page 1))
  (defun narrow-to-prev-page ()
    (interactive)
    (goto-char 0)
    (narrow-to-page -1))
  (define-key narrow-map (kbd "]") 'narrow-to-next-page)
  (define-key narrow-map (kbd "[") 'narrow-to-prev-page))

(defun rc-emacs-master ()
  (defalias 'quit-emacs 'save-buffers-kill-terminal)
  (global-unset-key (kbd "C-x C-c"))
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-z")))

(defun rc-show-paren-expression ()
  (interactive)
  (show-paren-mode)
  (setq show-paren-style 'expression)
  (set-face-background 'show-paren-match "grey85")
  (set-face-background 'show-paren-mismatch "MediumPurple2"))

(defun rc-show-paren-parens ()
  (interactive)
  (show-paren-mode)
  (setq show-paren-style 'parenthesis)
  (set-face-background 'show-paren-match "grey80")
  (set-face-background 'show-paren-mismatch "purple")
  (set-face-foreground 'show-paren-mismatch "white"))

(defun find-function-at-point-p ()
  (let ((symb (function-called-at-point)))
    (when symb
      (find-function symb)
      t)))

(defun or-find-tag-imenu (&optional use-find-tag)
  "if a tag-file is in use, call find-tag. Fall back to idomenu."
  (interactive "P")
  (if (or use-find-tag tags-file-name)
      (call-interactively 'find-tag)
    (if (equal mode-name "Emacs-Lisp")
        (or (find-function-at-point-p)
            (idomenu))
      (call-interactively 'idomenu))))

(defun rc-ido ()
  (require 'ido)
  ;; (package-require 'ido-better-flex)
  (package-require 'ido-load-library)
  (package-require 'idomenu)
  (custom-set-variables
   '(ido-enable-flex-matching t)
   '(ido-everywhere t)
   '(ido-mode 'both))
  (custom-set-faces
   '(ido-incomplete-regexp ((t (:foreground "grey40"))))
   '(ido-indicator ((t (:foreground "yellow4"))))
   '(ido-subdir ((t (:foreground "blue3")))))
  (global-set-key (kbd "M-.") 'or-find-tag-imenu)
  (global-set-key (kbd "H-i") 'or-find-tag-imenu)
  (package-require 'find-file-in-repository)
  (global-set-key (kbd "C-x f") 'find-file-in-repository))

(defun rc-winner ()
  (require 'winner)
  (define-key winner-mode-map (kbd "C-c <C-right>") 'winner-undo)
  (define-key winner-mode-map (kbd "C-c <C-left>") 'winner-undo)
  (winner-mode 1)
  (global-set-key (kbd "H-<left>") 'windmove-left)
  (global-set-key (kbd "H-<right>") 'windmove-right)
  (global-set-key (kbd "H-<up>") 'windmove-up)
  (global-set-key (kbd "H-<down>") 'windmove-down))

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

(defun rc-look-and-feel ()
  (rc-font-lg)

  (custom-set-variables
   '(vc-annotate-background nil)
   '(vc-annotate-color-map (quote ((20 . "#AA0000") (40 . "#AA3300") (60 . "#AA6600") (80 . "#AA9900") (100 . "#AAAA00") (120 . "#99AA00") (140 . "#66AA00") (160 . "#33AA00") (180 . "#00AA00") (200 . "#00AA33") (220 . "#00AA66") (240 . "#00AA99") (260 . "#00AAAA") (280 . "#0099AA") (300 . "#0066AA") (320 . "#0033AA") (340 . "#0000AA")))))

  (custom-set-faces
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
   '(whitespace-space-after-tab ((t (:background "lightyellow" :foreground "firebrick"))))))

(defun turn-on-tabs () (interactive) (setq indent-tabs-mode t))
(defun turn-off-tabs () (interactive) (setq indent-tabs-mode nil))
(defun set-tab-width-2 () (interactive) (setq tab-width 2) (setq c-basic-offset 2))
(defun set-tab-width-4 () (interactive) (setq tab-width 4) (setq c-basic-offset 4))
(defun set-tab-width-8 () (interactive) (setq tab-width 8) (setq c-basic-offset 8))
(defun turn-on-auto-fill () (interactive) (auto-fill-mode 1))
(defun turn-off-auto-fill () (interactive) (auto-fill-mode -1))

(defun rc-ggtags ()
  "https://github.com/leoliu/ggtags"
  (package-require 'ggtags)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))


;;;; Git
;;;; Most of the commands in this section are just interactive, so run them with M-x git-...

(defun git-grep (command)
  "Run git grep like grep"
  (interactive
   (list
    (read-from-minibuffer
     "Run git grep (like this): "
     "git grep -n -H -I -e ")))
  (grep (concat command " .")))

(defun git-add-edit ()
  "Run git add -e"
  (interactive)
  (async-shell-command "git add -e"))

(defun git-commit ()
  "Run git commit"
  (interactive)
  (async-shell-command "git commit"))

(defun chomp (str)
  (string-match "^\\(.*?\\)[[:space:]\r\n]*$" str)
  (match-string 1 str))

(defun backtick (command)
  (with-temp-buffer
    (shell-command command (current-buffer))
    (chomp (buffer-string))))

(defun git-get-current-branch ()
  (backtick "git rev-parse --abbrev-ref HEAD"))

(defun git-get-current-root ()
  (backtick "git rev-parse --git-dir"))

(defun git-get-current-directory ()
  (backtick "git rev-parse --show-toplevel"))

(defun git-set-rebase ()
  (interactive)
  (let ((branch (git-get-current-branch)))
    (shell-command (concat "git config branch." branch ".rebase true"))))

(defun git-set-default-push ()
  (interactive)
  (shell-command
   (concat "git config remote.origin.push '+refs/heads/*:refs/remotes/"
           (or (getenv "GIT_USER") (getenv "USER"))
           "/*'")))

(defun git-edit-config ()
  (interactive)
  (find-file (concat (git-get-current-root) "/config")))

(defun git-set-hook-pre-commit ()
  (interactive)
  (let ((ppwd default-directory))
    (unwind-protect
        (progn
          (cd (git-get-current-root))
          (cd "hooks")
          (when (not (file-exists-p "pre-commit"))
            (when (file-exists-p "pre-commit.sample")
              (copy-file "pre-commit.sample" "pre-commit"))))
      (cd ppwd))))

(defun git-make-tags ()
  (interactive)
  (with-temp-buffer
    (cd (git-get-current-directory))
    (start-process "make-tags" nil "make" "TAGS")))

(defun rc-git ()
  (package-require 'wgrep)
  (global-set-key (kbd "C-x g g") 'git-grep)
  (global-set-key (kbd "C-x g a") 'git-add-edit)
  (global-set-key (kbd "C-x g e") 'git-add-edit)
  (global-set-key (kbd "C-x g c") 'git-commit)
  (add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-unix-mode))
  (add-to-list 'auto-mode-alist '("\\.git/config$" . conf-unix-mode)))

(defun rc-magit ()
  (package-require 'magit)
  (global-set-key (kbd "C-x g s") 'magit-status)
  (define-key magit-status-mode-map (kbd "P")
    `(keymap (80 . magit-push-dumber))))

(defun magit-push-dumber (&optional prefix)
  (interactive "P")
  (if (not prefix)
      (magit-run-git-async "push" "-v" "origin")
    (apply 'magit-run-git-async
           (split-string
            (read-from-minibuffer "git " "push -v origin")))))


;;;; Start everything up

(defun rc-init-emacs ()
  (rc-osx)
  (rc-backups-and-autosave-directory "~/.emacs.d/backup")
  (rc-emacs-miscellany)
  (rc-emacs-slides)
  (rc-show-paren-expression)
  (rc-ido)
  (rc-winner)
  (rc-paredit)
  (rc-clojure-mode)
  (rc-java-mode)
  (rc-javascript-mode)
  (rc-markdown-mode)
  (rc-magit)
  (rc-git))

(defun rc-init-site-lisp ()
  (require 'rc-clojure)
  (require 'rc-mu4e)
  (require 'rc-erc)
  (require 'rc-org-mode)
  (require 'tiling)

  (require 'uuid)
  (defalias 'uuid 'insert-random-uuid)

  (require 'chop)
  (global-set-keys
   '(("<C-up>" . chop-move-up)
     ("<C-down>" . chop-move-down)))

  (require 'goto-last-change)
  (global-set-key (kbd "C-x C-/") 'goto-last-change))

;;;; .emacs looks something like this:
;; (load-file "~/code/dot-file/.emacs")
;; (add-to-list 'load-path "~/code/dot-file/site-lisp")
;; (rc-init-emacs)
;; (rc-look-and-feel)
;; (rc-emacs-master)
;; (rc-init-site-lisp)
