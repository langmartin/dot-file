;;; -*- lexical-binding: t -*-
;;;; Bootstrapping
(require 'package)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; FIXME https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (remove-from-list 'package-archives "melpa-stable")
;; (remove-from-list 'package-archives "gnu")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-pinned-packages
      '((clj-refactor . "melpa-stable")
        (cider . "melpa-stable")
        (clojure-mode . "melpa-stable")
        (elixir-mode . "melpa-stable")
        ;; (go-mode . "melpa-stable")
        ;; (lsp-mode . "melpa-stable")
        (company . "melpa-stable")
        (magit . "melpa-stable")
        (magit-popup . "melpa-stable")
        (markdown-mode . "melpa-stable")
        (tide . "melpa-stable")))

(package-initialize)

(defun remove-from-list (list-var name)
  (set list-var
       (filter (lambda (x)
                 (not (string= (car x) name)))
               (symbol-value list-var))))

(defun package-require (package)
  (unless (package-installed-p package)
    (package-install package))
  (require package))

(defun maybe-add-to-exec-path (filename)
  (let ((f (expand-file-name filename)))
    (when (file-exists-p f)
      (add-to-list 'exec-path f))))

(when (not (package-installed-p 'dash))
  (package-refresh-contents)
  (package-install 'dash))

(require 'dash)
(require 'subr-x)

(defun package-install-with-archive (archive-pair &rest packages)
  "archive-pair: (\"melpa\" . \"https://melpa.org/packages/\")
packages: 'foo 'bar"
  (let ((pkgs (-remove 'package-installed-p packages)))
    (when (consp pkgs)
      (let ((old package-archives))
        (setq package-archives (list archive-pair))
        (package-refresh-contents)
        (unwind-protect
            (-each pkgs 'package-install)
          (setq package-archives old)
          (package-refresh-contents))))))

(defun package-install-with-melpa (&rest packages)
  (apply 'package-install-with-archive
         '("melpa" . "https://melpa.org/packages/")
         packages))

(defun rc-melpa-packages ()
  (package-install-with-melpa 'flycheck 'flymake-shellcheck)
  (package-install-with-melpa 'elixir-mode))

(defun exec-path-setenv ()
  (interactive)
  (let* ((env (split-string (getenv "PATH") ":"))
         (new (-remove (lambda (x) (member x env)) exec-path)))
    (setenv "PATH" (string-join (append new env) ":"))))

;; reverse order, all add to the beginning
(maybe-add-to-exec-path "/sbin")
(maybe-add-to-exec-path "/usr/sbin")
(maybe-add-to-exec-path "/usr/local/sbin")
(maybe-add-to-exec-path "/opt/homebrew/sbin")
(maybe-add-to-exec-path "/usr/local/bin")
(maybe-add-to-exec-path "/opt/homebrew/bin")
(maybe-add-to-exec-path "/usr/local/go/bin")
(maybe-add-to-exec-path "~/code/contrib/google-cloud-sdk/bin")
(maybe-add-to-exec-path "/usr/local/texlive/2020/bin/x86_64-darwin")
(maybe-add-to-exec-path "/opt/homebrew/texlive/2020/bin/x86_64-darwin")
(maybe-add-to-exec-path "/usr/local/opt/openjdk/bin")
(maybe-add-to-exec-path "~/.gem/ruby/2.6.0/bin")
(maybe-add-to-exec-path "~/.cargo/bin")
(maybe-add-to-exec-path "~/go/bin")
(maybe-add-to-exec-path "~/.asdf/shims")
(maybe-add-to-exec-path "~/langmartin/dot-file/bin")
(maybe-add-to-exec-path "~/bin")
(exec-path-setenv)


;;;; Osx

(defun rc-osx ()
  (fringe-mode '(1 . 1))

  (setenv "EMACS" "/usr/local/bin/emacs")
  (setenv "EDITOR" "/usr/local/bin/emacsclient")
  (setenv "GIT_EDITOR" (getenv "EDITOR"))
  (setenv "PAGER" "head -n100")
  (setenv "GIT_PAGER" "")
  (setenv "MANPAGER" "cat")

  (custom-set-variables
   '(dired-listing-switches "-alh")
   '(ns-alternate-modifier (quote super))
   '(ns-command-modifier (quote meta))
   '(Info-additional-directory-list (quote ("/usr/share/info"))))

  (global-unset-key (kbd "s-h"))        ; ns-do-hide-emacs
  (global-set-key (kbd "M-`") 'switch-to-last-buffer))

(defun rc-font-lg ()
  (interactive)
  (set-frame-font "Monaco-15"))

(defun rc-font-xl ()
  (interactive)
  (set-frame-font "Monaco-24"))

(defun rc-font-sm ()
  (interactive)
  (set-frame-font "Monaco-13"))

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun transparency (&optional opacity)
  (interactive "p")
  (let* ((n (->> (selected-frame) frame-parameters (assoc 'alpha) cdr))
         (m (if (= 1 opacity)
                (if (or (not n) (= 100 n)) 95 100)
              opacity)))
    (set-frame-parameter (selected-frame) 'alpha m)))

(defun rc-anybar ()
  (defun anybar-color (color &optional port)
    (interactive "s")
    (shell-command
     (format "echo -n \"%s\" | nc -4u -w0 localhost %s"
             color (or port 1738))))

  (defun anybar-blue  (&rest _) (interactive) (anybar-color "blue"))
  (defun anybar-cyan  (&rest _) (interactive) (anybar-color "cyan"))
  (defun anybar-green (&rest _) (interactive) (anybar-color "green"))
  (defun anybar       (&rest _) (interactive) (anybar-color "white")))

(defun rc-disable-mouse ()
  (package-require 'disable-mouse)
  (custom-set-variables
   '(disable-mouse-mode-global-lighter ""))
  (global-disable-mouse-mode))


;;;; Modes

(defun rc-ocaml-mode ()
  (package-require 'tuareg)
  (add-hook 'tuareg-mode-hook 'cleanup-untabify-save)
  (add-to-list 'load-path "/Users/lang/.opam/default/share/emacs/site-lisp")
  (add-to-list 'exec-path "/Users/lang/.opam/default/bin")
  (require 'ocp-indent))

(defun rc-r-mode ()
  (package-require 'ess))

(defun rc-shell-mode ()
  (package-require 'flymake-shellcheck)
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(defun rc-clojure-mode ()
  (package-require 'cider)
  (package-require 'flycheck-joker)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'flycheck-mode)

  (define-key clojure-mode-map (kbd "C-x `") 'cider-jump-to-compilation-error)
  (define-key clojure-mode-map (kbd "s-l") 'clojure-insert-lambda)
  (define-key clojure-mode-map (kbd "s-t") 'clojure-insert-trace)
  (define-key clojure-mode-map (kbd "s-c") 'clojure-insert-clear-ns)

  ;; (package-require 'clj-refactor)
  (defun clojure-refactor-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    ;; (cljr-add-keybindings-with-prefix "C-c C-m")
    )
  ;; (add-hook 'clojure-mode-hook 'clojure-refactor-mode-hook)

  (add-hook 'cider-connected-hook 'anybar-blue)
  (advice-add 'cider-jack-in :before #'anybar)

  (custom-set-variables
   '(cider-repl-pop-to-buffer-on-connect nil)
   '(cider-prompt-for-symbol nil)
   '(clojure-defun-indents
     (quote
      (and-let testing match wcar unless unless-let prop/for-all)))))

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

  (defadvice he-substitute-string (after he-paredit-fix)
    "remove extra paren when expanding line in paredit"
    (if (and paredit-mode (member (substring str -1) '(")" "]" "}")))
        (progn (backward-delete-char 1) (forward-char))))

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

(defun cleanup-save ()
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer))

(defun cleanup-untabify-save ()
  (interactive)
  (untabify (point-min) (point-max))
  (cleanup-save))

(defun cleanup-tabify-save ()
  (interactive)
  (tabify (point-min) (point-max))
  (cleanup-save))

(defun turn-off-electric-indent ()
  (electric-indent-mode -1))

(defalias 'cleanup-buffer 'erase-buffer)

(defun etc-hosts ()
  (interactive)
  (find-file "/su:admin@localhost|sudo:root@localhost:/etc/hosts"))

(defun rc-java-mode ()
  (eval-after-load "cc-mode"
    '(progn
       (add-to-list 'java-mode-hook 'set-tab-width-4)
       (add-to-list 'java-mode-hook 'turn-on-tabs)
       (define-key java-mode-map (kbd "C-x C-s") 'cleanup-save)
       (define-key java-mode-map (kbd "C-c C-t") 'git-make-tags))))

(defun rc-javascript-mode ()
  (package-require 'js2-mode)
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

  (define-key js2-mode-map (kbd "s-f") 'javascript-insert-lambda)
  (define-key js2-mode-map (kbd "C-x C-s") 'cleanup-untabify-save)
  (define-key js2-mode-map (kbd "C-x `") 'js2-next-error)
  (add-hook 'js2-mode-hook 'turn-off-tabs)
  (add-hook 'js2-mode-hook 'turn-off-electric-indent)
  (add-hook 'js-mode-hook 'set-tab-width-4)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  (eval-after-load "json-mode"
    '(progn
       (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
       (add-hook 'json-mode-hook 'turn-off-tabs)
       (add-hook 'json-mode-hook 'set-tab-width-2)))

  (package-require 'skewer-mode)
  (skewer-setup))

(defun rc-html-css-mode ()
  (eval-after-load "mhtml-mode"
    '(progn
       (package-require 'emmet-mode)
       (add-to-list 'mhtml-mode-hook 'emmet-mode)))

  (require 'sql)
  (add-to-list 'sql-mode-hook 'turn-off-tabs)

  (eval-after-load "css-mode"
    '(progn
       (define-key css-mode-map (kbd "C-x C-s") 'cleanup-save))))

(defun rc-markdown-mode ()
  (interactive)
  (package-require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(defun rc-prolog ()
  (custom-set-variables
   '(prolog-system (quote swi)))
  (add-to-list 'auto-mode-alist
               '("\\.pl$" . prolog-mode)))

(defun rc-haskell ()
  (eval-after-load "haskell-mode"
    '(progn
       (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
       (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
       (add-hook 'haskell-mode-hook 'haskell-doc-mode)))
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.lhs$" . haskell-mode)))

(defun lsp-enable-on-type-formatting-nil ()
  (make-local-variable 'lsp-enable-on-type-formatting)
  (set 'lsp-enable-on-type-formatting nil))

(defun rc-elixir ()
  (use-package lsp-mode
               :commands lsp
               :ensure t
               :diminish lsp-mode
               :hook (elixir-mode . lsp)
               :init (add-to-list 'exec-path "~/contrib/elixir-ls"))

  (eval-after-load "elixir-mode"
    '(progn
       (add-to-list 'elixir-mode-hook 'yas-minor-mode)
       (add-to-list 'elixir-mode-hook 'lsp-enable-on-type-formatting-nil)
       (define-key elixir-mode-map (kbd "C-x C-s") 'cleanup-untabify-save)
       (define-key elixir-mode-map (kbd "C-x C-s") 'cleanup-untabify-save))))

(defun rc-rust ()
  (use-package lsp-mode
    :commands lsp
    :ensure t
    :diminish lsp-mode
    :hook (rust-mode . lsp)
    :init (add-to-list 'exec-path "~/.cargo/bin/rls")))

(defun rc-c ()
  (use-package lsp-mode
    :commands lsp
    :ensure t
    :diminish lsp-mode
    :hook (c-mode . lsp))
  (custom-set-variables '(lsp-clangd-binary-path "/usr/bin/clangd")))

(defun go-ent ()
  (interactive)
  (set-face-background 'mode-line "goldenrod")
  (set-face-background 'mode-line-inactive "goldenrod4")
  (setq server-socket-dir (format "%s/emacs-ent%d" (or (getenv "TMPDIR") "/tmp") (user-uid)))
  (setenv "EDITOR" (concat "/usr/local/bin/emacsclient -s " server-socket-dir))
  (setenv "GOPATH" "/Users/lang/go-ent")
  (setenv "GOFLAGS" "-tags=consulent -tags=ent")
  (magit-status "/Users/lang/go-ent/src/github.com/hashicorp/nomad"))

(defun go-insert-lambda ()
  (interactive)
  (let ((before "func () {")
        (after  "}()"))
    (insert before)
    (insert after)
    (backward-char (length after))))

(defun go-insert-err ()
  (interactive)
  (insert "if err != nil {\nreturn fmt.Errorf(\"%v\", err)\n}"))

(defun set-fill-column-92 ()
  (interactive)
  (setq fill-column 92))

(defun set-lsp-sym-async ()
  (interactive)
  (setq lsp--document-symbols-request-async t))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook 'lsp-format-buffer t t)
  (add-hook 'before-save-hook 'lsp-organize-imports t t))

(defun goimports ()
  (interactive)
  (save-current-buffer)
  (shell-command (concat "goimports -w " buffer-file-name))
  (revert-buffer t t t))

(defun find-api ()
    (interactive)
    (->> (buffer-file-name)
         (replace-regexp-in-string (regexp-quote "vendor/github.com/hashicorp/nomad/") "")
         (find-file-existing)))

(defun rc-go ()
  (package-install 'yasnippet t)
  (package-install 'lsp-mode t)
  (package-install 'go-mode)
  (package-install 'gotest)

  (eval-after-load "lsp-mode"
    '(progn
       (define-key lsp-mode-map (kbd "C-M-,") 'lsp-find-references)))

  (eval-after-load "go-mode"
    '(progn
       (require 'yasnippet)
       (require 'lsp-mode)
       (define-key go-mode-map (kbd "s-e") 'go-insert-err)
       (define-key go-mode-map (kbd "C-c C-t n") 'go-test-current-test)
       (add-hook 'go-mode-hook 'lsp-go-install-save-hooks)
       (add-hook 'go-mode-hook 'set-fill-column-92)
       (add-hook 'go-mode-hook 'lsp-deferred)
       (add-hook 'go-mode-hook 'set-lsp-sym-async)
       (add-hook 'go-test-mode-hook 'visual-line-mode)

       ;; setup gopls for a virtualbox called "linux" in the ssh config
       (lsp-register-client
        (make-lsp-client :new-connection (lsp-tramp-connection "/home/vagrant/bin/lsp-gopls")
                         :major-modes '(go-mode)
                         :priority 0
                         :server-id 'gopls-linux
                         :remote? t
                         :library-folders-fn
                         (lambda (_workspace)
                           '("/usr/local/go" "/home/vagrant/go/pkg/mod"))))))

  (package-install 'hcl-mode)
  (eval-after-load "hcl-mode"
    '(progn
       (add-to-list 'auto-mode-alist '("\\.tf\\'" . hcl-mode))))

  (custom-set-variables
   '(lsp-prefer-flymake :none)
   '(lsp-file-watch-ignored
     (quote
      ("[/\\\\]\\.git$" "[/\\\\]\\.hg$" "[/\\\\]\\.bzr$" "[/\\\\]_darcs$" "[/\\\\]\\.svn$" "[/\\\\]_FOSSIL_$" "[/\\\\]\\.idea$" "[/\\\\]\\.ensime_cache$" "[/\\\\]\\.eunit$" "[/\\\\]node_modules$" "[/\\\\]\\.fslckout$" "[/\\\\]\\.tox$" "[/\\\\]\\.stack-work$" "[/\\\\]\\.bloop$" "[/\\\\]\\.metals$" "[/\\\\]target$" "[/\\\\]\\.deps$" "[/\\\\]build-aux$" "[/\\\\]autom4te.cache$" "[/\\\\]\\.reference$" "[/\\\\]vendor$")))))

(defun rc-lua ()
  (eval-after-load "lua-mode"
    '(progn
       (define-key lua-mode-map (kbd "M-.") 'imenu))))

(defun rc-typescript ()
  (use-package tide :ensure t)
  (use-package company :ensure t)
  (use-package flycheck :ensure t)
  (use-package web-mode :ensure t)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (custom-set-variables
   '(web-mode-auto-quote-style 3)
   '(web-mode-enable-auto-quoting nil)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setq font-lock-maximum-decoration t)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  )


;;;; Miscellaneous emacs settings

(defun focus-shell (name)
  (interactive)
  (if (file-remote-p default-directory)
      (let ((default-directory "~"))
        (shell name))
    (shell name)))

(defun interactive-partial (f &rest xs)
  (lambda ()
    (interactive)
    (apply f xs)))

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
  (require 'uniquify nil t)
  (package-require 'mode-line-bell)
  (mode-line-bell-mode)
  (package-require 'git-link)

  (set-default 'epa-file-encrypt-to "lang.martin@gmail.com")

  (custom-set-variables
   '(column-number-mode t)
   '(font-lock-maximum-decoration nil)
   '(line-number-mode t)
   '(sentence-end-double-space nil)
   '(indent-tabs-mode nil)
   '(hippie-expand-try-functions-list
     (quote
      (try-expand-all-abbrevs
       try-expand-list
       try-expand-line
       try-expand-dabbrev
       try-expand-dabbrev-all-buffers
       try-expand-dabbrev-from-kill
       try-complete-file-name-partially
       try-complete-file-name
       try-complete-lisp-symbol-partially
       try-complete-lisp-symbol)))
   '(uniquify-buffer-name-style 'forward)
   '(uniquify-strip-common-suffix t))

  (eval-after-load "shell"
    '(progn
       (define-key shell-mode-map (kbd "C-c M-o") 'erase-buffer)
       ;; (define-key shell-mode-map (kbd "M-\r") 'shell-resync-dirs)
       ;; (defun set-shell-dirstack-query-dirs ()
       ;;   (setq shell-dirstack-query "dirs"))
       ;; (add-to-list 'shell-mode-hook 'set-shell-dirstack-query-dirs)
       (setq comint-process-echoes t)
       ;; (add-to-list 'ac-modes 'shell-mode)
       ;; (add-hook 'shell-mode-hook 'ac-rlc-setup-sources)
       ))

  (defun comint-send-tab ()
    "Send a tab to the current buffer's process."
    (interactive)
    (comint-send-input t t)
    (process-send-string nil "  "))

  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "s-SPC") 'just-one-space)
  (global-set-key (kbd "s-s") (interactive-partial 'focus-shell "*shell*"))
  (global-set-key (kbd "s-a") (interactive-partial 'focus-shell "*shell admin*"))
  (global-set-key (kbd "s-d") (interactive-partial 'focus-shell "*shell dee*"))
  (global-set-key (kbd "s-f") (interactive-partial 'focus-shell "*shell eff*"))
  (global-set-key (kbd "s-g") (interactive-partial 'focus-shell "*shell gee*"))
  (global-set-key (kbd "s-v") (interactive-partial 'focus-shell "*shell vagrant*"))
  (global-set-key (kbd "s-r") 'revert-buffer))

(defun yyyymmdd ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun yyyymmdd-pretty ()
  (interactive)
  (insert (format-time-string "%a, %b %d %Y")))

(defun hhmmss ()
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(defun yyyy-mm-ddThh:mm:ss ()
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))

(defun yyyymmddhhmmss ()
  (interactive)
  (insert (format-time-string "%Y%m%d%H%M%S")))

(defun rc-emacs-slides ()
  (defun slides-start ()
    (interactive)
    (set-frame-font "Monaco-26")
    (setq slide-old-mode-line-format mode-line-format)
    (setq mode-line-format nil)
    (eldoc-mode -1)
    (narrow-to-page)
    (set-variable 'cursor-type nil)
    (local-set-key (kbd "<next>") 'narrow-to-next-page)
    (local-set-key (kbd "<prior>") 'narrow-to-prev-page))
  (defun slides-stop ()
    (interactive)
    (setq mode-line-format slide-old-mode-line-format)
    (eldoc-mode 1)
    (set-variable 'cursor-type t)
    (local-unset-key (kbd "<next>"))
    (local-unset-key (kbd "<prior>")))
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

(defun rc-dim-parens ()
  (interactive)
  (defface paren-face
    '((((class color) (background dark))
       (:foreground "grey20"))
      (((class color) (background light))
       (:foreground "grey80")))
    "Face used to dim parentheses.")
  (defun dim-parens-mode ()
    (interactive)
    (font-lock-add-keywords nil '(("(\\|)" . 'paren-face))))
  (add-hook 'emacs-lisp-mode-hook 'dim-parens-mode)
  (add-hook 'clojure-mode-hook 'dim-parens-mode))

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
  ;; (global-set-key (kbd "M-.") 'or-find-tag-imenu)
  (global-set-key (kbd "s-i") 'or-find-tag-imenu)
  (package-require 'find-file-in-repository)
  (global-set-key (kbd "C-x f") 'find-file-in-repository)
  )

(defun rc-recentf ()
  (require 'recentf)
  (recentf-mode)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files)
  )

(defun rc-winner ()
  (require 'winner)
  (define-key winner-mode-map (kbd "C-c <C-right>") 'winner-undo)
  (define-key winner-mode-map (kbd "C-c <C-left>") 'winner-undo)
  (winner-mode 1)
  (global-set-key (kbd "s-<left>") 'windmove-left)
  (global-set-key (kbd "s-<right>") 'windmove-right)
  (global-set-key (kbd "s-<up>") 'windmove-up)
  (global-set-key (kbd "s-<down>") 'windmove-down))

(defun backup-buffer-force ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(defun rc-backups-and-autosave-directory (backup)
  "Set all the variables to move backups & autosave files out of
the working directory"
  (let* ((backup (if (eql "/" (aref backup (- (length backup) 1)))
                     backup
                   (concat backup "/")))
         (autosave (concat backup "auto-save/")))
    (make-directory backup t)
    (make-directory autosave t)
    (setq backup-by-copying t
          delete-old-versions t
          kept-new-versions 10
          kept-old-versions 2
          version-control t
          backup-directory-alist `(("." . ,backup))
          tramp-backup-directory-alist backup-directory-alist
          auto-save-list-file-prefix autosave
          auto-save-file-name-transforms `(("\\`.*/\\([^/]*\\)\\'" ,(concat autosave "\\1") t))
          vc-make-backup-files t
          create-lockfiles nil))
  (add-hook 'before-save-hook 'backup-buffer-force))

(defun rc-look-and-feel ()
  (interactive)
  (rc-font-sm)

  (custom-set-variables
   '(vc-annotate-background nil)
   '(vc-annotate-background-mode nil)
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

(defun set-tab-width-vars (n)
  (setq tab-width n
        c-basic-offset n
        js-indent-level n))

(defun turn-on-tabs () (interactive) (setq indent-tabs-mode t))
(defun turn-off-tabs () (interactive) (setq indent-tabs-mode nil))
(defun set-tab-width-2 () (interactive) (set-tab-width-vars 2))
(defun set-tab-width-4 () (interactive) (set-tab-width-vars 4))
(defun set-tab-width-8 () (interactive) (set-tab-width-vars 8))
(defun turn-on-auto-fill () (interactive) (auto-fill-mode 1))
(defun turn-off-auto-fill () (interactive) (auto-fill-mode -1))

(defun rc-ggtags ()
  "https://github.com/leoliu/ggtags"
  (package-require 'ggtags)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

(defun rc-vi-mode-minimal ()
  (symbol-function 'switch-to-buffer)
  (defalias 'plain-switch-to-buffer 'switch-to-buffer)

  (defun switch-to-insert-mode-buffer (&rest args)
    (interactive)
    (apply 'plain-switch-to-buffer args)
    (when (eqv "VI" mode-name)
      (vi-goto-insert-state)))

  ;; (defalias 'switch-to-buffer 'switch-to-insert-mode-buffer)
  (global-set-key (kbd "s-z") 'vi-mode))

(defun align-spaces (beg end)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (let ((align-to-tab-stop nil)
        (indent-tabs-mode nil))
    (align-regexp beg end "\\(\\s-*\\)\\S-+" 1 1 t)))


;;;; Git
;;;; Most of the commands in this section are just interactive, so run them with M-x git-...

(defun git-grep (command)
  "Run git grep like grep"
  (interactive
   (list
    (read-from-minibuffer
     "Run git grep (like this): "
     "git grep -n -H -I -e ")))
  (grep command))

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

(defun git-set-merge ()
  (interactive)
  (let ((branch (git-get-current-branch)))
    (shell-command (concat "git config --unset branch." branch ".rebase"))))

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

(defun git-set-hook-pre-push ()
  (interactive)
  (let ((ppwd default-directory))
    (unwind-protect
        (progn
          (cd (git-get-current-root))
          (cd "hooks")
          (when (not (file-exists-p "pre-push"))
            (append-to-file
             "#!/bin/sh
while read local_ref local_sha remote_ref remote_sha
do
    if [ \"$remote_ref\" = \"refs/heads/master\" ]; then
        echo \"pre-push hook: Can not push to remote master branch.\"
        exit 1
    fi
done
exit 0
"
             nil
             "pre-push")
            (set-file-modes "pre-push" #o755)))
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
  (custom-set-variables
   '(magit-default-tracking-name-function (quote magit-default-tracking-name-branch-only))
   '(global-magit-file-mode nil))
  (eval-after-load "magit-files"
    '(progn
       (define-key magit-blob-mode-map (kbd "<return>") 'magit-blob-visit-file))))

(defun magit-blob-visit-file ()
  (interactive)
  (let ((p (point)))
    (find-file magit-buffer-file-name)
    (goto-char p)))

(defun magit-push-dumber (&optional prefix)
  (interactive "P")
  (if (not prefix)
      (magit-run-git-async "push" "-v" "origin")
    (apply 'magit-run-git-async
           (split-string
            (read-from-minibuffer "git " "push -v origin")))))


;;;; Start everything up

(defun rc-init-emacs ()
  (rc-melpa-packages)
  (rc-osx)
  (rc-backups-and-autosave-directory "~/.emacs.d/backup")
  (rc-emacs-miscellany)
  (rc-anybar)
  (rc-disable-mouse)
  (rc-emacs-slides)
  (rc-show-paren-expression)
  (rc-ido)
  (rc-winner)
  (rc-paredit)
  (rc-shell-mode)
  (rc-clojure-mode)
  (rc-java-mode)
  (rc-javascript-mode)
  (rc-html-css-mode)
  (rc-markdown-mode)
  (rc-prolog)
  (rc-haskell)
  (rc-go)
  (rc-lua)
  (rc-typescript)
  (rc-r-mode)
  (rc-elixir)
  (rc-rust)
  (rc-magit)
  (rc-git)
  (rc-c))

(defun rc-init-site-lisp ()
  ;; (require 'rc-clojure)
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
  (global-set-key (kbd "C-x C-/") 'goto-last-change)

  (package-require 'polymode)
  (require 'tla-pcal-mode))

;;;; .emacs looks something like this:
;; (load-file "~/code/dot-file/.emacs")
;; (add-to-list 'load-path "~/code/dot-file/site-lisp")
;; (let ((default-directory  "~/code/dot-file/site-lisp")) (normal-top-level-add-subdirs-to-load-path))
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
;; ;; lsp-mode 6.2.1 for gopls 0.4.1
;; (add-to-list 'load-path "~/code/contrib/lsp-mode")
;; (rc-init-emacs)
;; (rc-look-and-feel)
;; (rc-init-site-lisp)
;; (rc-emacs-master)
