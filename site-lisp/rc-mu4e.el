(require 'utility)
(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent)

(defun mu4e-and-update ()
  (interactive)
  (mu4e-update-mail-and-index t))

(global-set-key
 (kbd "C-x m")
 (alist-to-keymap-via-kbd
  '(("c" . mu4e-compose-new)
    ("m" . mu4e-compose-new)
    ("r" . mu4e)
    ("u" . mu4e-and-update))))

;; brew install --with-emacs mu
;; brew install offlineimap
;; brew install html2text

(defadvice mu4e~main-view (after set-directory activate)
  (cd "~"))

(defadvice mu4e~compose-set-friendly-buffer-name (after always-prefix activate)
  (let ((str (concat "mu4e " (buffer-name))))
    (rename-buffer (truncate-string-to-width str mu4e~compose-buffer-max-name-length) t)))


;;;; Toggle viewing with the other html viewer

(defun mu4e-view-with-html2text-on ()
  (interactive)
  (let ((mu4e-html2text-command
         (if mu4e-html2text-command
             nil
           "html2text -width 72 -nobs -utf8")))
    (define-key mu4e-view-mode-map "," 'mu4e-view-with-html2text-off)
    (mu4e-view-refresh)))

(defun mu4e-view-with-html2text-off ()
  (interactive)
  (define-key mu4e-view-mode-map "," 'mu4e-view-with-html2text-on)
  (mu4e-view-refresh))

(define-key mu4e-view-mode-map "," 'mu4e-view-with-html2text-on)


;;;; Flag by moving to a special folder; flags don't sync well to
;;;; exchange, not used currently

(defcustom mu4e-starred-folder "/Starred"
  "Your folder for flagged messages to work around offlineimap's flagged message support"
  :type '(string :tag "Folder name")
  :group 'mu4e-folders)

(defun mu4e-headers-mark-move-to-starred ()
  (interactive)
  (mu4e-mark-set 'move mu4e-starred-folder)
  (mu4e-headers-next))

(defun mu4e-view-mark-move-to-starred ()
  (interactive)
  (mu4e~view-in-headers-context
   (mu4e-headers-mark-move-to-starred)))

(defcustom mu4e-junk-folder "/Junk Email"
  "Your folder for junk mail"
  :type '(string :tag "Folder name")
  :group 'mu4e-folders)

(defun mu4e-headers-mark-move-to-junk ()
  (interactive)
  (mu4e-mark-set 'move mu4e-junk-folder)
  (mu4e-headers-next))

(defun mu4e-view-mark-move-to-junk ()
  (interactive)
  (mu4e~view-in-headers-context
   (mu4e-headers-mark-move-to-junk)))


;;; Gmail like key bindings

(defun rc-mu4e-gmail-shortcuts ()
  (define-key mu4e-main-mode-map "g" 'mu4e-headers-search-bookmark)
  (define-key mu4e-main-mode-map "/" 'mu4e-headers-search)
  (define-key mu4e-headers-mode-map "G" 'mu4e-headers-rerun-search)
  (define-key mu4e-headers-mode-map "g" 'mu4e-headers-search-bookmark)
  (define-key mu4e-view-mode-map "G" 'mu4e-view-go-to-url)
  (define-key mu4e-view-mode-map "g" 'mu4e-headers-search-bookmark)

  (define-key mu4e-headers-mode-map "r" 'mu4e-compose-reply)
  (define-key mu4e-view-mode-map "r" 'mu4e-compose-reply)

  (set 'gmail-y-tags "-\\Inbox,-on-monday,-on-first,-on-occasion")

  (set 'mu4e-marks
       (append
        '((tag
           :char       "l"
           :prompt     "label"
           :ask-target (lambda () (read-string "[+-]label "))
           :action     (lambda (docid msg target)
                         (mu4e-action-retag-message msg target)))
          (flag
           :char        "+"
           :prompt      "starred"
           :show-target (lambda (target) "starred")
           :action      (lambda (docid msg target)
                          (mu4e-action-retag-message msg "+\\Starred")
                          (mu4e~proc-move docid nil "+F+S")))
          (unflag
           :char        "-"
           :prompt      "unstarred"
           :show-target (lambda (target) "unstarred")
           :action      (lambda (docid msg target)
                          (mu4e-action-retag-message msg "-\\Starred")
                          (mu4e~proc-move docid nil "-F+S")))
          (archive
           :char       "y"
           :prompt     "archive"
           :show-target (lambda (target) "archive")
           :action      (lambda (docid msg target)
                          (let* ((tags (mu4e-message-field msg :tags))
                                 (inbox (filter (lambda (x) (string= "\\Inbox" x)) tags))
                                 (starred (filter (lambda (x) (string= "\\Starred" x)) tags)))
                            (if (and inbox starred)
                                (mu4e-action-retag-message msg gmail-y-tags)
                              (progn
                                (mu4e-action-retag-message msg (concat gmail-y-tags ",-\\Starred"))
                                (mu4e~proc-move docid nil "+S-F"))))))
          (trash
           :char        "D"
           :prompt      "trash"
           :show-target (lambda (target) "trash")
           :action      (lambda (docid msg target)
                          (mu4e-action-retag-message msg (concat "+recycle," gmail-y-tags))
                          (mu4e~proc-move docid nil "+T-N"))))
        mu4e-marks))

  ;; unset these, so they can be added fresh
  ;; (set 'mu4e-marks (cdr (cddddr mu4e-marks)))

  (mu4e~headers-defun-mark-for tag)
  (define-key mu4e-headers-mode-map (kbd "l") 'mu4e-headers-mark-for-tag)
  (mu4e~headers-defun-mark-for archive)
  (mu4e~view-defun-mark-for archive)
  (define-key mu4e-headers-mode-map (kbd "y") 'mu4e-headers-mark-for-archive)
  (define-key mu4e-view-mode-map (kbd "y") 'mu4e-view-mark-for-archive)

  (setq
   mu4e-bookmarks
   `(("tag:\\\\Inbox" "Inbox" ?i)
     (,"tag:hero" "Hero" ?h)
     (,"tag:on-monday" "on Monday" ?m)
     (,"tag:on-first" "on The First" ?f)
     (,"tag:on-occasion" "on Occasion" ?o)
     (,"tag:\\\\Starred OR flag:flagged" "Flagged" ?s)
     (,"tag:\\\\Draft OR maildir:\"/[Gmail].Drafts\"" "Drafts" ?d)
     ;; (,(concat "from:" user-mail-address " AND date:30d..now")
     ;;  "Last 30 days sent" 116)
     ("tag:\\\\Sent AND date:30d..now" "Last 30 days sent" ?t)
     ("date:7d..now" "Last 7 days" ?a))))

(defun rc-mu4e-send-longcuts ()
  (define-key mu4e-compose-mode-map (kbd "C-c C-s") nil)
  (define-key mu4e-compose-mode-map (kbd "C-c C-c") nil))

(defun mail-select-account (acct addr)
  ;; 1. add a second account to .offlineimaprc
  ;; 2. add a second account to .msmtprc
  ;; 3. make sure that mail-select-account is true, so that msmtp is
  ;;    flagged to choose the account from the envelope sender
  (setenv "MAILDIR" (concat "/Users/lang/Maildir/" acct))
  (setenv "MU_HOME" (concat "/Users/lang/.cache/mu/" acct))
  (setenv "MU_ADDR" addr)
  (setq mu4e-maildir (concat "/Users/lang/Maildir/" acct))
  (setq mu4e-mu-home (concat "/Users/lang/.cache/mu/" acct))
  (setq user-mail-address addr)
  (setq mu4e-get-mail-command (concat "offlineimap -a " acct))
  (mu4e-quit)
  ;; (mu4e-and-update)
  ;; (mu4e)
  )

(defun mail-hashi ()
  (interactive)
  (mail-select-account "hashi" "lang@hashicorp.com"))

(defun mail-gmail ()
  (interactive)
  (mail-select-account "gmail" "lang.martin@gmail.com"))

(defun mu-reindex ()
  (interactive)
  (shell-command "mu init --muhome $MU_HOME --my-address $MU_ADDR && mu index --muhome $MU_HOME &"))

(eval-after-load "mu4e"
  '(progn
     (rc-mu4e-gmail-shortcuts)

     ;; bcc myself
     ;; (add-hook
     ;;  'mu4e-compose-mode-hook
     ;;  (defun rc-mu4e-bcc-myself ()
     ;;         (save-excursion
     ;;           (message-add-header
     ;;            (concat "Bcc: " user-mail-address "\n")))))

     ;; auto pgp sign
     ;; (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign)
     (rc-mu4e-send-longcuts)))

(custom-set-variables
 '(mu4e-headers-skip-duplicates t)
 '(mu4e-attachment-dir "~/Downloads")
 '(mu4e-compose-signature-auto-include nil)
 '(mu4e-date-format-long "%Y-%m-%d")
 '(mu4e-headers-date-format "%y-%m-%d")
 '(mu4e-drafts-folder "/[Gmail].Drafts")
 ;; '(mu4e-get-mail-command "isync INBOX Archive Drafts 'Deleted Items'")
 '(mu4e-get-mail-command "offlineimap")
 '(mu4e-headers-fields (quote ((:human-date . 12) (:flags . 6) (:from . 22) (:subject))))
 ;; mu4e-shr2text seems to be working, this seems not to be
 ;; '(mu4e-html2text-command "textutil -convert txt -stdin -stdout")
 '(mu4e-view-show-addresses t)
 '(mu4e-headers-include-related t)
 '(mu4e-headers-leave-behavior (quote apply))
 '(mu4e-confirm-quit nil)
 '(mail-specify-envelope-from t))

;; (custom-save-all)

(provide 'rc-mu4e)
