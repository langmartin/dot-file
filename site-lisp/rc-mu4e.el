(require 'utility)
(require 'mu4e)
(setq mail-user-agent 'mu4e-user-agent)

(defun mu4e-and-update ()
  (interactive)
  (mu4e)
  (mu4e-update-mail-and-index nil))

(global-set-key
 (kbd "C-x m")
 (alist-to-keymap-via-kbd
  '(("c" . mu4e-compose-new)
    ("m" . mu4e-compose-new)
    ("r" . mu4e)
    ("u" . mu4e-and-update))))

;; brew install --with-emacs mu
;; brew install isync
;; brew install html2text


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

  (add-to-list
   'mu4e-marks
   '(tag
     :char       "l"
     :prompt     "label"
     :ask-target (lambda () (read-string "[+-]label "))
     :action     (lambda (docid msg target)
                   (mu4e-action-retag-message msg target))))

  (add-to-list
   'mu4e-marks
   '(archive
     :char       "y"
     :prompt     "archive"
     :show-target (lambda (target) "archive")
     :action      (lambda (docid msg target)
                    ;; must come before proc-move since retag runs
                    ;; 'sed' on the file
                    (mu4e-action-retag-message msg "-\\Inbox")
                    (mu4e~proc-move docid nil "+S-u-N"))))

  (mu4e~headers-defun-mark-for tag)
  (mu4e~headers-defun-mark-for archive)
  (define-key mu4e-headers-mode-map (kbd "l") 'mu4e-headers-mark-for-tag)
  ;; (define-key mu4e-view-mode-map (kbd "l") 'mu4e-headers-mark-for-tag)
  (define-key mu4e-headers-mode-map (kbd "y") 'mu4e-headers-mark-for-archive)
  ;; (define-key mu4e-view-mode-map (kbd "y") 'mu4e-view-mark-for-archive)

  (setq
   mu4e-bookmarks
   `(("tag:\\\\Inbox" "Inbox" ?i)
     (,"tag:on-monday" "on Monday" ?m)
     (,"tag:on-first" "on The First" ?f)
     (,"tag:on-occasion" "on Occasion" ?o)
     (,"tag:\\\\Starred" "Flagged" ?s)
     (,"tag:\\\\Draft" "Drafts" ?d)
     ;; (,(concat "from:" user-mail-address " AND date:30d..now")
     ;;  "Last 30 days sent" 116)
     ("tag:\\\\Sent AND date:30d..now" "Last 30 days sent" ?t)
     ("date:7d..now" "Last 7 days" ?a))))

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

     ;; (setq mu4e-compose-mode-hook nil)

     ;; auto pgp sign
     (add-hook
      'mu4e-compose-mode-hook
      'mml-secure-message-sign)))

(custom-set-variables
 '(mu4e-headers-skip-duplicates t)
 '(mu4e-attachment-dir "~/Downloads")
 '(mu4e-compose-signature-auto-include nil)
 '(mu4e-date-format-long "%Y-%m-%d")
 '(mu4e-headers-date-format "%y-%m-%d")
 ;; '(mu4e-get-mail-command "isync INBOX Archive Drafts 'Deleted Items'")
 '(mu4e-get-mail-command "offlineimap")
 '(mu4e-headers-fields (quote ((:human-date . 12) (:flags . 6) (:from . 22) (:subject))))
 '(mu4e-html2text-command "textutil -convert txt -stdin -stdout")
 '(mu4e-view-show-addresses t)
 '(mu4e-headers-include-related t)
 '(mu4e-confirm-quit nil))

;; (custom-save-all)

(provide 'rc-mu4e)
