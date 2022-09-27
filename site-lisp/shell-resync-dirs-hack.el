(defun shell-resync-dirs ()
  "Resync the buffer's idea of the current directory stack.
This command queries the shell with the command bound to
`shell-dirstack-query' (default \"dirs\"), reads the next
line output and parses it to form the new directory stack.
DON'T issue this command unless the buffer is at a shell prompt.
Also, note that if some other subprocess decides to do output
immediately after the query, its output will be taken as the
new directory stack -- you lose.  If this happens, just do the
command again."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (process-mark proc))
         (started-at-pmark (= (point) (marker-position pmark))))
    (save-excursion
      (goto-char pmark)
      ;; If the process echoes commands, don't insert a fake command in
      ;; the buffer or it will appear twice.
      (unless comint-process-echoes
        (insert shell-dirstack-query) (insert "\n"))
      (sit-for 0)                       ; force redisplay
      (comint-send-string proc shell-dirstack-query)
      (comint-send-string proc "\n")
      (set-marker pmark (point))
      (let ((pt (point))
            (regexp
             (concat
              (if comint-process-echoes
                  ;; Skip command echo if the process echoes
                  (concat "\\(" (regexp-quote shell-dirstack-query) "\n\\)")
                "\\(\\)")
              "\\(.+\n\\)")))
        ;; This extra newline prevents the user's pending input from spoofing us.
        (insert "\n") (backward-char 1)
        ;; Wait for one line.
        (while (not (looking-at regexp))
          (accept-process-output proc)
          (goto-char pt)))
      (goto-char pmark) (delete-char 1) ; remove the extra newline
      ;; That's the dirlist.  Grab it & parse it.
      (let* ((dls (buffer-substring-no-properties
                   (match-beginning 0) (1- (match-end 0))))
             (dlsl nil)
             (pos 0)
             (ds nil))
        ;; Split the dirlist into whitespace and non-whitespace chunks.
        ;; dlsl will be a reversed list of tokens.
        (while (string-match "\\(\\S-+\\|\\s-+\\)" dls pos)
          (push (match-string 1 dls) dlsl)
          (setq pos (match-end 1)))

        ;; Prepend trailing entries until they form an existing directory,
        ;; whitespace and all.  Discard the next whitespace and repeat.
        (while dlsl
          (let ((newelt "")
                tem1 tem2)
            ;; HACK check both variables so we stop when if the dirs
            ;; -l itself is inadvertently slurped from the shell
            ;; output
            (while (and newelt dlsl)
              ;; We need tem1 because we don't want to prepend
              ;; `comint-file-name-prefix' repeatedly into newelt via tem2.
              (setq tem1 (pop dlsl)
                    tem2 (concat comint-file-name-prefix tem1 newelt))
              (cond ((file-directory-p tem2)
                     (push tem2 ds)
                     (when (string= " " (car dlsl))
                       (pop dlsl))
                     (setq newelt nil))
                    (t
                     (setq newelt (concat tem1 newelt)))))))

        (with-demoted-errors "Couldn't cd: %s"
          (shell-cd (car ds))
          (setq shell-dirstack (cdr ds)
                shell-last-dir (car shell-dirstack))
          (shell-dirstack-message))))
    (if started-at-pmark (goto-char (marker-position pmark)))))
