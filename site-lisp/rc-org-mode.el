(require 'utility)
(require 'srfi-1)
(require 'org)
(require 'org-attach)
(require 'org-collector)

(global-set-keys
 '(("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c ." . org-time-stamp)
   ("C-c C-x C-j" . org-clock-goto))
 'erc-mode-hook)

(defun org-kill-link-as-commit-message ()
  "Capture a stored link. Add it to the kill ring in a format suitable for use as a version control commit message."
  (interactive)
  (call-interactively 'org-store-link)
  (let ((link (car org-stored-links)))
    ;; A link is a (list URL DESCRIPTION)
    (kill-new (concat (cadr link) "\n" (car link) "\n"))
    (setq org-stored-links (cdr org-stored-links))))

(define-key org-mode-map (kbd "C-x C-s") 'cleanup-untabify-save)
(define-key org-mode-map (kbd "C-c M-l") 'org-kill-link-as-commit-message)

(progn
  ;; (add-hook 'org-mode-hook 'comment-char-org)
  (add-hook 'org-mode-hook 'turn-off-tabs)
  (add-hook 'org-mode-hook 'turn-on-auto-fill))

(setq org-file-apps
      (cons '(directory . emacs)
            org-file-apps))

(custom-set-variables
 '(org-enforce-todo-dependencies t)
 '(org-log-done nil)
 '(org-clock-modeline-total (quote current))
 '(org-cycle-include-plain-lists nil)
 '(org-clock-into-drawer "LOGBOOK")
 '(org-duration-format (quote h:mm))
 '(org-adapt-indentation nil)
 '(org-hierarchical-checkbox-statistics nil)
 '(org-hierarchical-todo-statistics nil)
 '(org-cycle-include-plain-lists 'integrate)
 '(org-latex-default-packages-alist
   (quote
    (("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t nil)
     ("" "grffile" t nil)
     ("" "longtable" nil nil)
     ("" "wrapfig" nil nil)
     ("" "rotating" nil nil)
     ("normalem" "ulem" t nil)
     ("" "amsmath" t nil)
     ("" "textcomp" t nil)
     ("" "amssymb" t nil)
     ("" "capt-of" nil nil)
     ("colorlinks=true,pdfstartview=FitV,linkcolor=Blue,citecolor=Blue,urlcolor=Blue,filecolor=Blue" "hyperref" nil nil)
     ("" "parskip" nil nil)))))

;;;; I've added the endnotes package to this header, it doesn't change
;;;; anything by default. In order to use it, you need to add two
;;;; literal latex includes to your org-mode file. At the top, add:
;;;;        #+LaTeX: \let\footnote=\endnote
;;;;
;;;; Then, wherever you want the notes to appear, add:
;;;;        #+LaTeX: \theendnotes

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(defun rc-org-old-fashioned-custom-export ()
  (update-alist
   'org-export-latex-classes
   '("langmartin"
     "\\documentclass[11pt]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{amssymb}
\\usepackage[usenames,dvipsnames]{xcolor}
\\usepackage[colorlinks=true,pdfstartview=FitV,linkcolor=Blue,citecolor=Blue,urlcolor=Blue,filecolor=Blue]{hyperref}
\\usepackage{parskip}
\\setcounter{secnumdepth}{5}
\\usepackage{endnotes}

\\usepackage{titling}
\\pretitle{\\begin{center}\\LARGE}
\\posttitle{\\par\\end{center}\\vskip -2em}
\\preauthor{\\begin{center}
\\large \\lineskip 0em%
\\begin{tabular}[t]{c}}
\\postauthor{\\end{tabular}\\par\\end{center}}
\\predate{\\begin{center}\\large}
\\postdate{\\par\\end{center}}
"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
   'equal))

;; (custom-set-variables
;;  '(org-export-latex-default-class "langmartin"))

(defun org-shift-timestamps (start end n)
  "Update all timestamps in the region n hours"
  (interactive "r\nnAdd hours: ")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "[[<]" end t)
      (when (org-at-timestamp-p t)
        (org-timestamp-change n 'hour)))))

(defun rc-org-latex-code ()
 ;; Include the latex-exporter
  (require 'ox-latex)
  ;; Add minted to the defaults packages to include when exporting.
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  ;; Tell the latex export to use the minted package for source
  ;; code coloration.
  (setq org-latex-listings 'minted)
  ;; Let the exporter use the -shell-escape option to let latex
  ;; execute external programs.
  ;; This obviously and can be dangerous to activate!
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -8bit -output-directory %o %f")))

(provide 'rc-org-mode)
