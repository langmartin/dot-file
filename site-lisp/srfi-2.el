;;; srfi-2.el --- and-let* form for Emacs

;; Copyright (C) 2009 Lang Martin

;; Author: Lang Martin <lang.martin@gmail.com>
;; Maintainer: Lang Martin <lang.martin@gmail.com>
;; Created: 17 Jul 2009
;; Version: 1.0
;; URL: http://orangesoda.net/emacs-lisp/srfi-2.el

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; srfi-2.el provides the and-let* form, a sequentially binding form
;; of and. It is a programming tool, not directly intended for use by
;; an end user. See http://srfi.schemers.org/srfi-2/srfi-2.html.

;;; Code:

(defmacro and-let1 (binding body &optional else)
  "Bind one pair, and execute the body if the value of
the binding is true. Use and-let* instead."
  (declare (indent 1))
  (let ((sym (car binding))
        (exp (cdr binding)))
    (if (null exp)
        `(if ,sym ,body ,else)
      `(let ((,sym ,@exp))
         (if ,sym ,body ,else)))))

(defmacro if-let* (bindings body &optional else)
  "See and-let*, but require the body to be a single form. The
  else clause is invoked if any binding fails before reaching the
  body."
  (declare (indent 2))
  (if (null bindings) `,body
    `(and-let1 ,(car bindings)
       (if-let* ,(cdr bindings) ,body ,else)
       ,else)))

(if (fboundp 'cl-assert)
    (cl-assert
     (and
      (eq (if-let* ((foo 1)) 'true 'false)            'true)
      (eq (if-let* ((foo nil)) nil 'false)            'false)
      (eq (if-let* ((foo t)) nil 'false)               nil)
      (eq (if-let* ((foo 1) (bar 2)) (+ foo bar))      3)
      (eq (if-let* ((foo 1) (bar nil)) (+ foo bar) t)  t))
     ))

(defmacro and-let* (bindings &rest body)
  "Bind variables like let*, but ensuring that each value is true
  in sequences. As values are true, continue to bind and then
  execute the body. See scheme srfi-2."
  (declare (indent 1))
  `(if-let* ,bindings (progn ,@body)))

(if (cl-fboundp 'assert)
    (cl-assert
     (and
      (= 3 (and-let* ((foo 1) (bar 2)) (+ foo bar)))
      (null (and-let* ((foo nil) (error "shouldn't reach this line"))))
      (eq 'a (and-let* ((foo (memq 'a '(b c a d)))
                        ((listp foo))
                        (foo (car foo)))
               foo)))))

(provide 'srfi-2)

;;; srfi-2.el ends here
