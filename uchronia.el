;;; uchronia.el --- Uchronia mode -*- lexical-binding: t -*-

;; Copyright (C) 2020-2025 Daniel Mendler

;; Author: Daniel Mendler
;; Maintainer: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/uchronia

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Uchronia mode

;;; Code:

(eval-when-compile (require 'subr-x))

(defgroup uchronia nil
  "Uchronia"
  :group 'convenience
  :prefix "uchronia-")

(defcustom uchronia-filter
  '("^uchronia-"
    execute-extended-command
    exit-minibuffer)
  "Filter commands from the uchronia command history."
  :type '(repeat regexp))

(defvar uchronia-history nil)
(defvar-local uchronia--last-input nil)
(defvar-local uchronia--last-command nil)

;;;###autoload
(define-minor-mode uchronia-mode
  "Uchronia mode."
  :global t
  :lighter " Uchronia"
  (remove-hook 'minibuffer-setup-hook #'uchronia--minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'uchronia--minibuffer-exit)
  (when uchronia-mode
    (add-hook 'minibuffer-setup-hook #'uchronia--minibuffer-setup)
    (add-hook 'minibuffer-exit-hook #'uchronia--minibuffer-exit)))

(defun uchronia--record-input ()
  "Save minibuffer input in `uchronia--lat-input'."
  (setq uchronia--last-input (minibuffer-contents-no-properties)))

(defun uchronia--minibuffer-setup ()
  "Setup minibuffer hook of uchronia."
  (setq uchronia--last-command this-command)
  (add-hook 'post-command-hook #'uchronia--record-input nil 'local))

(defun uchronia--minibuffer-exit ()
  "Exit minibuffer hook of uchronia."
  (let* ((cand (minibuffer-contents-no-properties))
         (cmd uchronia--last-command)
         (input uchronia--last-input)
         (elem (list cmd input cand)))
    ;; Store command history
    (unless (string-match-p (string-join
                             (mapcar (lambda (x)
                                       (if (stringp x)
                                           x
                                         (format "\\`%s\\'" x)))
                                     uchronia-filter)
                             "\\|")
                            (symbol-name cmd))
      (unless (equal elem (car uchronia-history))
        (add-to-history 'uchronia-history elem)))))

(defun uchronia-repeat (elem)
  "Repeat command history ELEM."
  (interactive
   (list (or (car uchronia-history)
             (user-error "Command history is empty"))))
  ;; This should not happen due to uchronia-filter, but just check to be sure.
  (when (eq (car elem) #'uchronia-repeat)
    (user-error "Cannot repeat `uchronia-repeat'"))
  (unless uchronia-mode
    (user-error "Uchronia mode is not enabled"))
  (minibuffer-with-setup-hook
      (lambda ()
        (delete-minibuffer-contents)
        (insert (cadr elem)))
    (command-execute (car elem))))

(defun uchronia-select ()
  "Select from uchronia command history and call `uchronia-repeat'."
  (interactive)
  (unless uchronia-mode
    (user-error "Uchronia mode is not enabled"))
  (let ((cands (or
                (delete-dups
                 (mapcar (lambda (elem)
                           (cons (concat
                                  (propertize (symbol-name (car elem))
                                              'face 'font-lock-function-name-face)
                                  (propertize " " 'display '(space :align-to (+ left 40))) ;; TODO compute aligment
                                  (propertize (string-trim (cadr elem))
                                              'face 'font-lock-string-face)
                                  (propertize " " 'display '(space :align-to (+ left 80))) ;; TODO compute aligment
                                  (propertize (string-trim
                                               (replace-regexp-in-string "[\x100000-\x10FFFD]*" "" (caddr elem))) ;; TODO consult hack
                                               'face 'font-lock-comment-face))
                                 elem))
                         uchronia-history))
                (user-error "Command history is empty"))))
    (uchronia-repeat (cdr (assoc (completing-read "History: "
                                                  (lambda (str pred action)
                                                    (if (eq action 'metadata)
                                                        '(metadata (display-sort-function . identity)
                                                                   (cycle-sort-function . identity))
                                                      (complete-with-action action cands str pred)))
                                                  nil t nil t)
                                 cands)))))

(defvar savehist-minibuffer-history-variables)
(with-eval-after-load 'savehist
  (add-to-list 'savehist-minibuffer-history-variables 'uchronia-history))

(provide 'uchronia)
;;; uchronia.el ends here
