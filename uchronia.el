;;; uchronia.el --- Uchronia mode -*- lexical-binding: t -*-

;; Author: Daniel Mendler
;; Maintainer: Daniel Mendler
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
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

(require 'subr-x)
(require 'savehist)

(defgroup uchronia nil
  "Uchronia"
  :group 'convenience
  :prefix "uchronia-")

(defface uchronia-command
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight commands in `uchronia-select'.")

(defcustom uchronia-input-commands
  '(switch-to-buffer)
  "Commands which should use the input history."
  :type 'list)

(defcustom uchronia-input-histories
  '(buffer-name-history)
  "Histories which should store the input."
  :type 'list)

(defcustom uchronia-filter
  "^uchronia-\\|^execute-extended-command$\\|^exit-minibuffer$"
  "Filter commands from the uchronia command history."
  :type 'regexp)

(defvar uchronia-history nil)
(add-to-list 'savehist-minibuffer-history-variables 'uchronia-history)

(defvar-local uchronia--last-input nil)
(defvar-local uchronia--last-command nil)
(defvar-local uchronia--last-history nil)

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
  (add-hook 'post-command-hook #'uchronia--record-input nil t))

(defun uchronia--minibuffer-exit ()
  "Exit minibuffer hook of uchronia."
  (unless (eq t minibuffer-history-variable)
    (let* ((hist minibuffer-history-variable)
           (cand (minibuffer-contents-no-properties))
           (cmd uchronia--last-command)
           (input uchronia--last-input))
      (run-at-time
       0 nil
       (lambda ()
         (let ((hist-val (symbol-value hist)))
           ;; Try to detect if minibuffer has been aborted.
           ;; TODO Is there a better way?
           ;; TODO Change this such that aborted sessions can also be repeated?
           (when (string= (car hist-val) cand)
             ;; Remove selected candidate from history and store input
             (when (or (memq cmd uchronia-input-commands)
                       (memq hist uchronia-input-histories))
               (set hist (cdr hist-val))
               (add-to-history hist input))

             ;; Store command history
             (unless (string-match-p uchronia-filter (symbol-name cmd))
               (let ((elem (cons cmd input)))
                 (unless (equal elem (car uchronia-history))
                   (add-to-history 'uchronia-history elem)))))))))))

(defun uchronia-repeat (elem)
  "Repeat command history ELEM."
  (interactive
   (list
    (if-let (elem (car uchronia-history))
        elem
      (error "Command history is empty"))))
  ;; This should not happen due to uchronia-filter, but just check to be sure.
  (when (eq (car elem) #'uchronia-repeat)
    (error "Cannot repeat `uchronia-repeat'"))
  (unless uchronia-mode (error "Uchronia mode is not enabled"))
  (minibuffer-with-setup-hook
      (lambda () (insert (cdr elem)))
    (command-execute (car elem))))

(defun uchronia-select ()
  "Select from uchronia command history and call `uchronia-repeat'."
  (interactive)
  (unless uchronia-mode (error "Uchronia mode is not enabled"))
  (let ((cands (or
                (delete-dups
                 (mapcar (lambda (elem)
                           (cons (concat
                                  (propertize (symbol-name (car elem))
                                              'face 'uchronia-command)
                                  " " (cdr elem))
                                 elem))
                         uchronia-history))
                (error "Command history is empty"))))
    (uchronia-repeat (cdr (assoc (completing-read "History: " cands nil t nil t) cands)))))

(provide 'uchronia)
;;; uchronia.el ends here
