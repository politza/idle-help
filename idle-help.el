;;; idle-help.el --- Watch keys and their bindings while idling.  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Andreas Politz

;; Author: Andreas Politz <politza@hochschule-trier.de>
;; Keywords: help
;; Package-Version: 0.5

;; This program is free software; you can redistribute it and/or modify
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
;;
;; Display infos about keys and what they do while idleing.
;;
;; (idle-help-mode 1)
;; 

;; 

;;; Code:


;; * ================================================================== *
;; * Customizable
;; * ================================================================== *

(defgroup idle-help nil
  "Watch keys and their bindings while idling."
  :group 'help)

(defcustom idle-help-idle-delay 60
  "Number of idle seconds before starting to give hints."
  :group 'idle-help
  :type 'natnum)

(defcustom idle-help-update-rate 12
  "Number of seconds before showing a different key.")

(defcustom idle-help-display-function
  'idle-help-display-help-in-modeline
  "The function used for displaying help.

It should receive on argument.  If it is a string, the function
should display it.  If it is nil, it should undisplay any
previous messages."
  :group 'idle-help
  :type 'function)

(defcustom idle-help-keymap-for-help nil
  "Which keymap to use for giving hints.

If this variable is nil, current local bindings are used; and if
it is the symbol t, all keymaps accessible in the buffer.
Otherwise this ought to be a valid keymap.")


;; * ================================================================== *
;; * Internal variables
;; * ================================================================== *


(defvar idle-help-debug nil
  "Turns on some timer event logging.")

(defvar idle-help-timer nil
  "Timer used for idle-help.")

(defvar idle-help-loop-timer nil
  "Non idle timer for updating the help message.")

(defvar idle-help-keymap-for-help-data nil
  "Internally used preprocessed keymap data.")

(define-minor-mode idle-help-mode
  "Display infos about keybindings, while idleing."
  nil nil nil
  :global t
  (idle-help--logger "Turned %s in %s" (if idle-help-mode "on" "off")
                     (current-buffer))
  (when (timerp idle-help-timer)
    (cancel-timer idle-help-timer))
  (when (timerp idle-help-loop-timer)
    (cancel-timer idle-help-loop-timer))
  (funcall idle-help-display-function nil)

  (when idle-help-mode
    (setq idle-help-timer
          (run-with-idle-timer idle-help-idle-delay t
            'idle-help-loop-handler))))

(defun idle-help-pre-command ()
  (remove-hook 'pre-command-hook 'idle-help-pre-command)
  (idle-help--logger "pre-command: %s" (current-buffer))
  (when (timerp idle-help-loop-timer)
    (cancel-timer idle-help-loop-timer))
  (setq idle-help-loop-timer nil)
  (setq idle-help-keymap-for-help-data nil)
  (funcall idle-help-display-function nil))

(defun idle-help-loop-handler ()
  (idle-help--logger "loop-handler: %s" (window-buffer))
  (when idle-help-mode ;; doesn't hurt
    (add-hook 'pre-command-hook 'idle-help-pre-command)
    (idle-help-fetch-keymap-data)
    (idle-help-do-help)
    (when (timerp idle-help-loop-timer)
      (cancel-timer idle-help-loop-timer))
    (setq idle-help-loop-timer
          (run-with-timer idle-help-update-rate nil
            'idle-help-loop-handler))))

(defun idle-help-fetch-keymap-data ()
  (when (or (null idle-help-keymap-for-help-data)
            (not (eq idle-help-keymap-for-help
                     (car idle-help-keymap-for-help-data))))
    (setq idle-help-keymap-for-help-data
          (cons idle-help-keymap-for-help
                (if (or (eq t idle-help-keymap-for-help)
                        (null idle-help-keymap-for-help))
                    (idle-help--buffer-bindings nil
                     (not idle-help-keymap-for-help))
                  (idle-help--keymap-bindings
                   idle-help-keymap-for-help))))))

(defun idle-help-docstring-first-line (doc)
  (and (stringp doc)
       (substitute-command-keys
        (save-match-data
          (when (string-match "^\\(?:[*]\\|:.*\n\\([ \t]*\n\\)*\\)" doc)
            (setq doc (substring doc (match-end 0))))
          (cond ((string-match "\n" doc)
                 (substring doc 0 (match-beginning 0)))
                (t doc))))))

(defun idle-help-do-help ()
  "Display a help-string for some key."
  (idle-help--logger "do-help: %s" (current-buffer))
  (if (null (cdr idle-help-keymap-for-help-data))
      (funcall idle-help-display-function
               "No keymap.")
    (let* ((data (cdr idle-help-keymap-for-help-data))
           (key-cmd (nth (random (length data)) data))
           (key (car key-cmd))
           (cmd (cdr key-cmd))
           (got-doc t)
           (doc (idle-help-docstring-first-line
                 (or (let ((doc (documentation cmd)))
                       (and (> (length doc) 0)
                            doc))
                     (progn
                       (setq got-doc nil)
                       (concat (symbol-name cmd) "."))))))
      (when (> (length doc) 0)
        (setq doc (concat (string (downcase (aref doc 0)))
                          (substring doc 1))))
      (setq key (propertize
                 key 'face '(:weight bold))
            ;; doc (propertize
            ;;      doc 'face 'font-lock-function-name-face)
            )
      (funcall idle-help-display-function
               (concat "Press " key (if got-doc " to " " for ") doc)))))



;; * ================================================================== *
;; * Modeline display
;; * ================================================================== *

(let ((modeline-tag (make-symbol "idle-help-display-modeline-tag")))
  (set modeline-tag t)
  (defun idle-help-display-help-in-modeline (msg)
    "Display MSG in modeline, while preserving the current format.

If MSG is nil revoke any tinkering with the modeline."
    (let ((buffer (if (minibufferp)
                      (let ((win (idle-help-bottom-left-window)))
                        (and win (window-buffer win)))
                    (current-buffer))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (eq (car-safe (car-safe mode-line-format))
                    modeline-tag)
            (setq mode-line-format (cadr (cadr mode-line-format))))
          (when msg
            (setq mode-line-format
                  (list (list modeline-tag
                              (replace-regexp-in-string "%" "%%" msg))
                        (list nil mode-line-format))))
          (force-mode-line-update))))))

(defun idle-help-bottom-left-window (&optional frame)
  (let ((window (frame-root-window frame)))
    (while (and window
                (not (window-live-p window)))
      (cond
       ((window-top-child window)
        (setq window (window-top-child window))
        (while (and window
                    (window-next-sibling window))
          (setq window (window-next-sibling window))))
       (t
        (setq window (window-left-child window)))))
    window))


;; * ================================================================== *
;; * Keymap Info
;; * ================================================================== *

(defun idle-help--buffer-bindings (&optional buffer local-only-p)
  (let* ((buffer (or buffer (current-buffer)))
         indent-tabs-mode
         bindings
         (key-binding-re
          (regexp-quote
           "key             binding\n---             -------\n"))
         (header-re
          (concat
           "^\n\\(.*\n\\)*?"
           key-binding-re)))
    (with-temp-buffer
      ;;(display-buffer (current-buffer))
      (describe-buffer-bindings buffer)
      (flush-lines "^$" (point-min) (point-max))
      (goto-char 1)
      (save-excursion
        (re-search-forward header-re nil t)
        (delete-region 1 (match-beginning 0))
        (goto-char 1)
        (while (re-search-forward header-re nil t)
          (let ((global (match-string 1)))
            (delete-region (match-beginning 0)
                           (match-end 0))
            (when (equal global "Global Bindings:\n")
              (or local-only-p
                  (re-search-forward header-re nil t))
              (delete-region (point)
                             (point-max))))))
      (while (not (eobp))
        (when (or (re-search-forward
                   (concat "^\\=\\s-*\\(.*\\)\n \\{32\\}\\(\\S-.*\\)")
                   nil t)
                  (re-search-forward
                   (concat "^\\=\\s-*\\(.*?\\) +\\(Prefix Command\\|Keyboard Macro\\|??\\|\\S-+\\)$")
                   nil t))
          (unless (or (and (> (length (match-string 1)) 0)
                           (eq ?< (aref (match-string 1) 0))))
            (let* ((key (match-string 1))
                   (binding (match-string 2))
                   (cmd (ignore-errors
                          (let ((sexp (read-from-string binding)))
                            (and (consp sexp)
                                 (eq (cdr sexp) (length binding))
                                 (commandp (car sexp))
                                 (car sexp))))))
              (when (and cmd (not (memq cmd '(self-insert-command))))
                (when (and nil
                           (> (length key) 3)
                           (equal "ESC " (substring key 0 4)))
                  (setq key (replace-regexp-in-string
                             "\\ESC \\(<\\)?\\(.*\\)"
                             (lambda (_)
                               (concat
                                (if (match-string 1 key)
                                    "<")
                                "M-" (match-string 2 key)))
                             key)))
                (or (and nil
                         (string-match "\\`\\(.*\\) .. \\(.*\\)\\'" key)
                         (condition-case nil
                             (let* ((k1 (read-kbd-macro (match-string 1) t))
                                    (k2 (read-kbd-macro (match-string 2) t)))
                               (dotimes (i (1+ (- (aref k2 0) (aref k1 0))))
                                 (push (cons (key-description
                                              (vector (+ i (aref k1 0))))
                                             cmd)
                                       bindings))
                               t)
                           (error nil)))
                    (push (cons key cmd) bindings))))))
        (forward-line)))
    bindings))

(defun idle-help--keymap-bindings (map)
  (with-temp-buffer
    (use-local-map map)
    (let (minor-mode-map-alist
          minor-mode-overriding-map-alist
          overriding-local-map
          overriding-terminal-local-map
          emulation-mode-map-alists)
      (idle-help--buffer-bindings nil t))))

(defun idle-help--logger (fmt &rest args)
  (when idle-help-debug
    (with-current-buffer (get-buffer-create "*idle-help-log*")
      (goto-char (point-max))
      (princ
       (concat
        (format-time-string "%T:")
        (if args
            (apply 'format fmt args)
          (format "%s" fmt)))
       (current-buffer))
      (newline)
      (let ((win (or (get-buffer-window nil t)
                     (display-buffer (current-buffer)))))
        (set-window-point win (point-max))))
    nil))


(provide 'idle-help)

;;; idle-help.el ends here
