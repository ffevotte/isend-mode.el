;;; isend-mode.el --- Interactively send parts of an Emacs buffer to an interpreter

;; Copyright (C) 2012 François Févotte

;; This file is NOT part of Emacs.

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

;; If you make improvements to this code or have suggestions, please do not hesitate to fork the
;; repository or submit bug reports on github. The repository is at:
;;
;;     https://github.com/ffevotte/isend-mode.el

;;; Code:

;;;###autoload
(defgroup isend nil
  "Interactively send parts of an Emacs buffer to an interpreter."
  :group 'processes)

;;;###autoload
(defcustom isend-skip-empty-lines t
  "If non-nil, `isend-send' skips empty lines (i.e. lines containing only spaces).
  Note that this is effective only for sending single lines. To strip whitespace 
  from sent regions use `isend-strip-empty-lines'."
  :group 'isend
  :type  'boolean)

;;;###autoload
(defcustom isend-strip-empty-lines nil
  "If non-nil, `isend-send' strips empty lines (i.e. lines containing only spaces).
  Note that this works when sending an entire region. If enabled, all lines containing
  whitespace only will be stripped from the region before it is sent."
  :group 'isend
  :type  'boolean)

;;;###autoload
(defcustom isend-end-with-empty-line nil
  "If non-nil, `isend-send' appends an empty line to everything you send.
  This is useful, for example, in working with python code,
  in which whitespace terminates definitions."
  :group 'isend
  :type  'boolean)

(define-minor-mode isend-mode
  "Toggle ISend (Interactive Send) mode\\<isend-mode-map>.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When ISend mode is enabled, you can associate the current buffer with
another buffer using `isend-associate' and then send lines to the
associated buffer using \\[isend-send] (`isend-send').

This might be useful to send commands to an interpreter in a terminal
buffer (such as `ansi-term' or `eshell')

\\{isend-mode-map}"
  :init-value nil
  :lighter    " Isend"
  :keymap     '(([C-return] . isend-send)))

(defvar isend-command-buffer nil
  "Buffer to which lines will be sent using `isend-send'.")
(make-variable-buffer-local 'isend-command-buffer)

;;;###autoload
(defun isend-associate (buffername)
 "Set the buffer to which commands will be sent using `isend-send'.
This should usually be something like '*ansi-term*' or '*terminal*'."
 (interactive "bAssociate buffer to terminal: ")
 (setq isend-command-buffer buffername)
 (isend-mode 1))

;;;###autoload
(defalias 'isend 'isend-associate)

(defun isend-send ()
 "Send the current line to a terminal.
Use `send-command-setbuffer' to set the associated terminal
buffer. If the region is active, all lines spanned by it are
sent."
 (interactive)
 (when (not (boundp 'isend-command-buffer))
   (error "No associated terminal buffer. You should run `isend-associate'"))
 (let ((begin (point))
       (end   (point)))
   (cond
    ;; If the region is active, use region boundaries
    ((use-region-p)
     (setq begin (region-beginning)
           end   (- (region-end) 1)))

    ;; If the region is not active and `isend-skip-empty-lines' is non-nil,
    ;; move forward to the first non-empty line
    (isend-skip-empty-lines
     (search-forward-regexp "." nil t)
     (setq begin (point)
           end   (point))))

   ;; Expand the region to span whole lines
   (goto-char begin)
   (setq begin (line-beginning-position))
   (goto-char end)
   (setq end (line-end-position))

   ;; Actually insert the region into the associated buffer
   ;; and send it.
   ;; the regexp strips empty lines from the command to be sent
   (let ((command (if isend-strip-empty-lines
		      (replace-regexp-in-string
		       "\n\\([\s]+\\|\n+\\)?+\n" "\n"
		       (buffer-substring begin end))
		      (buffer-substring begin end))))
     (with-current-buffer isend-command-buffer
       (goto-char (point-max))
       (if isend-end-with-empty-line (insert (concat command "\n"))
					(insert command))
       (cond ((eq major-mode 'term-mode)(term-send-input))
             (t (funcall (key-binding (kbd "RET")))))))

   ;; Move point to the next line
   ;; (skip empty lines if `isend-skip-empty-lines' is non-nil)
   (goto-char (line-end-position))
   (if isend-skip-empty-lines
       (when (search-forward-regexp "." nil t)
         (goto-char (line-beginning-position)))
     (beginning-of-line 2))))

(provide 'isend-mode)

;; isend-mode.el ends here
