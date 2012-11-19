;;; isend.el --- Interactively send parts of an Emacs buffer to an interpreter

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
 (interactive "b")
 (setq isend-command-buffer buffername)
 (isend-mode 1))

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
   (when (use-region-p)
     (setq begin (region-beginning)
           end   (- (region-end) 1)))
   (goto-char begin)
   (setq begin (line-beginning-position))
   (goto-char end)
   (setq end (line-end-position))
   (let ((command (buffer-substring begin end)))
     (with-current-buffer isend-command-buffer
       (goto-char (point-max))
       (insert command)
       (cond ((eq major-mode 'term-mode)(term-send-input))
             (t (funcall (key-binding (kbd "RET")))))))
   (goto-char (line-end-position))
   (when (search-forward-regexp "." nil t)
     (goto-char (line-beginning-position)))))

(provide 'isend-mode)

;; isend-mode.el ends here