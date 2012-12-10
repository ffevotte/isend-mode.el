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

;; If you make improvements to this code or have suggestions, please do not
;; hesitate to fork the repository or submit bug reports on github. The
;; repository is at:
;;
;;     https://github.com/ffevotte/isend-mode.el

;;; Code:

;; Get rid of warning about `term-send-input' not being defined.
(require 'term)



;; Customization variables

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
(defcustom isend-delete-indentation nil
  "If non-nil, `isend-send' deletes indentation in regions sent.

Note that this only works when sending a region (as opposed to a
single line). Relative indentation with respect to the first line
in the region is preserved.

This is useful to send e.g. Python blocks.")

;;;###autoload
(defcustom isend-end-with-empty-line nil
  "If non-nil, `isend-send' appends an empty line to everything you send.

This is useful, for example, in working with python code,
in which whitespace terminates definitions."
  :group 'isend
  :type  'boolean)



;; Minor mode definition and activation

(define-minor-mode isend-mode
  "Toggle ISend (Interactive Send) mode\\<isend-mode-map>.
With ARG, turn ISend mode on if ARG is positive, otherwise
turn it off.

This mode allows sending commands from a regular buffer to an
interpreter in a terminal buffer (such as `ansi-term' or
`eshell')

Note that you should NOT manually activate this mode. You should
use `isend-associate' instead.

When ISend mode is enabled and a destination buffer has been
defined using `isend-associate', you can send lines or regions to
the associated buffer associated buffer using \\[isend-send]
(or `isend-send').


\\{isend-mode-map}"
  :init-value nil
  :lighter    " Isend"
  :keymap     '(([C-return] . isend-send)))

(defvar isend--command-buffer)
(make-variable-buffer-local 'isend--command-buffer)

;;;###autoload
(defun isend-associate (buffername)
 "Set the buffer to which commands will be sent using `isend-send'.
This should usually be something like '*ansi-term*' or '*terminal*'."
 (interactive "bAssociate buffer to terminal: ")
 (setq isend--command-buffer buffername)
 (isend-mode 1))

;;;###autoload
(defalias 'isend 'isend-associate)



;; The main workhorse; `isend-send'

(defun isend-send ()
 "Send the current line to a terminal.
Use `isend-associate' to set the associated terminal buffer. If
the region is active, all lines spanned by it are sent."
 (interactive)
 (when (not (boundp 'isend--command-buffer))
   (error "No associated terminal buffer. You should run `isend-associate'"))

 (let* ((region-active (region-active-p))

        ;; The region to be sent
        (bds   (isend--region-boundaries))
        (begin (car bds))
        (end   (cdr bds))

        ;; Buffers involved
        (origin (current-buffer))
        (destination isend--command-buffer)
        filtered)

   ;; A temporary buffer is used to apply filters
   (with-temp-buffer
     (setq filtered (current-buffer))
     (insert-buffer-substring origin begin end)

     ;; Apply filters on the region
     (when (and region-active isend-strip-empty-lines)
       (delete-matching-lines "^[[:space:]]*$" (point-min) (point-max)))

     (when (and region-active isend-delete-indentation)
       (goto-char (point-min))
       (back-to-indentation)
       (indent-rigidly (point-min) (point-max) (- (current-column))))

     (when (and region-active isend-end-with-empty-line)
       (goto-char (point-max))
       (insert "\n"))

     ;; Actually insert the region into the associated buffer
     (with-current-buffer destination
       (goto-char (point-max))
       (insert-buffer-substring filtered)
       (cond
        ;; Terminal buffer: specifically call `term-send-input'
        ;; to handle both the char and line modes of `ansi-term'.
        ((eq major-mode 'term-mode)
         (term-send-input))

        ;; Other buffer: call whatever is bound to 'RET'
        (t
         (funcall (key-binding (kbd "RET"))))))))

 ;; Move point to the next line
 (isend--next-line))



;; Helper functions

(defun isend--region-seed ()
  "Return a 'seed' of the region to be sent.
The result is a cons cell of the form (beg . end)"
  (cond
   ;; If the region is active, use region boundaries
   ((use-region-p)
    (cons (region-beginning)
          (- (region-end) 1)))

   ;; If the region is not active and `isend-skip-empty-lines' is non-nil,
   ;; move forward to the first non-empty line.
   (isend-skip-empty-lines
    (skip-chars-forward "[:space:]\n")
    (cons (point)
          (point)))

   ;; Otherwise, use current point
   (t
    (cons (point)
          (point)))))

(defun isend--region-boundaries ()
  "Return the boundaries of the region to be sent.
The result is a cons cell of the form (beg . end)
The region is expanded so that no line is only partially sent."
  (let* ((bds (isend--region-seed))
         (beg (car bds))
         (end (cdr bds)))

    ;; Expand the region to span whole lines
    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (setq end (line-end-position))
    (when (= beg (point-max))
      (error "Nothing more to send!"))
    (cons beg end)))

(defun isend--next-line ()
  "Move point to the next line.
Empty lines are skipped if `isend-skip-empty-lines' is non-nil."
  (goto-char (line-end-position))
  (if isend-skip-empty-lines
      (when (> (skip-chars-forward "[:space:]\n") 0)
        (goto-char (line-beginning-position)))
    (beginning-of-line 2)))


(provide 'isend-mode)

;; isend-mode.el ends here
