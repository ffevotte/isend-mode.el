;;; isend.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (isend-associate isend-default-ipython-setup isend-default-python-setup
;;;;;;  isend-default-shell-setup isend-send-region-function isend-send-line-function
;;;;;;  isend-end-with-empty-line isend-delete-indentation isend-strip-empty-lines
;;;;;;  isend-skip-empty-lines isend) "isend-mode" "isend-mode.el"
;;;;;;  (20737 6670))
;;; Generated autoloads from isend-mode.el

(let ((loads (get 'isend 'custom-loads))) (if (member '"isend-mode" loads) nil (put 'isend 'custom-loads (cons '"isend-mode" loads))))

(defvar isend-skip-empty-lines t "\
If non-nil, `isend-send' skips empty lines (i.e. lines containing only spaces).

Note that this is effective only for sending single lines. To strip whitespace
from sent regions use `isend-strip-empty-lines'.")

(custom-autoload 'isend-skip-empty-lines "isend-mode" t)

(defvar isend-strip-empty-lines nil "\
If non-nil, `isend-send' strips empty lines (i.e. lines containing only spaces).

Note that this works when sending an entire region. If enabled, all lines containing
whitespace only will be stripped from the region before it is sent.")

(custom-autoload 'isend-strip-empty-lines "isend-mode" t)

(defvar isend-delete-indentation nil "\
If non-nil, `isend-send' deletes indentation in regions sent.

Note that this only works when sending a region (as opposed to a
single line). Relative indentation with respect to the first line
in the region is preserved.

This is useful to send e.g. Python blocks.")

(custom-autoload 'isend-delete-indentation "isend-mode" t)

(defvar isend-end-with-empty-line nil "\
If non-nil, `isend-send' appends an empty line to everything you send.

This is useful, for example, in working with python code,
in which whitespace terminates definitions.")

(custom-autoload 'isend-end-with-empty-line "isend-mode" t)

(defvar isend-send-line-function 'insert-buffer-substring "\
Function used by `isend-send' to send a single line.

This function takes as argument the name of a buffer containing
the text to be sent.

Possible values include:
- `insert-buffer-substring' (default)
- `isend--ipython-cpaste'
- `isend--ipython-paste'")

(custom-autoload 'isend-send-line-function "isend-mode" t)

(defvar isend-send-region-function 'insert-buffer-substring "\
Function used by `isend-send' to send a region.

This function takes as argument the name of a buffer containing
the text to be sent.

Possible values include:
- `insert-buffer-substring' (default)
- `isend--ipython-cpaste'
- `isend--ipython-paste'")

(custom-autoload 'isend-send-region-function "isend-mode" t)

(autoload 'isend-default-shell-setup "isend-mode" "\
Not documented

\(fn)" nil nil)

(autoload 'isend-default-python-setup "isend-mode" "\
Not documented

\(fn)" nil nil)

(autoload 'isend-default-ipython-setup "isend-mode" "\
Not documented

\(fn)" nil nil)

(autoload 'isend-associate "isend-mode" "\
Set the buffer to which commands will be sent using `isend-send'.
This should usually be something like '*ansi-term*' or '*terminal*'.

\(fn BUFFERNAME)" t nil)

(defalias 'isend 'isend-associate)

;;;***

(provide 'isend)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; isend.el ends here
