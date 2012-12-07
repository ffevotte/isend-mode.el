;;; isend.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (isend-associate isend-end-with-empty-line isend-strip-empty-lines
;;;;;;  isend-skip-empty-lines isend) "isend-mode" "isend-mode.el"
;;;;;;  (20673 40981))
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

(defvar isend-end-with-empty-line nil "\
If non-nil, `isend-send' appends an empty line to everything you send.

This is useful, for example, in working with python code,
in which whitespace terminates definitions.")

(custom-autoload 'isend-end-with-empty-line "isend-mode" t)

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
