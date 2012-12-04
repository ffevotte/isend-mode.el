;;; isend.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (isend-associate isend-skip-empty-lines isend)
;;;;;;  "isend-mode" "isend-mode.el" (20669 52181))
;;; Generated autoloads from isend-mode.el

(let ((loads (get 'isend 'custom-loads))) (if (member '"isend-mode" loads) nil (put 'isend 'custom-loads (cons '"isend-mode" loads))))

(defvar isend-skip-empty-lines t "\
If non-nil, `isend-send' skips empty lines (i.e. lines containing only spaces)")

(custom-autoload 'isend-skip-empty-lines "isend-mode" t)

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
