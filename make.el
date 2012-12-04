(let ((generated-autoload-file (concat (file-name-directory (buffer-file-name)) "isend.el")))
    (dolist (x '("isend-mode.el"))
      (update-file-autoloads x 'save-after)))
