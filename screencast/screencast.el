(load-file "isend.el")
(load-file "isend-mode.el")

(split-window-right)
(find-file (concat (file-name-directory load-file-name) "demo"))
(sh-mode)

(with-selected-window (next-window)
  (let ((default-directory "~"))
    (term (getenv "SHELL"))))

(global-set-key (kbd "C-x C-c") #'kill-emacs)

;; C-RET doesn't work in my terminal
(define-key isend-mode-map (kbd "C-j") #'isend-send)

(message "")
