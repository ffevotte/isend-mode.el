(defmacro isend--test (&rest body)
  `(let ((destination (get-buffer-create "*isend test destination*"))
         (origin      (get-buffer-create "*isend test origin*")))
     (with-current-buffer destination
       (erase-buffer))
     (with-current-buffer origin
       (setq buffer-read-only nil)
       (erase-buffer)
       (insert-file-contents "contents")
       (setq buffer-read-only t)

       (isend-associate destination)
       (goto-char (point-min))
       ,@body
       (with-current-buffer destination
         (buffer-substring (point-min) (point-max))))))

(message "Isend -- running tests...")
(setq isend-skip-empty-lines    nil
      isend-strip-empty-lines   nil
      isend-end-with-empty-line nil)



(message "Sending line by line")
(assert (string=
 (isend--test
  (deactivate-mark)
  (dotimes (i 5)
    (isend-send)))

 "line1
line2
   
line4 -- the line above contained only space
line5
"))



(message "Sending a region")
(assert (string=
 (isend--test
  (set-mark 0)
  (activate-mark)
  (goto-char 250)
  (isend-send))

 "line1
line2
   
line4 -- the line above contained only space
line5
    line6     -- indented
    line7     -- indented
        line8 -- indented more
    line9     -- indented
line10
"))



(message "Sending line by line with `isend-skip-empty-lines'")
(assert (string=
 (isend--test
  (let ((isend-skip-empty-lines t))
    (deactivate-mark)
    (dotimes (i 5)
      (isend-send))))

 "line1
line2
line4 -- the line above contained only space
line5
    line6     -- indented
"))



(message "Sending region with `isend-strip-empty-lines'")
(assert (string=
 (isend--test
  (let ((isend-strip-empty-lines t))
    (set-mark 0)
    (activate-mark)
    (goto-char 250)
    (isend-send)))

 "line1
line2
line4 -- the line above contained only space
line5
    line6     -- indented
    line7     -- indented
        line8 -- indented more
    line9     -- indented
line10
"))



(message "Sending region with `isend-end-with-empty-line'")
(assert (string=
 (isend--test
  (let ((isend-end-with-empty-line t))
    (set-mark 0)
    (activate-mark)
    (goto-char 250)
    (isend-send)))

 "line1
line2
   
line4 -- the line above contained only space
line5
    line6     -- indented
    line7     -- indented
        line8 -- indented more
    line9     -- indented
line10

"))



(message "Sending region with `isend-delete-indentation'")
(assert (string=
 (isend--test
  (let ((isend-delete-indentation t))
    (set-mark 20)
    (activate-mark)
    (goto-char 250)
    (isend-send)))

 "line4 -- the line above contained only space
line5
    line6     -- indented
    line7     -- indented
        line8 -- indented more
    line9     -- indented
line10
"))



(message "Sending region with `isend-delete-indentation'")
(assert (string=
 (isend--test
  (let ((isend-delete-indentation t))
    (set-mark 74)
    (activate-mark)
    (goto-char 157)
    (isend-send)))

 "line6     -- indented
line7     -- indented
    line8 -- indented more
line9     -- indented
"))



(message "Isend -- all tests passed")