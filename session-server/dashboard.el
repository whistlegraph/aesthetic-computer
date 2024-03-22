(defun draw-x-in-new-buffer ()
  "Draws a visual X in a new buffer continuously."
  (interactive)
  (let* ((buffer (generate-new-buffer "X-Buffer"))
         (size 5) ; Size of the X
         (char "X") ; Character to use
         (delay 1) ; Delay in seconds
         (draw-x (lambda ()
                   (with-current-buffer buffer
                     (read-only-mode -1)
                     (erase-buffer)
                     ;; Drawing the top half of the X
                     (dotimes (i size)
                       (insert (make-string i ?\s) char
                               (make-string (- size i 1) ?\s) char "\n"))
                     ;; Drawing the bottom half of the X
                     (dotimes (i size)
                       (insert (make-string (- size i 1) ?\s) char
                               (make-string i ?\s) char "\n"))
                     (read-only-mode 1)))))
    (switch-to-buffer buffer)
    (setq buffer-read-only t)
    (run-with-timer 0 delay draw-x)))
