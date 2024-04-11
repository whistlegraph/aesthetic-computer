;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun update-pixel-buffer (frame buffer-name)
  "Update the pixel buffer for a given FRAME number in BUFFER-NAME."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (with-current-buffer buffer
       (setq line-spacing -8)  ; Adjust this value as needed
       (erase-buffer)
        (dotimes (y 32)
          (dotimes (x 32)
            (insert (propertize " " 'face `(:background ,(if (cl-evenp (+ x y frame)) "green" "blue")))))
          (insert "\n"))))))

(defun start-pixel-animation ()
  "Start the pixel buffer animation."
  (interactive)
  (let ((frame 0)
        (buffer-name "*Pixel Buffer*"))
    (unless (get-buffer buffer-name)
      (get-buffer-create buffer-name)
      (switch-to-buffer buffer-name))
    (run-with-timer 0 0.1 (lambda ()
                               (update-pixel-buffer frame buffer-name)
                               (setq frame (1+ frame))))))

(start-pixel-animation)
