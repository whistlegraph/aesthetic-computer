;;; Config — parse config.json from USB

(in-package :ac-native.config)

(defstruct config
  (handle "unknown" :type string)
  (piece nil :type (or null string))
  (email nil :type (or null string)))

(defun load-config (&optional (path "/mnt/config.json"))
  "Load config from USB. Returns a config struct."
  (if (probe-file path)
      (let ((text (with-open-file (s path :direction :input)
                    (let ((buf (make-string (file-length s))))
                      (read-sequence buf s)
                      buf))))
        ;; Minimal JSON parsing — just extract "handle" and "piece"
        (make-config
         :handle (or (json-extract text "handle") "unknown")
         :piece (json-extract text "piece")
         :email (json-extract text "email")))
      (make-config)))

(defun json-extract (text key)
  "Extract a string value for KEY from a JSON text. Very minimal."
  (let* ((needle (format nil "\"~A\"" key))
         (pos (search needle text)))
    (when pos
      (let* ((colon (position #\: text :start (+ pos (length needle))))
             (quote1 (and colon (position #\" text :start (1+ colon))))
             (quote2 (and quote1 (position #\" text :start (1+ quote1)))))
        (when (and quote1 quote2)
          (subseq text (1+ quote1) quote2))))))
