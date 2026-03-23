;;; Config — parse config.json from USB

(in-package :ac-native.config)

(defstruct config
  (handle "unknown" :type string)
  (piece nil :type (or null string))
  (email nil :type (or null string))
  (claude-token nil :type (or null string))
  (github-pat nil :type (or null string))
  (wifi t :type boolean))

(defun strip-identity-block (text)
  "If TEXT starts with the AC_IDENTITY_BLOCK_V1 marker, skip past it and any
trailing null bytes (the identity block is 32KB zero-padded). Returns the
remaining JSON portion."
  (let ((marker "AC_IDENTITY_BLOCK_V1"))
    (if (and (>= (length text) (length marker))
             (string= marker (subseq text 0 (length marker))))
        ;; Skip marker, then skip newline after marker
        (let ((start (length marker)))
          ;; Skip past the newline following the marker
          (when (and (< start (length text))
                     (char= (char text start) #\Newline))
            (incf start))
          ;; Skip any null bytes (zero-padded identity block)
          (loop while (and (< start (length text))
                           (char= (char text start) #\Nul))
                do (incf start))
          (subseq text start))
        text)))

(defun json-extract-bool (text key)
  "Extract a boolean value for KEY from JSON text. Returns T for true, NIL for false.
Returns :MISSING if the key is not found (so callers can distinguish absent from false)."
  (let* ((needle (format nil "\"~A\"" key))
         (pos (search needle text)))
    (if pos
        (let* ((colon (position #\: text :start (+ pos (length needle)))))
          (when colon
            (let ((rest (string-trim '(#\Space #\Tab #\Newline) (subseq text (1+ colon)))))
              (cond ((and (>= (length rest) 4) (string= "true" (subseq rest 0 4))) t)
                    ((and (>= (length rest) 5) (string= "false" (subseq rest 0 5))) nil)
                    (t nil)))))
        :missing)))

(defun load-config (&optional (path "/mnt/config.json"))
  "Load config from USB. Returns a config struct."
  (if (probe-file path)
      (let* ((raw (with-open-file (s path :direction :input)
                    (let ((buf (make-string (file-length s))))
                      (read-sequence buf s)
                      buf)))
             (text (strip-identity-block raw))
             (wifi-val (json-extract-bool text "wifi")))
        (make-config
         :handle (or (json-extract text "handle") "unknown")
         :piece (json-extract text "piece")
         :email (json-extract text "email")
         :claude-token (json-extract text "claudeToken")
         :github-pat (json-extract text "githubPat")
         :wifi (if (eq wifi-val :missing) t wifi-val)))
      (make-config)))

(defun write-device-tokens (cfg)
  "Write Claude and GitHub tokens from config to /claude-token and /github-pat."
  (when (config-claude-token cfg)
    (with-open-file (s "/claude-token" :direction :output :if-exists :supersede)
      (write-string (config-claude-token cfg) s)))
  (when (config-github-pat cfg)
    (with-open-file (s "/github-pat" :direction :output :if-exists :supersede)
      (write-string (config-github-pat cfg) s))))

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
