;;; kidlisp-parser.lisp — Tokenizer + reader for KidLisp dialect
;;; Handles auto-wrapping bare lines, comma separation, timing syntax.

(in-package :ac-native.kidlisp)

(defun timing-token-p (s)
  "Is S a timing token like '1s', '2s...', '0.5s!'?"
  (and (stringp s)
       (> (length s) 1)
       (let ((base (string-right-trim ".!" s)))
         (and (> (length base) 0)
              (char= (char base (1- (length base))) #\s)
              (every (lambda (c) (or (digit-char-p c) (char= c #\.)))
                     (subseq base 0 (1- (length base))))))))

(defun parse-number (s)
  "Try to parse S as a number. Returns number or NIL."
  (handler-case
      (let ((n (read-from-string s)))
        (when (numberp n) n))
    (error () nil)))

(defun tokenize (source)
  "Split KidLisp source into tokens (strings)."
  (let ((tokens nil)
        (i 0)
        (len (length source)))
    (flet ((peek () (when (< i len) (char source i)))
           (advance () (prog1 (char source i) (incf i))))
      (loop while (< i len) do
        (let ((c (peek)))
          (cond
            ;; Whitespace
            ((member c '(#\Space #\Tab #\Newline #\Return))
             (advance))
            ;; Comment
            ((char= c #\;)
             (loop while (and (< i len) (not (char= (peek) #\Newline)))
                   do (advance)))
            ;; Parens and comma
            ((char= c #\() (push "(" tokens) (advance))
            ((char= c #\)) (push ")" tokens) (advance))
            ((char= c #\,) (advance)) ; skip commas as separators
            ;; Quoted string
            ((or (char= c #\") (char= c #\'))
             (let ((quote c)
                   (start i))
               (advance) ; skip opening quote
               (loop while (and (< i len) (not (char= (peek) quote)))
                     do (when (char= (peek) #\\) (advance)) ; skip escape
                        (advance))
               (when (< i len) (advance)) ; skip closing quote
               (push (subseq source (1+ start) (1- i)) tokens)))
            ;; Atom (symbol, number, color name, timing token)
            (t
             (let ((start i))
               (loop while (and (< i len)
                                (not (member (peek) '(#\Space #\Tab #\Newline #\Return
                                                      #\( #\) #\, #\;))))
                     do (advance))
               (push (subseq source start i) tokens)))))))
    (nreverse tokens)))

(defun bare-line-p (line)
  "Is LINE a bare expression that needs auto-wrapping in parens?
Bare lines start with a word (not a paren) and have arguments."
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (and (> (length trimmed) 0)
         (not (char= (char trimmed 0) #\())
         (not (char= (char trimmed 0) #\;))
         ;; Has a space (i.e., has arguments)
         (position #\Space trimmed))))

(defun preprocess (source)
  "Pre-process KidLisp source: auto-wrap bare lines, handle commas."
  (let ((lines (uiop:split-string source :separator '(#\Newline))))
    ;; Process each line
    (let ((processed
            (mapcar (lambda (line)
                      (let ((trimmed (string-trim '(#\Space #\Tab #\Return) line)))
                        (cond
                          ;; Empty or comment
                          ((or (= (length trimmed) 0)
                               (char= (char trimmed 0) #\;))
                           "")
                          ;; Already wrapped in parens
                          ((char= (char trimmed 0) #\()
                           trimmed)
                          ;; Bare line: auto-wrap
                          ((bare-line-p trimmed)
                           (format nil "(~A)" trimmed))
                          ;; Single word (color name on line 1, etc.)
                          (t trimmed))))
                    lines)))
      ;; Join all lines, wrap in implicit progn
      (format nil "(progn ~{~A ~})" processed))))

(defun read-tokens (tokens)
  "Read a list of tokens into a nested list AST."
  (let ((pos 0))
    (labels ((read-expr ()
               (when (>= pos (length tokens))
                 (return-from read-expr nil))
               (let ((tok (nth pos tokens)))
                 (cond
                   ((string= tok "(")
                    (incf pos)
                    (let ((items nil))
                      (loop while (and (< pos (length tokens))
                                       (not (string= (nth pos tokens) ")")))
                            do (push (read-expr) items))
                      (when (and (< pos (length tokens))
                                 (string= (nth pos tokens) ")"))
                        (incf pos))
                      (nreverse items)))
                   ((string= tok ")")
                    (incf pos)
                    nil)
                   (t
                    (incf pos)
                    ;; Try to parse as number
                    (or (parse-number tok) tok))))))
      (let ((results nil))
        (loop while (< pos (length tokens))
              do (push (read-expr) results))
        (if (= (length results) 1)
            (first results)
            (cons "progn" (nreverse results)))))))

(defun kidlisp-parse (source)
  "Parse KidLisp source string into an AST (nested lists)."
  (let* ((preprocessed (preprocess source))
         (tokens (tokenize preprocessed)))
    (read-tokens tokens)))
