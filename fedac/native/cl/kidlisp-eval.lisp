;;; kidlisp-eval.lisp — Tree-walking interpreter for KidLisp

(in-package :ac-native.kidlisp)

(defstruct kidlisp-instance
  (ast nil)
  (global-def (make-hash-table :test #'equal))
  (frame-count 0 :type fixnum)
  (once-executed (make-hash-table :test #'equal))
  (timing-last (make-hash-table :test #'equal))
  (timing-seq (make-hash-table :test #'equal))
  (random-state (make-random-state t))
  ;; Graphics state (set each frame)
  graph
  screen-w
  screen-h
  audio
  (ink-r 255) (ink-g 255) (ink-b 255))

(defun kidlisp-frame (inst)
  "Increment frame counter and evaluate the AST for one frame."
  (incf (kidlisp-instance-frame-count inst))
  (kidlisp-eval inst (kidlisp-instance-ast inst) nil))

;;; ── Variable resolution ──

(defun lookup-var (inst name env)
  "Resolve a variable: env -> global-def -> magic vars -> color."
  (or (cdr (assoc name env :test #'equal))
      (gethash name (kidlisp-instance-global-def inst))
      (magic-var inst name)
      (when (color-name-p name) name)))

(defun magic-var (inst name)
  "Resolve magic variables: width, height, w, h, w/2, h/2, frame."
  (let ((sw (kidlisp-instance-screen-w inst))
        (sh (kidlisp-instance-screen-h inst)))
    (cond
      ((or (string= name "width") (string= name "w")) sw)
      ((or (string= name "height") (string= name "h")) sh)
      ((string= name "w/2") (floor sw 2))
      ((string= name "h/2") (floor sh 2))
      ((string= name "frame") (kidlisp-instance-frame-count inst))
      (t nil))))

;;; ── Timing ──

(defun parse-timing (head)
  "Parse a timing token. Returns (seconds iterating-p bang-p) or NIL."
  (when (timing-token-p head)
    (let* ((has-dots (search "..." head))
           (has-bang (and (not has-dots) (find #\! head)))
           (num-str (string-right-trim ".!s" head)))
      (let ((secs (or (parse-number num-str) 1.0)))
        (values secs (not (null has-dots)) (not (null has-bang)))))))

(defun handle-timing (inst secs iterating-p args env)
  "Handle timed execution. Returns the evaluated body at the right interval."
  (let* ((key (format nil "~A:~A" secs args))
         (now (/ (get-internal-real-time) (float internal-time-units-per-second)))
         (last (gethash key (kidlisp-instance-timing-last inst) 0.0)))
    (when (>= (- now last) secs)
      (setf (gethash key (kidlisp-instance-timing-last inst)) now)
      (if iterating-p
          ;; Iterating: cycle through args
          (let* ((idx (gethash key (kidlisp-instance-timing-seq inst) 0))
                 (val (nth (mod idx (length args)) args)))
            (setf (gethash key (kidlisp-instance-timing-seq inst)) (1+ idx))
            (kidlisp-eval inst val env))
          ;; Single execution
          (let ((result nil))
            (dolist (a args result)
              (setf result (kidlisp-eval inst a env))))))))

;;; ── Core evaluator ──

(defun kidlisp-eval (inst expr env)
  "Evaluate a KidLisp expression."
  (cond
    ;; NIL
    ((null expr) nil)
    ;; Number
    ((numberp expr) expr)
    ;; String atom (variable or color name)
    ((stringp expr)
     (or (lookup-var inst expr env) expr))
    ;; List (function call or special form)
    ((listp expr)
     (let ((head (first expr))
           (args (rest expr)))
       (cond
         ;; Empty list
         ((null head) nil)
         ;; Head is a list (nested expression) — evaluate head first
         ((listp head)
          (let ((result (kidlisp-eval inst head env)))
            (declare (ignore result))
            ;; Evaluate remaining as sequence
            (let ((last nil))
              (dolist (a args last)
                (setf last (kidlisp-eval inst a env))))))
         ;; Timing token
         ((timing-token-p head)
          (multiple-value-bind (secs iterating-p) (parse-timing head)
            (handle-timing inst secs iterating-p args env)))
         ;; Integer as timing shorthand (e.g., (2 body...) = every 2 frames)
         ((and (numberp head) (integerp head) args)
          (when (zerop (mod (kidlisp-instance-frame-count inst) head))
            (let ((result nil))
              (dolist (a args result)
                (setf result (kidlisp-eval inst a env))))))
         ;; String head — function call or special form
         ((stringp head)
          (eval-call inst head args env))
         ;; Fallback
         (t nil))))
    ;; Fallback
    (t expr)))

(defun eval-call (inst head args env)
  "Evaluate a named function call or special form."
  (cond
    ;; ── Special forms (don't pre-evaluate args) ──
    ((string= head "progn")
     (let ((result nil))
       (dolist (a args result)
         (setf result (kidlisp-eval inst a env)))))

    ((string= head "def")
     (when (>= (length args) 2)
       (let ((name (first args))
             (val (kidlisp-eval inst (second args) env)))
         (when (stringp name)
           (setf (gethash name (kidlisp-instance-global-def inst)) val)))))

    ((string= head "later")
     ;; (later name (params...) body...)
     (when (>= (length args) 2)
       (let ((name (first args))
             (params (if (listp (second args)) (second args) nil))
             (body (if (listp (second args)) (cddr args) (cdr args))))
         (when (stringp name)
           (setf (gethash name (kidlisp-instance-global-def inst))
                 (list :later params body))))))

    ((string= head "if")
     (let ((cond-val (kidlisp-eval inst (first args) env)))
       (if (and cond-val (not (eql cond-val 0)))
           (kidlisp-eval inst (second args) env)
           (when (third args)
             (kidlisp-eval inst (third args) env)))))

    ((string= head "not")
     (let ((val (kidlisp-eval inst (first args) env)))
       (if (and val (not (eql val 0))) 0 1)))

    ((string= head "once")
     (let ((key (format nil "once:~A" args)))
       (unless (gethash key (kidlisp-instance-once-executed inst))
         (setf (gethash key (kidlisp-instance-once-executed inst)) t)
         (let ((result nil))
           (dolist (a args result)
             (setf result (kidlisp-eval inst a env)))))))

    ((or (string= head "?") (string= head "choose"))
     ;; Random choice from args
     (when args
       (let ((idx (random (length args) (kidlisp-instance-random-state inst))))
         (kidlisp-eval inst (nth idx args) env))))

    ((string= head "repeat")
     ;; (repeat N [i] body...)
     (let* ((n (kidlisp-eval inst (first args) env))
            (count (if (numberp n) (round n) 0))
            (has-iter (and (>= (length args) 3) (stringp (second args))))
            (iter-name (when has-iter (second args)))
            (body (if has-iter (cddr args) (cdr args))))
       (let ((result nil))
         (dotimes (i count result)
           (let ((new-env (if iter-name
                              (acons iter-name i env)
                              env)))
             (dolist (b body)
               (setf result (kidlisp-eval inst b new-env))))))))

    ;; ── Comparison operators ──
    ((string= head ">")
     (let ((a (kidlisp-eval inst (first args) env))
           (b (kidlisp-eval inst (second args) env)))
       (if (and (numberp a) (numberp b) (> a b)) 1 0)))
    ((string= head "<")
     (let ((a (kidlisp-eval inst (first args) env))
           (b (kidlisp-eval inst (second args) env)))
       (if (and (numberp a) (numberp b) (< a b)) 1 0)))
    ((string= head "=")
     (let ((a (kidlisp-eval inst (first args) env))
           (b (kidlisp-eval inst (second args) env)))
       (if (equal a b) 1 0)))

    ;; ── Built-in functions (evaluate args first) ──
    (t
     (let ((evaled (mapcar (lambda (a) (kidlisp-eval inst a env)) args)))
       (call-builtin inst head evaled)))))

(defun call-builtin (inst name args)
  "Call a built-in KidLisp function with evaluated args."
  (let ((fn (gethash name *builtins*)))
    (if fn
        (funcall fn inst args)
        ;; Check user-defined (later) functions
        (let ((def (gethash name (kidlisp-instance-global-def inst))))
          (cond
            ((and (listp def) (eq (first def) :later))
             (let* ((params (second def))
                    (body (third def))
                    (env (mapcar #'cons
                                 (mapcar (lambda (p) (if (stringp p) p (format nil "~A" p)))
                                         params)
                                 args)))
               (let ((result nil))
                 (dolist (b body result)
                   (setf result (kidlisp-eval inst b env))))))
            ;; If it resolves to a color, return it
            ((color-name-p name)
             (resolve-color (list name) (kidlisp-instance-frame-count inst)))
            ;; Unknown — return nil silently
            (t nil))))))
