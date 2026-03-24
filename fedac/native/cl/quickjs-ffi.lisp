;;; QuickJS CFFI bindings for AC Native OS
;;; Embeds QuickJS into the Common Lisp runtime so .mjs pieces can run.

(in-package :ac-native.quickjs)

;;; ── Library loading ──

(cffi:define-foreign-library libquickjs
  (:unix "libquickjs.so")
  (t (:default "libquickjs")))

(cffi:use-foreign-library libquickjs)

;;; ── Constants ──

(defconstant +js-tag-int+       0)
(defconstant +js-tag-bool+      1)
(defconstant +js-tag-null+      2)
(defconstant +js-tag-undefined+  3)
(defconstant +js-tag-float64+   7)
(defconstant +js-tag-string+   -7)
(defconstant +js-tag-object+   -1)

(defconstant +js-eval-type-global+  0)
(defconstant +js-eval-type-module+  1)

;;; ── Opaque types ──

(cffi:defctype js-runtime :pointer)
(cffi:defctype js-context :pointer)
;; JSValue is a 64-bit union on x86-64 (NaN-boxing). We pass as two uint32 words.
;; For simplicity, represent as a 2-element struct or pass via helper C shim.

;;; ── Core runtime functions ──

(cffi:defcfun ("JS_NewRuntime" js-new-runtime) js-runtime)

(cffi:defcfun ("JS_FreeRuntime" js-free-runtime) :void
  (rt js-runtime))

(cffi:defcfun ("JS_NewContext" js-new-context) js-context
  (rt js-runtime))

(cffi:defcfun ("JS_FreeContext" js-free-context) :void
  (ctx js-context))

;;; ── Helper shim functions (defined in quickjs-shim.c) ──
;;; These wrap the NaN-boxed JSValue into pointer-safe calls.

(cffi:defcfun ("qjs_eval" qjs-eval) :int
  (ctx js-context)
  (code :string)
  (code-len :int)
  (filename :string)
  (eval-flags :int))

(cffi:defcfun ("qjs_eval_module" qjs-eval-module) :int
  (ctx js-context)
  (code :string)
  (code-len :int)
  (filename :string))

;; Get global object property as string (returns NULL-terminated string, caller must free)
(cffi:defcfun ("qjs_get_global_string" qjs-get-global-string) :string
  (ctx js-context)
  (name :string))

;; Set global property to integer
(cffi:defcfun ("qjs_set_global_int" qjs-set-global-int) :void
  (ctx js-context)
  (name :string)
  (value :int))

;; Set global property to float
(cffi:defcfun ("qjs_set_global_float" qjs-set-global-float) :void
  (ctx js-context)
  (name :string)
  (value :double))

;; Set global property to string
(cffi:defcfun ("qjs_set_global_string" qjs-set-global-string) :void
  (ctx js-context)
  (name :string)
  (value :string))

;; Register a C callback as a global JS function
;; Signature: int (*)(JSContext*, int argc, void* argv_opaque)
(cffi:defcfun ("qjs_register_func" qjs-register-func) :void
  (ctx js-context)
  (name :string)
  (func :pointer)
  (argc :int))

;; Call a global JS function by name with no args, return 0 on success
(cffi:defcfun ("qjs_call_global" qjs-call-global) :int
  (ctx js-context)
  (name :string))

;; Call a global function with an object argument (the API object)
(cffi:defcfun ("qjs_call_with_api" qjs-call-with-api) :int
  (ctx js-context)
  (func-name :string))

;; Get integer argument from callback
(cffi:defcfun ("qjs_arg_int" qjs-arg-int) :int
  (ctx js-context)
  (argv :pointer)
  (index :int))

;; Get float argument from callback
(cffi:defcfun ("qjs_arg_float" qjs-arg-float) :double
  (ctx js-context)
  (argv :pointer)
  (index :int))

;; Get string argument from callback (caller must free)
(cffi:defcfun ("qjs_arg_string" qjs-arg-string) :string
  (ctx js-context)
  (argv :pointer)
  (index :int))

;; Check for pending exception, print it, return 1 if exception occurred
(cffi:defcfun ("qjs_check_exception" qjs-check-exception) :int
  (ctx js-context))

;; Execute pending jobs (promises, async)
(cffi:defcfun ("qjs_execute_pending" qjs-execute-pending) :int
  (ctx js-context))

;; Check if a global function exists
(cffi:defcfun ("qjs_has_global_func" qjs-has-global-func) :int
  (ctx js-context)
  (name :string))
