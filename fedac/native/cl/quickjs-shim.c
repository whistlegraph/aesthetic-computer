/* quickjs-shim.c — Thin C shim for calling QuickJS from Common Lisp via CFFI.
 *
 * QuickJS uses NaN-boxed JSValue (64-bit union) which is awkward to pass
 * through CFFI. This shim provides pointer-safe wrappers that CL can call.
 *
 * Compile: gcc -shared -fPIC -o libquickjs-shim.so quickjs-shim.c \
 *          -I/path/to/quickjs -L/path/to -lquickjs -lm
 */

#include "quickjs.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ── Eval helpers ── */

int qjs_eval(JSContext *ctx, const char *code, int code_len, const char *filename, int flags) {
    JSValue val = JS_Eval(ctx, code, code_len, filename, flags);
    if (JS_IsException(val)) {
        JSValue exc = JS_GetException(ctx);
        const char *str = JS_ToCString(ctx, exc);
        if (str) { fprintf(stderr, "[qjs] eval error: %s\n", str); JS_FreeCString(ctx, str); }
        /* Print stack trace if available */
        if (JS_IsObject(exc)) {
            JSValue stack = JS_GetPropertyStr(ctx, exc, "stack");
            const char *ss = JS_ToCString(ctx, stack);
            if (ss) { fprintf(stderr, "%s\n", ss); JS_FreeCString(ctx, ss); }
            JS_FreeValue(ctx, stack);
        }
        JS_FreeValue(ctx, exc);
        JS_FreeValue(ctx, val);
        return -1;
    }
    JS_FreeValue(ctx, val);
    return 0;
}

int qjs_eval_module(JSContext *ctx, const char *code, int code_len, const char *filename) {
    return qjs_eval(ctx, code, code_len, filename, JS_EVAL_TYPE_MODULE);
}

/* ── Global property access ── */

const char *qjs_get_global_string(JSContext *ctx, const char *name) {
    JSValue global = JS_GetGlobalObject(ctx);
    JSValue val = JS_GetPropertyStr(ctx, global, name);
    JS_FreeValue(ctx, global);
    const char *str = JS_ToCString(ctx, val);
    JS_FreeValue(ctx, val);
    return str; /* caller must JS_FreeCString */
}

void qjs_set_global_int(JSContext *ctx, const char *name, int value) {
    JSValue global = JS_GetGlobalObject(ctx);
    JS_SetPropertyStr(ctx, global, name, JS_NewInt32(ctx, value));
    JS_FreeValue(ctx, global);
}

void qjs_set_global_float(JSContext *ctx, const char *name, double value) {
    JSValue global = JS_GetGlobalObject(ctx);
    JS_SetPropertyStr(ctx, global, name, JS_NewFloat64(ctx, value));
    JS_FreeValue(ctx, global);
}

void qjs_set_global_string(JSContext *ctx, const char *name, const char *value) {
    JSValue global = JS_GetGlobalObject(ctx);
    JS_SetPropertyStr(ctx, global, name, JS_NewString(ctx, value));
    JS_FreeValue(ctx, global);
}

/* ── Callback registration ──
 * CL registers a C function pointer. The shim wraps it into a JSCFunction
 * that extracts args and calls through.
 *
 * For simplicity, we use a table of up to 256 registered callbacks.
 */

typedef JSValue (*NativeFn)(JSContext *ctx, JSValueConst this_val,
                            int argc, JSValueConst *argv);

/* Register a native C function as a global JS function */
void qjs_register_func(JSContext *ctx, const char *name, NativeFn func, int argc) {
    JSValue global = JS_GetGlobalObject(ctx);
    JS_SetPropertyStr(ctx, global, name,
                      JS_NewCFunction(ctx, func, name, argc));
    JS_FreeValue(ctx, global);
}

/* ── Argument extraction helpers ── */

int qjs_arg_int(JSContext *ctx, JSValueConst *argv, int index) {
    int32_t val = 0;
    JS_ToInt32(ctx, &val, argv[index]);
    return val;
}

double qjs_arg_float(JSContext *ctx, JSValueConst *argv, int index) {
    double val = 0;
    JS_ToFloat64(ctx, &val, argv[index]);
    return val;
}

const char *qjs_arg_string(JSContext *ctx, JSValueConst *argv, int index) {
    return JS_ToCString(ctx, argv[index]); /* caller must JS_FreeCString */
}

/* ── Call global function ── */

int qjs_call_global(JSContext *ctx, const char *name) {
    JSValue global = JS_GetGlobalObject(ctx);
    JSValue func = JS_GetPropertyStr(ctx, global, name);
    if (!JS_IsFunction(ctx, func)) {
        JS_FreeValue(ctx, func);
        JS_FreeValue(ctx, global);
        return -1;
    }
    JSValue ret = JS_Call(ctx, func, global, 0, NULL);
    int err = 0;
    if (JS_IsException(ret)) {
        JSValue exc = JS_GetException(ctx);
        const char *str = JS_ToCString(ctx, exc);
        if (str) { fprintf(stderr, "[qjs] %s() error: %s\n", name, str); JS_FreeCString(ctx, str); }
        JS_FreeValue(ctx, exc);
        err = -1;
    }
    JS_FreeValue(ctx, ret);
    JS_FreeValue(ctx, func);
    JS_FreeValue(ctx, global);
    return err;
}

/* Call a function with the API object as argument.
 * The API object is built by building properties on a fresh object,
 * then passing it as the first arg. This mirrors how the C version works.
 *
 * For the CL bridge, we build the API object in JS (via eval) and
 * call the lifecycle function with it. This function calls a wrapper
 * that's been eval'd: __cl_call_lifecycle(funcName)
 */
int qjs_call_with_api(JSContext *ctx, const char *func_name) {
    char code[256];
    snprintf(code, sizeof(code),
        "if (typeof %s === 'function') { %s(__ac_api); }", func_name, func_name);
    return qjs_eval(ctx, code, strlen(code), "<lifecycle>", JS_EVAL_TYPE_GLOBAL);
}

/* ── Exception handling ── */

int qjs_check_exception(JSContext *ctx) {
    JSValue exc = JS_GetException(ctx);
    if (JS_IsNull(exc) || JS_IsUndefined(exc)) {
        JS_FreeValue(ctx, exc);
        return 0;
    }
    const char *str = JS_ToCString(ctx, exc);
    if (str) { fprintf(stderr, "[qjs] exception: %s\n", str); JS_FreeCString(ctx, str); }
    JS_FreeValue(ctx, exc);
    return 1;
}

/* ── Pending jobs (promises) ── */

int qjs_execute_pending(JSContext *ctx) {
    JSContext *ctx2;
    int n = 0;
    while (JS_ExecutePendingJob(JS_GetRuntime(ctx), &ctx2) > 0)
        n++;
    return n;
}

/* ── Function existence check ── */

int qjs_has_global_func(JSContext *ctx, const char *name) {
    JSValue global = JS_GetGlobalObject(ctx);
    JSValue func = JS_GetPropertyStr(ctx, global, name);
    int is_func = JS_IsFunction(ctx, func);
    JS_FreeValue(ctx, func);
    JS_FreeValue(ctx, global);
    return is_func;
}
