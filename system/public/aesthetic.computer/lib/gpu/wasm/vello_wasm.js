let wasm;

let cachedDataViewMemory0 = null;
function getDataViewMemory0() {
    if (cachedDataViewMemory0 === null || cachedDataViewMemory0.buffer.detached === true || (cachedDataViewMemory0.buffer.detached === undefined && cachedDataViewMemory0.buffer !== wasm.memory.buffer)) {
        cachedDataViewMemory0 = new DataView(wasm.memory.buffer);
    }
    return cachedDataViewMemory0;
}

function getStringFromWasm0(ptr, len) {
    ptr = ptr >>> 0;
    return decodeText(ptr, len);
}

let cachedUint8ArrayMemory0 = null;
function getUint8ArrayMemory0() {
    if (cachedUint8ArrayMemory0 === null || cachedUint8ArrayMemory0.byteLength === 0) {
        cachedUint8ArrayMemory0 = new Uint8Array(wasm.memory.buffer);
    }
    return cachedUint8ArrayMemory0;
}

function passStringToWasm0(arg, malloc, realloc) {
    if (realloc === undefined) {
        const buf = cachedTextEncoder.encode(arg);
        const ptr = malloc(buf.length, 1) >>> 0;
        getUint8ArrayMemory0().subarray(ptr, ptr + buf.length).set(buf);
        WASM_VECTOR_LEN = buf.length;
        return ptr;
    }

    let len = arg.length;
    let ptr = malloc(len, 1) >>> 0;

    const mem = getUint8ArrayMemory0();

    let offset = 0;

    for (; offset < len; offset++) {
        const code = arg.charCodeAt(offset);
        if (code > 0x7F) break;
        mem[ptr + offset] = code;
    }
    if (offset !== len) {
        if (offset !== 0) {
            arg = arg.slice(offset);
        }
        ptr = realloc(ptr, len, len = offset + arg.length * 3, 1) >>> 0;
        const view = getUint8ArrayMemory0().subarray(ptr + offset, ptr + len);
        const ret = cachedTextEncoder.encodeInto(arg, view);

        offset += ret.written;
        ptr = realloc(ptr, len, offset, 1) >>> 0;
    }

    WASM_VECTOR_LEN = offset;
    return ptr;
}

let cachedTextDecoder = new TextDecoder('utf-8', { ignoreBOM: true, fatal: true });
cachedTextDecoder.decode();
const MAX_SAFARI_DECODE_BYTES = 2146435072;
let numBytesDecoded = 0;
function decodeText(ptr, len) {
    numBytesDecoded += len;
    if (numBytesDecoded >= MAX_SAFARI_DECODE_BYTES) {
        cachedTextDecoder = new TextDecoder('utf-8', { ignoreBOM: true, fatal: true });
        cachedTextDecoder.decode();
        numBytesDecoded = len;
    }
    return cachedTextDecoder.decode(getUint8ArrayMemory0().subarray(ptr, ptr + len));
}

const cachedTextEncoder = new TextEncoder();

if (!('encodeInto' in cachedTextEncoder)) {
    cachedTextEncoder.encodeInto = function (arg, view) {
        const buf = cachedTextEncoder.encode(arg);
        view.set(buf);
        return {
            read: arg.length,
            written: buf.length
        };
    }
}

let WASM_VECTOR_LEN = 0;

const VelloSceneFinalization = (typeof FinalizationRegistry === 'undefined')
    ? { register: () => {}, unregister: () => {} }
    : new FinalizationRegistry(ptr => wasm.__wbg_velloscene_free(ptr >>> 0, 1));

/**
 * Scene builder for Vello - constructs draw commands
 */
export class VelloScene {
    __destroy_into_raw() {
        const ptr = this.__wbg_ptr;
        this.__wbg_ptr = 0;
        VelloSceneFinalization.unregister(this);
        return ptr;
    }
    free() {
        const ptr = this.__destroy_into_raw();
        wasm.__wbg_velloscene_free(ptr, 0);
    }
    /**
     * Draw a stroked rectangle
     * @param {number} x
     * @param {number} y
     * @param {number} w
     * @param {number} h
     * @param {number} r
     * @param {number} g
     * @param {number} b
     * @param {number} a
     * @param {number} stroke_width
     */
    rect_stroke(x, y, w, h, r, g, b, a, stroke_width) {
        wasm.velloscene_rect_stroke(this.__wbg_ptr, x, y, w, h, r, g, b, a, stroke_width);
    }
    /**
     * Draw a rounded rectangle
     * @param {number} x
     * @param {number} y
     * @param {number} w
     * @param {number} h
     * @param {number} radius
     * @param {number} r
     * @param {number} g
     * @param {number} b
     * @param {number} a
     */
    rounded_rect(x, y, w, h, radius, r, g, b, a) {
        wasm.velloscene_rounded_rect(this.__wbg_ptr, x, y, w, h, radius, r, g, b, a);
    }
    /**
     * Draw a stroked circle
     * @param {number} cx
     * @param {number} cy
     * @param {number} radius
     * @param {number} r
     * @param {number} g
     * @param {number} b
     * @param {number} a
     * @param {number} stroke_width
     */
    circle_stroke(cx, cy, radius, r, g, b, a, stroke_width) {
        wasm.velloscene_circle_stroke(this.__wbg_ptr, cx, cy, radius, r, g, b, a, stroke_width);
    }
    /**
     * Create a new Vello scene builder
     * @param {number} width
     * @param {number} height
     */
    constructor(width, height) {
        const ret = wasm.velloscene_new(width, height);
        this.__wbg_ptr = ret >>> 0;
        VelloSceneFinalization.register(this, this.__wbg_ptr, this);
        return this;
    }
    /**
     * Draw a line
     * @param {number} x1
     * @param {number} y1
     * @param {number} x2
     * @param {number} y2
     * @param {number} r
     * @param {number} g
     * @param {number} b
     * @param {number} a
     * @param {number} stroke_width
     */
    line(x1, y1, x2, y2, r, g, b, a, stroke_width) {
        wasm.velloscene_line(this.__wbg_ptr, x1, y1, x2, y2, r, g, b, a, stroke_width);
    }
    /**
     * Draw a filled rectangle
     * @param {number} x
     * @param {number} y
     * @param {number} w
     * @param {number} h
     * @param {number} r
     * @param {number} g
     * @param {number} b
     * @param {number} a
     */
    rect(x, y, w, h, r, g, b, a) {
        wasm.velloscene_rect(this.__wbg_ptr, x, y, w, h, r, g, b, a);
    }
    /**
     * Clear the scene with a color
     * @param {number} r
     * @param {number} g
     * @param {number} b
     * @param {number} a
     */
    clear(r, g, b, a) {
        wasm.velloscene_clear(this.__wbg_ptr, r, g, b, a);
    }
    /**
     * Get the width
     * @returns {number}
     */
    width() {
        const ret = wasm.velloscene_width(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
     * Draw a filled circle
     * @param {number} cx
     * @param {number} cy
     * @param {number} radius
     * @param {number} r
     * @param {number} g
     * @param {number} b
     * @param {number} a
     */
    circle(cx, cy, radius, r, g, b, a) {
        wasm.velloscene_circle(this.__wbg_ptr, cx, cy, radius, r, g, b, a);
    }
    /**
     * Get the height
     * @returns {number}
     */
    height() {
        const ret = wasm.velloscene_height(this.__wbg_ptr);
        return ret >>> 0;
    }
    /**
     * Resize the scene
     * @param {number} width
     * @param {number} height
     */
    resize(width, height) {
        wasm.velloscene_resize(this.__wbg_ptr, width, height);
    }
    /**
     * Get the clear color A
     * @returns {number}
     */
    clear_a() {
        const ret = wasm.velloscene_clear_a(this.__wbg_ptr);
        return ret;
    }
    /**
     * Get the clear color B
     * @returns {number}
     */
    clear_b() {
        const ret = wasm.velloscene_clear_b(this.__wbg_ptr);
        return ret;
    }
    /**
     * Get the clear color G
     * @returns {number}
     */
    clear_g() {
        const ret = wasm.velloscene_clear_g(this.__wbg_ptr);
        return ret;
    }
    /**
     * Get the clear color R
     * @returns {number}
     */
    clear_r() {
        const ret = wasm.velloscene_clear_r(this.__wbg_ptr);
        return ret;
    }
}
if (Symbol.dispose) VelloScene.prototype[Symbol.dispose] = VelloScene.prototype.free;

export function init() {
    wasm.init();
}

const EXPECTED_RESPONSE_TYPES = new Set(['basic', 'cors', 'default']);

async function __wbg_load(module, imports) {
    if (typeof Response === 'function' && module instanceof Response) {
        if (typeof WebAssembly.instantiateStreaming === 'function') {
            try {
                return await WebAssembly.instantiateStreaming(module, imports);
            } catch (e) {
                const validResponse = module.ok && EXPECTED_RESPONSE_TYPES.has(module.type);

                if (validResponse && module.headers.get('Content-Type') !== 'application/wasm') {
                    console.warn("`WebAssembly.instantiateStreaming` failed because your server does not serve Wasm with `application/wasm` MIME type. Falling back to `WebAssembly.instantiate` which is slower. Original error:\n", e);

                } else {
                    throw e;
                }
            }
        }

        const bytes = await module.arrayBuffer();
        return await WebAssembly.instantiate(bytes, imports);
    } else {
        const instance = await WebAssembly.instantiate(module, imports);

        if (instance instanceof WebAssembly.Instance) {
            return { instance, module };
        } else {
            return instance;
        }
    }
}

function __wbg_get_imports() {
    const imports = {};
    imports.wbg = {};
    imports.wbg.__wbg___wbindgen_throw_dd24417ed36fc46e = function(arg0, arg1) {
        throw new Error(getStringFromWasm0(arg0, arg1));
    };
    imports.wbg.__wbg_error_7534b8e9a36f1ab4 = function(arg0, arg1) {
        let deferred0_0;
        let deferred0_1;
        try {
            deferred0_0 = arg0;
            deferred0_1 = arg1;
            console.error(getStringFromWasm0(arg0, arg1));
        } finally {
            wasm.__wbindgen_free(deferred0_0, deferred0_1, 1);
        }
    };
    imports.wbg.__wbg_log_1d990106d99dacb7 = function(arg0) {
        console.log(arg0);
    };
    imports.wbg.__wbg_new_8a6f238a6ece86ea = function() {
        const ret = new Error();
        return ret;
    };
    imports.wbg.__wbg_stack_0ed75d68575b0f3c = function(arg0, arg1) {
        const ret = arg1.stack;
        const ptr1 = passStringToWasm0(ret, wasm.__wbindgen_malloc, wasm.__wbindgen_realloc);
        const len1 = WASM_VECTOR_LEN;
        getDataViewMemory0().setInt32(arg0 + 4 * 1, len1, true);
        getDataViewMemory0().setInt32(arg0 + 4 * 0, ptr1, true);
    };
    imports.wbg.__wbindgen_cast_2241b6af4c4b2941 = function(arg0, arg1) {
        // Cast intrinsic for `Ref(String) -> Externref`.
        const ret = getStringFromWasm0(arg0, arg1);
        return ret;
    };
    imports.wbg.__wbindgen_init_externref_table = function() {
        const table = wasm.__wbindgen_externrefs;
        const offset = table.grow(4);
        table.set(0, undefined);
        table.set(offset + 0, undefined);
        table.set(offset + 1, null);
        table.set(offset + 2, true);
        table.set(offset + 3, false);
    };

    return imports;
}

function __wbg_finalize_init(instance, module) {
    wasm = instance.exports;
    __wbg_init.__wbindgen_wasm_module = module;
    cachedDataViewMemory0 = null;
    cachedUint8ArrayMemory0 = null;


    wasm.__wbindgen_start();
    return wasm;
}

function initSync(module) {
    if (wasm !== undefined) return wasm;


    if (typeof module !== 'undefined') {
        if (Object.getPrototypeOf(module) === Object.prototype) {
            ({module} = module)
        } else {
            console.warn('using deprecated parameters for `initSync()`; pass a single object instead')
        }
    }

    const imports = __wbg_get_imports();
    if (!(module instanceof WebAssembly.Module)) {
        module = new WebAssembly.Module(module);
    }
    const instance = new WebAssembly.Instance(module, imports);
    return __wbg_finalize_init(instance, module);
}

async function __wbg_init(module_or_path) {
    if (wasm !== undefined) return wasm;


    if (typeof module_or_path !== 'undefined') {
        if (Object.getPrototypeOf(module_or_path) === Object.prototype) {
            ({module_or_path} = module_or_path)
        } else {
            console.warn('using deprecated parameters for the initialization function; pass a single object instead')
        }
    }

    if (typeof module_or_path === 'undefined') {
        module_or_path = new URL('vello_wasm_bg.wasm', import.meta.url);
    }
    const imports = __wbg_get_imports();

    if (typeof module_or_path === 'string' || (typeof Request === 'function' && module_or_path instanceof Request) || (typeof URL === 'function' && module_or_path instanceof URL)) {
        module_or_path = fetch(module_or_path);
    }

    const { instance, module } = await __wbg_load(await module_or_path, imports);

    return __wbg_finalize_init(instance, module);
}

export { initSync };
export default __wbg_init;
