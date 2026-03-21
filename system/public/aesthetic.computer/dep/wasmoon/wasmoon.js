(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports) :
    typeof define === 'function' && define.amd ? define(['exports'], factory) :
    (global = typeof globalThis !== 'undefined' ? globalThis : global || self, factory(global.wasmoon = {}));
})(this, (function (exports) { 'use strict';

    var _documentCurrentScript = typeof document !== 'undefined' ? document.currentScript : null;
    exports.LuaReturn = void 0;
    (function (LuaReturn) {
        LuaReturn[LuaReturn["Ok"] = 0] = "Ok";
        LuaReturn[LuaReturn["Yield"] = 1] = "Yield";
        LuaReturn[LuaReturn["ErrorRun"] = 2] = "ErrorRun";
        LuaReturn[LuaReturn["ErrorSyntax"] = 3] = "ErrorSyntax";
        LuaReturn[LuaReturn["ErrorMem"] = 4] = "ErrorMem";
        LuaReturn[LuaReturn["ErrorErr"] = 5] = "ErrorErr";
        LuaReturn[LuaReturn["ErrorFile"] = 6] = "ErrorFile";
    })(exports.LuaReturn || (exports.LuaReturn = {}));
    const PointerSize = 4;
    const LUA_MULTRET = -1;
    const LUAI_MAXSTACK = 1000000;
    const LUA_REGISTRYINDEX = -LUAI_MAXSTACK - 1000;
    exports.LuaType = void 0;
    (function (LuaType) {
        LuaType[LuaType["None"] = -1] = "None";
        LuaType[LuaType["Nil"] = 0] = "Nil";
        LuaType[LuaType["Boolean"] = 1] = "Boolean";
        LuaType[LuaType["LightUserdata"] = 2] = "LightUserdata";
        LuaType[LuaType["Number"] = 3] = "Number";
        LuaType[LuaType["String"] = 4] = "String";
        LuaType[LuaType["Table"] = 5] = "Table";
        LuaType[LuaType["Function"] = 6] = "Function";
        LuaType[LuaType["Userdata"] = 7] = "Userdata";
        LuaType[LuaType["Thread"] = 8] = "Thread";
    })(exports.LuaType || (exports.LuaType = {}));
    exports.LuaEventCodes = void 0;
    (function (LuaEventCodes) {
        LuaEventCodes[LuaEventCodes["Call"] = 0] = "Call";
        LuaEventCodes[LuaEventCodes["Ret"] = 1] = "Ret";
        LuaEventCodes[LuaEventCodes["Line"] = 2] = "Line";
        LuaEventCodes[LuaEventCodes["Count"] = 3] = "Count";
        LuaEventCodes[LuaEventCodes["TailCall"] = 4] = "TailCall";
    })(exports.LuaEventCodes || (exports.LuaEventCodes = {}));
    exports.LuaEventMasks = void 0;
    (function (LuaEventMasks) {
        LuaEventMasks[LuaEventMasks["Call"] = 1] = "Call";
        LuaEventMasks[LuaEventMasks["Ret"] = 2] = "Ret";
        LuaEventMasks[LuaEventMasks["Line"] = 4] = "Line";
        LuaEventMasks[LuaEventMasks["Count"] = 8] = "Count";
    })(exports.LuaEventMasks || (exports.LuaEventMasks = {}));
    exports.LuaLibraries = void 0;
    (function (LuaLibraries) {
        LuaLibraries["Base"] = "_G";
        LuaLibraries["Coroutine"] = "coroutine";
        LuaLibraries["Table"] = "table";
        LuaLibraries["IO"] = "io";
        LuaLibraries["OS"] = "os";
        LuaLibraries["String"] = "string";
        LuaLibraries["UTF8"] = "utf8";
        LuaLibraries["Math"] = "math";
        LuaLibraries["Debug"] = "debug";
        LuaLibraries["Package"] = "package";
    })(exports.LuaLibraries || (exports.LuaLibraries = {}));
    class LuaTimeoutError extends Error {
    }

    class Decoration {
        constructor(target, options) {
            this.target = target;
            this.options = options;
        }
    }
    function decorate(target, options) {
        return new Decoration(target, options);
    }

    class Pointer extends Number {
    }

    class MultiReturn extends Array {
    }

    const INSTRUCTION_HOOK_COUNT = 1000;
    class Thread {
        constructor(lua, typeExtensions, address, parent) {
            this.closed = false;
            this.lua = lua;
            this.typeExtensions = typeExtensions;
            this.address = address;
            this.parent = parent;
        }
        newThread() {
            const address = this.lua.lua_newthread(this.address);
            if (!address) {
                throw new Error('lua_newthread returned a null pointer');
            }
            return new Thread(this.lua, this.typeExtensions, address, this.parent || this);
        }
        resetThread() {
            this.assertOk(this.lua.lua_resetthread(this.address));
        }
        loadString(luaCode, name) {
            const size = this.lua.module.lengthBytesUTF8(luaCode);
            const pointerSize = size + 1;
            const bufferPointer = this.lua.module._malloc(pointerSize);
            try {
                this.lua.module.stringToUTF8(luaCode, bufferPointer, pointerSize);
                this.assertOk(this.lua.luaL_loadbufferx(this.address, bufferPointer, size, name !== null && name !== void 0 ? name : bufferPointer, null));
            }
            finally {
                this.lua.module._free(bufferPointer);
            }
        }
        loadFile(filename) {
            this.assertOk(this.lua.luaL_loadfilex(this.address, filename, null));
        }
        resume(argCount = 0) {
            const dataPointer = this.lua.module._malloc(PointerSize);
            try {
                this.lua.module.setValue(dataPointer, 0, 'i32');
                const luaResult = this.lua.lua_resume(this.address, null, argCount, dataPointer);
                return {
                    result: luaResult,
                    resultCount: this.lua.module.getValue(dataPointer, 'i32'),
                };
            }
            finally {
                this.lua.module._free(dataPointer);
            }
        }
        getTop() {
            return this.lua.lua_gettop(this.address);
        }
        setTop(index) {
            this.lua.lua_settop(this.address, index);
        }
        remove(index) {
            return this.lua.lua_remove(this.address, index);
        }
        setField(index, name, value) {
            index = this.lua.lua_absindex(this.address, index);
            this.pushValue(value);
            this.lua.lua_setfield(this.address, index, name);
        }
        async run(argCount = 0, options) {
            const originalTimeout = this.timeout;
            try {
                if ((options === null || options === void 0 ? void 0 : options.timeout) !== undefined) {
                    this.setTimeout(Date.now() + options.timeout);
                }
                let resumeResult = this.resume(argCount);
                while (resumeResult.result === exports.LuaReturn.Yield) {
                    if (this.timeout && Date.now() > this.timeout) {
                        if (resumeResult.resultCount > 0) {
                            this.pop(resumeResult.resultCount);
                        }
                        throw new LuaTimeoutError(`thread timeout exceeded`);
                    }
                    if (resumeResult.resultCount > 0) {
                        const lastValue = this.getValue(-1);
                        this.pop(resumeResult.resultCount);
                        if (lastValue === Promise.resolve(lastValue)) {
                            await lastValue;
                        }
                        else {
                            await new Promise((resolve) => setImmediate(resolve));
                        }
                    }
                    else {
                        await new Promise((resolve) => setImmediate(resolve));
                    }
                    resumeResult = this.resume(0);
                }
                this.assertOk(resumeResult.result);
                return this.getStackValues();
            }
            finally {
                if ((options === null || options === void 0 ? void 0 : options.timeout) !== undefined) {
                    this.setTimeout(originalTimeout);
                }
            }
        }
        runSync(argCount = 0) {
            const base = this.getTop() - argCount - 1;
            this.assertOk(this.lua.lua_pcallk(this.address, argCount, LUA_MULTRET, 0, 0, null));
            return this.getStackValues(base);
        }
        pop(count = 1) {
            this.lua.lua_pop(this.address, count);
        }
        call(name, ...args) {
            const type = this.lua.lua_getglobal(this.address, name);
            if (type !== exports.LuaType.Function) {
                throw new Error(`A function of type '${type}' was pushed, expected is ${exports.LuaType.Function}`);
            }
            for (const arg of args) {
                this.pushValue(arg);
            }
            const base = this.getTop() - args.length - 1;
            this.lua.lua_callk(this.address, args.length, LUA_MULTRET, 0, null);
            return this.getStackValues(base);
        }
        getStackValues(start = 0) {
            const returns = this.getTop() - start;
            const returnValues = new MultiReturn(returns);
            for (let i = 0; i < returns; i++) {
                returnValues[i] = this.getValue(start + i + 1);
            }
            return returnValues;
        }
        stateToThread(L) {
            var _a;
            return L === ((_a = this.parent) === null || _a === void 0 ? void 0 : _a.address) ? this.parent : new Thread(this.lua, this.typeExtensions, L, this.parent || this);
        }
        pushValue(rawValue, userdata) {
            const decoratedValue = this.getValueDecorations(rawValue);
            const target = decoratedValue.target;
            if (target instanceof Thread) {
                const isMain = this.lua.lua_pushthread(target.address) === 1;
                if (!isMain) {
                    this.lua.lua_xmove(target.address, this.address, 1);
                }
                return;
            }
            const startTop = this.getTop();
            switch (typeof target) {
                case 'undefined':
                    this.lua.lua_pushnil(this.address);
                    break;
                case 'number':
                    if (Number.isInteger(target)) {
                        this.lua.lua_pushinteger(this.address, BigInt(target));
                    }
                    else {
                        this.lua.lua_pushnumber(this.address, target);
                    }
                    break;
                case 'string':
                    this.lua.lua_pushstring(this.address, target);
                    break;
                case 'boolean':
                    this.lua.lua_pushboolean(this.address, target ? 1 : 0);
                    break;
                default:
                    if (!this.typeExtensions.find((wrapper) => wrapper.extension.pushValue(this, decoratedValue, userdata))) {
                        throw new Error(`The type '${typeof target}' is not supported by Lua`);
                    }
            }
            if (decoratedValue.options.metatable) {
                this.setMetatable(-1, decoratedValue.options.metatable);
            }
            if (this.getTop() !== startTop + 1) {
                throw new Error(`pushValue expected stack size ${startTop + 1}, got ${this.getTop()}`);
            }
        }
        setMetatable(index, metatable) {
            index = this.lua.lua_absindex(this.address, index);
            if (this.lua.lua_getmetatable(this.address, index)) {
                this.pop(1);
                const name = this.getMetatableName(index);
                throw new Error(`data already has associated metatable: ${name || 'unknown name'}`);
            }
            this.pushValue(metatable);
            this.lua.lua_setmetatable(this.address, index);
        }
        getMetatableName(index) {
            const metatableNameType = this.lua.luaL_getmetafield(this.address, index, '__name');
            if (metatableNameType === exports.LuaType.Nil) {
                return undefined;
            }
            if (metatableNameType !== exports.LuaType.String) {
                this.pop(1);
                return undefined;
            }
            const name = this.lua.lua_tolstring(this.address, -1, null);
            this.pop(1);
            return name;
        }
        getValue(index, inputType, userdata) {
            index = this.lua.lua_absindex(this.address, index);
            const type = inputType !== null && inputType !== void 0 ? inputType : this.lua.lua_type(this.address, index);
            switch (type) {
                case exports.LuaType.None:
                    return undefined;
                case exports.LuaType.Nil:
                    return null;
                case exports.LuaType.Number:
                    return this.lua.lua_tonumberx(this.address, index, null);
                case exports.LuaType.String:
                    return this.lua.lua_tolstring(this.address, index, null);
                case exports.LuaType.Boolean:
                    return Boolean(this.lua.lua_toboolean(this.address, index));
                case exports.LuaType.Thread:
                    return this.stateToThread(this.lua.lua_tothread(this.address, index));
                default: {
                    let metatableName;
                    if (type === exports.LuaType.Table || type === exports.LuaType.Userdata) {
                        metatableName = this.getMetatableName(index);
                    }
                    const typeExtensionWrapper = this.typeExtensions.find((wrapper) => wrapper.extension.isType(this, index, type, metatableName));
                    if (typeExtensionWrapper) {
                        return typeExtensionWrapper.extension.getValue(this, index, userdata);
                    }
                    console.warn(`The type '${this.lua.lua_typename(this.address, type)}' returned is not supported on JS`);
                    return new Pointer(this.lua.lua_topointer(this.address, index));
                }
            }
        }
        close() {
            if (this.isClosed()) {
                return;
            }
            if (this.hookFunctionPointer) {
                this.lua.module.removeFunction(this.hookFunctionPointer);
            }
            this.closed = true;
        }
        setTimeout(timeout) {
            if (timeout && timeout > 0) {
                if (!this.hookFunctionPointer) {
                    this.hookFunctionPointer = this.lua.module.addFunction(() => {
                        if (Date.now() > timeout) {
                            this.pushValue(new LuaTimeoutError(`thread timeout exceeded`));
                            this.lua.lua_error(this.address);
                        }
                    }, 'vii');
                }
                this.lua.lua_sethook(this.address, this.hookFunctionPointer, exports.LuaEventMasks.Count, INSTRUCTION_HOOK_COUNT);
                this.timeout = timeout;
            }
            else if (this.hookFunctionPointer) {
                this.hookFunctionPointer = undefined;
                this.timeout = undefined;
                this.lua.lua_sethook(this.address, null, 0, 0);
            }
        }
        getTimeout() {
            return this.timeout;
        }
        getPointer(index) {
            return new Pointer(this.lua.lua_topointer(this.address, index));
        }
        isClosed() {
            var _a;
            return !this.address || this.closed || Boolean((_a = this.parent) === null || _a === void 0 ? void 0 : _a.isClosed());
        }
        indexToString(index) {
            const str = this.lua.luaL_tolstring(this.address, index, null);
            this.pop();
            return str;
        }
        dumpStack(log = console.log) {
            const top = this.getTop();
            for (let i = 1; i <= top; i++) {
                const type = this.lua.lua_type(this.address, i);
                const typename = this.lua.lua_typename(this.address, type);
                const pointer = this.getPointer(i);
                const name = this.indexToString(i);
                const value = this.getValue(i, type);
                log(i, typename, pointer, name, value);
            }
        }
        assertOk(result) {
            if (result !== exports.LuaReturn.Ok && result !== exports.LuaReturn.Yield) {
                const resultString = exports.LuaReturn[result];
                const error = new Error(`Lua Error(${resultString}/${result})`);
                if (this.getTop() > 0) {
                    if (result === exports.LuaReturn.ErrorMem) {
                        error.message = this.lua.lua_tolstring(this.address, -1, null);
                    }
                    else {
                        const luaError = this.getValue(-1);
                        if (luaError instanceof Error) {
                            error.stack = luaError.stack;
                        }
                        error.message = this.indexToString(-1);
                    }
                }
                if (result !== exports.LuaReturn.ErrorMem) {
                    try {
                        this.lua.luaL_traceback(this.address, this.address, null, 1);
                        const traceback = this.lua.lua_tolstring(this.address, -1, null);
                        if (traceback.trim() !== 'stack traceback:') {
                            error.message = `${error.message}\n${traceback}`;
                        }
                        this.pop(1);
                    }
                    catch (err) {
                        console.warn('Failed to generate stack trace', err);
                    }
                }
                throw error;
            }
        }
        getValueDecorations(value) {
            return value instanceof Decoration ? value : new Decoration(value, {});
        }
    }

    class Global extends Thread {
        constructor(cmodule, shouldTraceAllocations) {
            if (shouldTraceAllocations) {
                const memoryStats = { memoryUsed: 0 };
                const allocatorFunctionPointer = cmodule.module.addFunction((_userData, pointer, oldSize, newSize) => {
                    if (newSize === 0) {
                        if (pointer) {
                            memoryStats.memoryUsed -= oldSize;
                            cmodule.module._free(pointer);
                        }
                        return 0;
                    }
                    const endMemoryDelta = pointer ? newSize - oldSize : newSize;
                    const endMemory = memoryStats.memoryUsed + endMemoryDelta;
                    if (newSize > oldSize && memoryStats.memoryMax && endMemory > memoryStats.memoryMax) {
                        return 0;
                    }
                    const reallocated = cmodule.module._realloc(pointer, newSize);
                    if (reallocated) {
                        memoryStats.memoryUsed = endMemory;
                    }
                    return reallocated;
                }, 'iiiii');
                const address = cmodule.lua_newstate(allocatorFunctionPointer, null);
                if (!address) {
                    cmodule.module.removeFunction(allocatorFunctionPointer);
                    throw new Error('lua_newstate returned a null pointer');
                }
                super(cmodule, [], address);
                this.memoryStats = memoryStats;
                this.allocatorFunctionPointer = allocatorFunctionPointer;
            }
            else {
                super(cmodule, [], cmodule.luaL_newstate());
            }
            if (this.isClosed()) {
                throw new Error('Global state could not be created (probably due to lack of memory)');
            }
        }
        close() {
            if (this.isClosed()) {
                return;
            }
            super.close();
            this.lua.lua_close(this.address);
            if (this.allocatorFunctionPointer) {
                this.lua.module.removeFunction(this.allocatorFunctionPointer);
            }
            for (const wrapper of this.typeExtensions) {
                wrapper.extension.close();
            }
        }
        registerTypeExtension(priority, extension) {
            this.typeExtensions.push({ extension, priority });
            this.typeExtensions.sort((a, b) => b.priority - a.priority);
        }
        loadLibrary(library) {
            switch (library) {
                case exports.LuaLibraries.Base:
                    this.lua.luaopen_base(this.address);
                    break;
                case exports.LuaLibraries.Coroutine:
                    this.lua.luaopen_coroutine(this.address);
                    break;
                case exports.LuaLibraries.Table:
                    this.lua.luaopen_table(this.address);
                    break;
                case exports.LuaLibraries.IO:
                    this.lua.luaopen_io(this.address);
                    break;
                case exports.LuaLibraries.OS:
                    this.lua.luaopen_os(this.address);
                    break;
                case exports.LuaLibraries.String:
                    this.lua.luaopen_string(this.address);
                    break;
                case exports.LuaLibraries.UTF8:
                    this.lua.luaopen_string(this.address);
                    break;
                case exports.LuaLibraries.Math:
                    this.lua.luaopen_math(this.address);
                    break;
                case exports.LuaLibraries.Debug:
                    this.lua.luaopen_debug(this.address);
                    break;
                case exports.LuaLibraries.Package:
                    this.lua.luaopen_package(this.address);
                    break;
            }
            this.lua.lua_setglobal(this.address, library);
        }
        get(name) {
            const type = this.lua.lua_getglobal(this.address, name);
            const value = this.getValue(-1, type);
            this.pop();
            return value;
        }
        set(name, value) {
            this.pushValue(value);
            this.lua.lua_setglobal(this.address, name);
        }
        getTable(name, callback) {
            const startStackTop = this.getTop();
            const type = this.lua.lua_getglobal(this.address, name);
            try {
                if (type !== exports.LuaType.Table) {
                    throw new TypeError(`Unexpected type in ${name}. Expected ${exports.LuaType[exports.LuaType.Table]}. Got ${exports.LuaType[type]}.`);
                }
                callback(startStackTop + 1);
            }
            finally {
                if (this.getTop() !== startStackTop + 1) {
                    console.warn(`getTable: expected stack size ${startStackTop} got ${this.getTop()}`);
                }
                this.setTop(startStackTop);
            }
        }
        getMemoryUsed() {
            return this.getMemoryStatsRef().memoryUsed;
        }
        getMemoryMax() {
            return this.getMemoryStatsRef().memoryMax;
        }
        setMemoryMax(max) {
            this.getMemoryStatsRef().memoryMax = max;
        }
        getMemoryStatsRef() {
            if (!this.memoryStats) {
                throw new Error('Memory allocations is not being traced, please build engine with { traceAllocations: true }');
            }
            return this.memoryStats;
        }
    }

    class LuaTypeExtension {
        constructor(thread, name) {
            this.thread = thread;
            this.name = name;
        }
        isType(_thread, _index, type, name) {
            return type === exports.LuaType.Userdata && name === this.name;
        }
        getValue(thread, index, _userdata) {
            const refUserdata = thread.lua.luaL_testudata(thread.address, index, this.name);
            if (!refUserdata) {
                throw new Error(`data does not have the expected metatable: ${this.name}`);
            }
            const referencePointer = thread.lua.module.getValue(refUserdata, '*');
            return thread.lua.getRef(referencePointer);
        }
        pushValue(thread, decoratedValue, _userdata) {
            const { target } = decoratedValue;
            const pointer = thread.lua.ref(target);
            const userDataPointer = thread.lua.lua_newuserdatauv(thread.address, PointerSize, 0);
            thread.lua.module.setValue(userDataPointer, pointer, '*');
            if (exports.LuaType.Nil === thread.lua.luaL_getmetatable(thread.address, this.name)) {
                thread.pop(2);
                throw new Error(`metatable not found: ${this.name}`);
            }
            thread.lua.lua_setmetatable(thread.address, -2);
            return true;
        }
    }

    class ErrorTypeExtension extends LuaTypeExtension {
        constructor(thread, injectObject) {
            super(thread, 'js_error');
            this.gcPointer = thread.lua.module.addFunction((functionStateAddress) => {
                const userDataPointer = thread.lua.luaL_checkudata(functionStateAddress, 1, this.name);
                const referencePointer = thread.lua.module.getValue(userDataPointer, '*');
                thread.lua.unref(referencePointer);
                return exports.LuaReturn.Ok;
            }, 'ii');
            if (thread.lua.luaL_newmetatable(thread.address, this.name)) {
                const metatableIndex = thread.lua.lua_gettop(thread.address);
                thread.lua.lua_pushstring(thread.address, 'protected metatable');
                thread.lua.lua_setfield(thread.address, metatableIndex, '__metatable');
                thread.lua.lua_pushcclosure(thread.address, this.gcPointer, 0);
                thread.lua.lua_setfield(thread.address, metatableIndex, '__gc');
                thread.pushValue((jsRefError, key) => {
                    if (key === 'message') {
                        return jsRefError.message;
                    }
                    return null;
                });
                thread.lua.lua_setfield(thread.address, metatableIndex, '__index');
                thread.pushValue((jsRefError) => {
                    return jsRefError.message;
                });
                thread.lua.lua_setfield(thread.address, metatableIndex, '__tostring');
            }
            thread.lua.lua_pop(thread.address, 1);
            if (injectObject) {
                thread.set('Error', {
                    create: (message) => {
                        if (message && typeof message !== 'string') {
                            throw new Error('message must be a string');
                        }
                        return new Error(message);
                    },
                });
            }
        }
        pushValue(thread, decoration) {
            if (!(decoration.target instanceof Error)) {
                return false;
            }
            return super.pushValue(thread, decoration);
        }
        close() {
            this.thread.lua.module.removeFunction(this.gcPointer);
        }
    }
    function createTypeExtension$6(thread, injectObject) {
        return new ErrorTypeExtension(thread, injectObject);
    }

    class RawResult {
        constructor(count) {
            this.count = count;
        }
    }

    function decorateFunction(target, options) {
        return new Decoration(target, options);
    }
    class FunctionTypeExtension extends LuaTypeExtension {
        constructor(thread, options) {
            super(thread, 'js_function');
            this.functionRegistry = typeof FinalizationRegistry !== 'undefined'
                ? new FinalizationRegistry((func) => {
                    if (!this.thread.isClosed()) {
                        this.thread.lua.luaL_unref(this.thread.address, LUA_REGISTRYINDEX, func);
                    }
                })
                : undefined;
            this.options = options;
            this.callbackContext = thread.newThread();
            this.callbackContextIndex = this.thread.lua.luaL_ref(thread.address, LUA_REGISTRYINDEX);
            if (!this.functionRegistry) {
                console.warn('FunctionTypeExtension: FinalizationRegistry not found. Memory leaks likely.');
            }
            this.gcPointer = thread.lua.module.addFunction((calledL) => {
                thread.lua.luaL_checkudata(calledL, 1, this.name);
                const userDataPointer = thread.lua.luaL_checkudata(calledL, 1, this.name);
                const referencePointer = thread.lua.module.getValue(userDataPointer, '*');
                thread.lua.unref(referencePointer);
                return exports.LuaReturn.Ok;
            }, 'ii');
            if (thread.lua.luaL_newmetatable(thread.address, this.name)) {
                thread.lua.lua_pushstring(thread.address, '__gc');
                thread.lua.lua_pushcclosure(thread.address, this.gcPointer, 0);
                thread.lua.lua_settable(thread.address, -3);
                thread.lua.lua_pushstring(thread.address, '__metatable');
                thread.lua.lua_pushstring(thread.address, 'protected metatable');
                thread.lua.lua_settable(thread.address, -3);
            }
            thread.lua.lua_pop(thread.address, 1);
            this.functionWrapper = thread.lua.module.addFunction((calledL) => {
                const calledThread = thread.stateToThread(calledL);
                const refUserdata = thread.lua.luaL_checkudata(calledL, thread.lua.lua_upvalueindex(1), this.name);
                const refPointer = thread.lua.module.getValue(refUserdata, '*');
                const { target, options } = thread.lua.getRef(refPointer);
                const argsQuantity = calledThread.getTop();
                const args = [];
                if (options.receiveThread) {
                    args.push(calledThread);
                }
                if (options.receiveArgsQuantity) {
                    args.push(argsQuantity);
                }
                else {
                    for (let i = 1; i <= argsQuantity; i++) {
                        const value = calledThread.getValue(i);
                        if (i !== 1 || !(options === null || options === void 0 ? void 0 : options.self) || value !== options.self) {
                            args.push(value);
                        }
                    }
                }
                try {
                    const result = target.apply(options === null || options === void 0 ? void 0 : options.self, args);
                    if (result === undefined) {
                        return 0;
                    }
                    else if (result instanceof RawResult) {
                        return result.count;
                    }
                    else if (result instanceof MultiReturn) {
                        for (const item of result) {
                            calledThread.pushValue(item);
                        }
                        return result.length;
                    }
                    else {
                        calledThread.pushValue(result);
                        return 1;
                    }
                }
                catch (err) {
                    if (err === Infinity) {
                        throw err;
                    }
                    calledThread.pushValue(err);
                    return calledThread.lua.lua_error(calledThread.address);
                }
            }, 'ii');
        }
        close() {
            this.thread.lua.module.removeFunction(this.gcPointer);
            this.thread.lua.module.removeFunction(this.functionWrapper);
            this.callbackContext.close();
            this.callbackContext.lua.luaL_unref(this.callbackContext.address, LUA_REGISTRYINDEX, this.callbackContextIndex);
        }
        isType(_thread, _index, type) {
            return type === exports.LuaType.Function;
        }
        pushValue(thread, decoration) {
            if (typeof decoration.target !== 'function') {
                return false;
            }
            const pointer = thread.lua.ref(decoration);
            const userDataPointer = thread.lua.lua_newuserdatauv(thread.address, PointerSize, 0);
            thread.lua.module.setValue(userDataPointer, pointer, '*');
            if (exports.LuaType.Nil === thread.lua.luaL_getmetatable(thread.address, this.name)) {
                thread.pop(1);
                thread.lua.unref(pointer);
                throw new Error(`metatable not found: ${this.name}`);
            }
            thread.lua.lua_setmetatable(thread.address, -2);
            thread.lua.lua_pushcclosure(thread.address, this.functionWrapper, 1);
            return true;
        }
        getValue(thread, index) {
            var _a;
            thread.lua.lua_pushvalue(thread.address, index);
            const func = thread.lua.luaL_ref(thread.address, LUA_REGISTRYINDEX);
            const jsFunc = (...args) => {
                var _a;
                if (this.callbackContext.isClosed()) {
                    console.warn('Tried to call a function after closing lua state');
                    return;
                }
                const callThread = this.callbackContext.newThread();
                try {
                    const internalType = callThread.lua.lua_rawgeti(callThread.address, LUA_REGISTRYINDEX, BigInt(func));
                    if (internalType !== exports.LuaType.Function) {
                        const callMetafieldType = callThread.lua.luaL_getmetafield(callThread.address, -1, '__call');
                        callThread.pop();
                        if (callMetafieldType !== exports.LuaType.Function) {
                            throw new Error(`A value of type '${internalType}' was pushed but it is not callable`);
                        }
                    }
                    for (const arg of args) {
                        callThread.pushValue(arg);
                    }
                    if ((_a = this.options) === null || _a === void 0 ? void 0 : _a.functionTimeout) {
                        callThread.setTimeout(Date.now() + this.options.functionTimeout);
                    }
                    const status = callThread.lua.lua_pcallk(callThread.address, args.length, 1, 0, 0, null);
                    if (status === exports.LuaReturn.Yield) {
                        throw new Error('cannot yield in callbacks from javascript');
                    }
                    callThread.assertOk(status);
                    if (callThread.getTop() > 0) {
                        return callThread.getValue(-1);
                    }
                    return undefined;
                }
                finally {
                    callThread.close();
                    this.callbackContext.pop();
                }
            };
            (_a = this.functionRegistry) === null || _a === void 0 ? void 0 : _a.register(jsFunc, func);
            return jsFunc;
        }
    }
    function createTypeExtension$5(thread, options) {
        return new FunctionTypeExtension(thread, options);
    }

    class NullTypeExtension extends LuaTypeExtension {
        constructor(thread) {
            super(thread, 'js_null');
            this.gcPointer = thread.lua.module.addFunction((functionStateAddress) => {
                const userDataPointer = thread.lua.luaL_checkudata(functionStateAddress, 1, this.name);
                const referencePointer = thread.lua.module.getValue(userDataPointer, '*');
                thread.lua.unref(referencePointer);
                return exports.LuaReturn.Ok;
            }, 'ii');
            if (thread.lua.luaL_newmetatable(thread.address, this.name)) {
                const metatableIndex = thread.lua.lua_gettop(thread.address);
                thread.lua.lua_pushstring(thread.address, 'protected metatable');
                thread.lua.lua_setfield(thread.address, metatableIndex, '__metatable');
                thread.lua.lua_pushcclosure(thread.address, this.gcPointer, 0);
                thread.lua.lua_setfield(thread.address, metatableIndex, '__gc');
                thread.pushValue(() => null);
                thread.lua.lua_setfield(thread.address, metatableIndex, '__index');
                thread.pushValue(() => 'null');
                thread.lua.lua_setfield(thread.address, metatableIndex, '__tostring');
                thread.pushValue((self, other) => self === other);
                thread.lua.lua_setfield(thread.address, metatableIndex, '__eq');
            }
            thread.lua.lua_pop(thread.address, 1);
            super.pushValue(thread, new Decoration({}, {}));
            thread.lua.lua_setglobal(thread.address, 'null');
        }
        getValue(thread, index) {
            const refUserData = thread.lua.luaL_testudata(thread.address, index, this.name);
            if (!refUserData) {
                throw new Error(`data does not have the expected metatable: ${this.name}`);
            }
            return null;
        }
        pushValue(thread, decoration) {
            if ((decoration === null || decoration === void 0 ? void 0 : decoration.target) !== null) {
                return false;
            }
            thread.lua.lua_getglobal(thread.address, 'null');
            return true;
        }
        close() {
            this.thread.lua.module.removeFunction(this.gcPointer);
        }
    }
    function createTypeExtension$4(thread) {
        return new NullTypeExtension(thread);
    }

    class PromiseTypeExtension extends LuaTypeExtension {
        constructor(thread, injectObject) {
            super(thread, 'js_promise');
            this.gcPointer = thread.lua.module.addFunction((functionStateAddress) => {
                const userDataPointer = thread.lua.luaL_checkudata(functionStateAddress, 1, this.name);
                const referencePointer = thread.lua.module.getValue(userDataPointer, '*');
                thread.lua.unref(referencePointer);
                return exports.LuaReturn.Ok;
            }, 'ii');
            if (thread.lua.luaL_newmetatable(thread.address, this.name)) {
                const metatableIndex = thread.lua.lua_gettop(thread.address);
                thread.lua.lua_pushstring(thread.address, 'protected metatable');
                thread.lua.lua_setfield(thread.address, metatableIndex, '__metatable');
                thread.lua.lua_pushcclosure(thread.address, this.gcPointer, 0);
                thread.lua.lua_setfield(thread.address, metatableIndex, '__gc');
                const checkSelf = (self) => {
                    if (Promise.resolve(self) !== self && typeof self.then !== 'function') {
                        throw new Error('promise method called without self instance');
                    }
                    return true;
                };
                thread.pushValue({
                    next: (self, ...args) => checkSelf(self) && self.then(...args),
                    catch: (self, ...args) => checkSelf(self) && self.catch(...args),
                    finally: (self, ...args) => checkSelf(self) && self.finally(...args),
                    await: decorateFunction((functionThread, self) => {
                        checkSelf(self);
                        if (functionThread.address === thread.address) {
                            throw new Error('cannot await in the main thread');
                        }
                        let promiseResult = undefined;
                        const awaitPromise = self
                            .then((res) => {
                            promiseResult = { status: 'fulfilled', value: res };
                        })
                            .catch((err) => {
                            promiseResult = { status: 'rejected', value: err };
                        });
                        const continuance = this.thread.lua.module.addFunction((continuanceState) => {
                            if (!promiseResult) {
                                return thread.lua.lua_yieldk(functionThread.address, 0, 0, continuance);
                            }
                            this.thread.lua.module.removeFunction(continuance);
                            const continuanceThread = thread.stateToThread(continuanceState);
                            if (promiseResult.status === 'rejected') {
                                continuanceThread.pushValue(promiseResult.value || new Error('promise rejected with no error'));
                                return this.thread.lua.lua_error(continuanceState);
                            }
                            if (promiseResult.value instanceof RawResult) {
                                return promiseResult.value.count;
                            }
                            else if (promiseResult.value instanceof MultiReturn) {
                                for (const arg of promiseResult.value) {
                                    continuanceThread.pushValue(arg);
                                }
                                return promiseResult.value.length;
                            }
                            else {
                                continuanceThread.pushValue(promiseResult.value);
                                return 1;
                            }
                        }, 'iiii');
                        functionThread.pushValue(awaitPromise);
                        return new RawResult(thread.lua.lua_yieldk(functionThread.address, 1, 0, continuance));
                    }, { receiveThread: true }),
                });
                thread.lua.lua_setfield(thread.address, metatableIndex, '__index');
                thread.pushValue((self, other) => self === other);
                thread.lua.lua_setfield(thread.address, metatableIndex, '__eq');
            }
            thread.lua.lua_pop(thread.address, 1);
            if (injectObject) {
                thread.set('Promise', {
                    create: (callback) => new Promise(callback),
                    all: (promiseArray) => {
                        if (!Array.isArray(promiseArray)) {
                            throw new Error('argument must be an array of promises');
                        }
                        return Promise.all(promiseArray.map((potentialPromise) => Promise.resolve(potentialPromise)));
                    },
                    resolve: (value) => Promise.resolve(value),
                });
            }
        }
        close() {
            this.thread.lua.module.removeFunction(this.gcPointer);
        }
        pushValue(thread, decoration) {
            if (Promise.resolve(decoration.target) !== decoration.target && typeof decoration.target.then !== 'function') {
                return false;
            }
            return super.pushValue(thread, decoration);
        }
    }
    function createTypeExtension$3(thread, injectObject) {
        return new PromiseTypeExtension(thread, injectObject);
    }

    function decorateProxy(target, options) {
        return new Decoration(target, options || {});
    }
    class ProxyTypeExtension extends LuaTypeExtension {
        constructor(thread) {
            super(thread, 'js_proxy');
            this.gcPointer = thread.lua.module.addFunction((functionStateAddress) => {
                const userDataPointer = thread.lua.luaL_checkudata(functionStateAddress, 1, this.name);
                const referencePointer = thread.lua.module.getValue(userDataPointer, '*');
                thread.lua.unref(referencePointer);
                return exports.LuaReturn.Ok;
            }, 'ii');
            if (thread.lua.luaL_newmetatable(thread.address, this.name)) {
                const metatableIndex = thread.lua.lua_gettop(thread.address);
                thread.lua.lua_pushstring(thread.address, 'protected metatable');
                thread.lua.lua_setfield(thread.address, metatableIndex, '__metatable');
                thread.lua.lua_pushcclosure(thread.address, this.gcPointer, 0);
                thread.lua.lua_setfield(thread.address, metatableIndex, '__gc');
                thread.pushValue((self, key) => {
                    switch (typeof key) {
                        case 'number':
                            key = key - 1;
                        case 'string':
                            break;
                        default:
                            throw new Error('Only strings or numbers can index js objects');
                    }
                    const value = self[key];
                    if (typeof value === 'function') {
                        return decorateFunction(value, { self });
                    }
                    return value;
                });
                thread.lua.lua_setfield(thread.address, metatableIndex, '__index');
                thread.pushValue((self, key, value) => {
                    switch (typeof key) {
                        case 'number':
                            key = key - 1;
                        case 'string':
                            break;
                        default:
                            throw new Error('Only strings or numbers can index js objects');
                    }
                    self[key] = value;
                });
                thread.lua.lua_setfield(thread.address, metatableIndex, '__newindex');
                thread.pushValue((self) => {
                    var _a, _b;
                    return (_b = (_a = self.toString) === null || _a === void 0 ? void 0 : _a.call(self)) !== null && _b !== void 0 ? _b : typeof self;
                });
                thread.lua.lua_setfield(thread.address, metatableIndex, '__tostring');
                thread.pushValue((self) => {
                    return self.length || 0;
                });
                thread.lua.lua_setfield(thread.address, metatableIndex, '__len');
                thread.pushValue((self) => {
                    const keys = Object.getOwnPropertyNames(self);
                    let i = 0;
                    return MultiReturn.of(() => {
                        const ret = MultiReturn.of(keys[i], self[keys[i]]);
                        i++;
                        return ret;
                    }, self, null);
                });
                thread.lua.lua_setfield(thread.address, metatableIndex, '__pairs');
                thread.pushValue((self, other) => {
                    return self === other;
                });
                thread.lua.lua_setfield(thread.address, metatableIndex, '__eq');
                thread.pushValue((self, ...args) => {
                    if (args[0] === self) {
                        args.shift();
                    }
                    return self(...args);
                });
                thread.lua.lua_setfield(thread.address, metatableIndex, '__call');
            }
            thread.lua.lua_pop(thread.address, 1);
        }
        isType(_thread, _index, type, name) {
            return type === exports.LuaType.Userdata && name === this.name;
        }
        getValue(thread, index) {
            const refUserdata = thread.lua.lua_touserdata(thread.address, index);
            const referencePointer = thread.lua.module.getValue(refUserdata, '*');
            return thread.lua.getRef(referencePointer);
        }
        pushValue(thread, decoratedValue) {
            var _a;
            const { target, options } = decoratedValue;
            if (options.proxy === undefined) {
                if (target === null || target === undefined) {
                    return false;
                }
                if (typeof target !== 'object') {
                    const isClass = typeof target === 'function' && ((_a = target.prototype) === null || _a === void 0 ? void 0 : _a.constructor) === target && target.toString().startsWith('class ');
                    if (!isClass) {
                        return false;
                    }
                }
                if (Promise.resolve(target) === target || typeof target.then === 'function') {
                    return false;
                }
            }
            else if (options.proxy === false) {
                return false;
            }
            if (options.metatable && !(options.metatable instanceof Decoration)) {
                decoratedValue.options.metatable = decorateProxy(options.metatable, { proxy: false });
                return false;
            }
            return super.pushValue(thread, decoratedValue);
        }
        close() {
            this.thread.lua.module.removeFunction(this.gcPointer);
        }
    }
    function createTypeExtension$2(thread) {
        return new ProxyTypeExtension(thread);
    }

    class TableTypeExtension extends LuaTypeExtension {
        constructor(thread) {
            super(thread, 'js_table');
        }
        close() {
        }
        isType(_thread, _index, type) {
            return type === exports.LuaType.Table;
        }
        getValue(thread, index, userdata) {
            const seenMap = userdata || new Map();
            const pointer = thread.lua.lua_topointer(thread.address, index);
            let table = seenMap.get(pointer);
            if (!table) {
                const keys = this.readTableKeys(thread, index);
                const isSequential = keys.length > 0 && keys.every((key, index) => key === String(index + 1));
                table = isSequential ? [] : {};
                seenMap.set(pointer, table);
                this.readTableValues(thread, index, seenMap, table);
            }
            return table;
        }
        pushValue(thread, { target }, userdata) {
            if (typeof target !== 'object' || target === null) {
                return false;
            }
            const seenMap = userdata || new Map();
            const existingReference = seenMap.get(target);
            if (existingReference !== undefined) {
                thread.lua.lua_rawgeti(thread.address, LUA_REGISTRYINDEX, BigInt(existingReference));
                return true;
            }
            try {
                const tableIndex = thread.getTop() + 1;
                const createTable = (arrayCount, keyCount) => {
                    thread.lua.lua_createtable(thread.address, arrayCount, keyCount);
                    const ref = thread.lua.luaL_ref(thread.address, LUA_REGISTRYINDEX);
                    seenMap.set(target, ref);
                    thread.lua.lua_rawgeti(thread.address, LUA_REGISTRYINDEX, BigInt(ref));
                };
                if (Array.isArray(target)) {
                    createTable(target.length, 0);
                    for (let i = 0; i < target.length; i++) {
                        thread.pushValue(i + 1, seenMap);
                        thread.pushValue(target[i], seenMap);
                        thread.lua.lua_settable(thread.address, tableIndex);
                    }
                }
                else {
                    createTable(0, Object.getOwnPropertyNames(target).length);
                    for (const key in target) {
                        thread.pushValue(key, seenMap);
                        thread.pushValue(target[key], seenMap);
                        thread.lua.lua_settable(thread.address, tableIndex);
                    }
                }
            }
            finally {
                if (userdata === undefined) {
                    for (const reference of seenMap.values()) {
                        thread.lua.luaL_unref(thread.address, LUA_REGISTRYINDEX, reference);
                    }
                }
            }
            return true;
        }
        readTableKeys(thread, index) {
            const keys = [];
            thread.lua.lua_pushnil(thread.address);
            while (thread.lua.lua_next(thread.address, index)) {
                const key = thread.indexToString(-2);
                keys.push(key);
                thread.pop();
            }
            return keys;
        }
        readTableValues(thread, index, seenMap, table) {
            const isArray = Array.isArray(table);
            thread.lua.lua_pushnil(thread.address);
            while (thread.lua.lua_next(thread.address, index)) {
                const key = thread.indexToString(-2);
                const value = thread.getValue(-1, undefined, seenMap);
                if (isArray) {
                    table.push(value);
                }
                else {
                    table[key] = value;
                }
                thread.pop();
            }
        }
    }
    function createTypeExtension$1(thread) {
        return new TableTypeExtension(thread);
    }

    function decorateUserdata(target) {
        return new Decoration(target, { reference: true });
    }
    class UserdataTypeExtension extends LuaTypeExtension {
        constructor(thread) {
            super(thread, 'js_userdata');
            this.gcPointer = thread.lua.module.addFunction((functionStateAddress) => {
                const userDataPointer = thread.lua.luaL_checkudata(functionStateAddress, 1, this.name);
                const referencePointer = thread.lua.module.getValue(userDataPointer, '*');
                thread.lua.unref(referencePointer);
                return exports.LuaReturn.Ok;
            }, 'ii');
            if (thread.lua.luaL_newmetatable(thread.address, this.name)) {
                const metatableIndex = thread.lua.lua_gettop(thread.address);
                thread.lua.lua_pushstring(thread.address, 'protected metatable');
                thread.lua.lua_setfield(thread.address, metatableIndex, '__metatable');
                thread.lua.lua_pushcclosure(thread.address, this.gcPointer, 0);
                thread.lua.lua_setfield(thread.address, metatableIndex, '__gc');
            }
            thread.lua.lua_pop(thread.address, 1);
        }
        isType(_thread, _index, type, name) {
            return type === exports.LuaType.Userdata && name === this.name;
        }
        getValue(thread, index) {
            const refUserdata = thread.lua.lua_touserdata(thread.address, index);
            const referencePointer = thread.lua.module.getValue(refUserdata, '*');
            return thread.lua.getRef(referencePointer);
        }
        pushValue(thread, decoratedValue) {
            if (!decoratedValue.options.reference) {
                return false;
            }
            return super.pushValue(thread, decoratedValue);
        }
        close() {
            this.thread.lua.module.removeFunction(this.gcPointer);
        }
    }
    function createTypeExtension(thread) {
        return new UserdataTypeExtension(thread);
    }

    class LuaEngine {
        constructor(cmodule, { openStandardLibs = true, injectObjects = false, enableProxy = true, traceAllocations = false, functionTimeout = undefined, } = {}) {
            this.cmodule = cmodule;
            this.global = new Global(this.cmodule, traceAllocations);
            this.global.registerTypeExtension(0, createTypeExtension$1(this.global));
            this.global.registerTypeExtension(0, createTypeExtension$5(this.global, { functionTimeout }));
            this.global.registerTypeExtension(1, createTypeExtension$3(this.global, injectObjects));
            if (injectObjects) {
                this.global.registerTypeExtension(5, createTypeExtension$4(this.global));
            }
            if (enableProxy) {
                this.global.registerTypeExtension(3, createTypeExtension$2(this.global));
            }
            else {
                this.global.registerTypeExtension(1, createTypeExtension$6(this.global, injectObjects));
            }
            this.global.registerTypeExtension(4, createTypeExtension(this.global));
            if (openStandardLibs) {
                this.cmodule.luaL_openlibs(this.global.address);
            }
        }
        doString(script) {
            return this.callByteCode((thread) => thread.loadString(script));
        }
        doFile(filename) {
            return this.callByteCode((thread) => thread.loadFile(filename));
        }
        doStringSync(script) {
            this.global.loadString(script);
            const result = this.global.runSync();
            return result[0];
        }
        doFileSync(filename) {
            this.global.loadFile(filename);
            const result = this.global.runSync();
            return result[0];
        }
        async callByteCode(loader) {
            const thread = this.global.newThread();
            const threadIndex = this.global.getTop();
            try {
                loader(thread);
                const result = await thread.run(0);
                if (result.length > 0) {
                    this.cmodule.lua_xmove(thread.address, this.global.address, result.length);
                    return this.global.getValue(this.global.getTop() - result.length + 1);
                }
                return undefined;
            }
            finally {
                this.global.remove(threadIndex);
            }
        }
    }

    var initWasmModule = (() => {
      var _scriptDir = (typeof document === 'undefined' && typeof location === 'undefined' ? require('u' + 'rl').pathToFileURL(__filename).href : typeof document === 'undefined' ? location.href : (_documentCurrentScript && _documentCurrentScript.src || new URL('index.js', document.baseURI).href));
      
      return (
    async function(moduleArg = {}) {

    var e=moduleArg,aa,ba;e.ready=new Promise((a,b)=>{aa=a;ba=b;});
    "_malloc _free _realloc _luaL_checkversion_ _luaL_getmetafield _luaL_callmeta _luaL_tolstring _luaL_argerror _luaL_typeerror _luaL_checklstring _luaL_optlstring _luaL_checknumber _luaL_optnumber _luaL_checkinteger _luaL_optinteger _luaL_checkstack _luaL_checktype _luaL_checkany _luaL_newmetatable _luaL_setmetatable _luaL_testudata _luaL_checkudata _luaL_where _luaL_fileresult _luaL_execresult _luaL_ref _luaL_unref _luaL_loadfilex _luaL_loadbufferx _luaL_loadstring _luaL_newstate _luaL_len _luaL_addgsub _luaL_gsub _luaL_setfuncs _luaL_getsubtable _luaL_traceback _luaL_requiref _luaL_buffinit _luaL_prepbuffsize _luaL_addlstring _luaL_addstring _luaL_addvalue _luaL_pushresult _luaL_pushresultsize _luaL_buffinitsize _lua_newstate _lua_close _lua_newthread _lua_resetthread _lua_atpanic _lua_version _lua_absindex _lua_gettop _lua_settop _lua_pushvalue _lua_rotate _lua_copy _lua_checkstack _lua_xmove _lua_isnumber _lua_isstring _lua_iscfunction _lua_isinteger _lua_isuserdata _lua_type _lua_typename _lua_tonumberx _lua_tointegerx _lua_toboolean _lua_tolstring _lua_rawlen _lua_tocfunction _lua_touserdata _lua_tothread _lua_topointer _lua_arith _lua_rawequal _lua_compare _lua_pushnil _lua_pushnumber _lua_pushinteger _lua_pushlstring _lua_pushstring _lua_pushcclosure _lua_pushboolean _lua_pushlightuserdata _lua_pushthread _lua_getglobal _lua_gettable _lua_getfield _lua_geti _lua_rawget _lua_rawgeti _lua_rawgetp _lua_createtable _lua_newuserdatauv _lua_getmetatable _lua_getiuservalue _lua_setglobal _lua_settable _lua_setfield _lua_seti _lua_rawset _lua_rawseti _lua_rawsetp _lua_setmetatable _lua_setiuservalue _lua_callk _lua_pcallk _lua_load _lua_dump _lua_yieldk _lua_resume _lua_status _lua_isyieldable _lua_setwarnf _lua_warning _lua_error _lua_next _lua_concat _lua_len _lua_stringtonumber _lua_getallocf _lua_setallocf _lua_toclose _lua_closeslot _lua_getstack _lua_getinfo _lua_getlocal _lua_setlocal _lua_getupvalue _lua_setupvalue _lua_upvalueid _lua_upvaluejoin _lua_sethook _lua_gethook _lua_gethookmask _lua_gethookcount _lua_setcstacklimit _luaopen_base _luaopen_coroutine _luaopen_table _luaopen_io _luaopen_os _luaopen_string _luaopen_utf8 _luaopen_math _luaopen_debug _luaopen_package _luaL_openlibs _memory ___indirect_function_table _fflush onRuntimeInitialized".split(" ").forEach(a=>{Object.getOwnPropertyDescriptor(e.ready,
    a)||Object.defineProperty(e.ready,a,{get:()=>g("You are getting "+a+" on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js"),set:()=>g("You are setting "+a+" on the Promise object, instead of the instance. Use .then() to get called back with the instance, see the MODULARIZE docs in src/settings.js")});});
    var ca=Object.assign({},e),da="./this.program",ea=(a,b)=>{throw b;},fa="object"==typeof window,k="function"==typeof importScripts,n="object"==typeof process&&"object"==typeof process.versions&&"string"==typeof process.versions.node,ha=!fa&&!n&&!k;if(e.ENVIRONMENT)throw Error("Module.ENVIRONMENT has been deprecated. To force the environment, use the ENVIRONMENT compile-time option (for example, -sENVIRONMENT=web or -sENVIRONMENT=node)");var r="",ia,ja,ka;
    if(n){if("undefined"==typeof process||!process.release||"node"!==process.release.name)throw Error("not compiled for this environment (did you build to HTML and try to run it not on the web, or set ENVIRONMENT to something - like node - and run it someplace else - like on the web?)");var la=process.versions.node,ma=la.split(".").slice(0,3);ma=1E4*ma[0]+100*ma[1]+1*ma[2].split("-")[0];if(16E4>ma)throw Error("This emscripten-generated code requires node v16.0.0 (detected v"+la+")");const {createRequire:a}=
    await import('module');var require$1=a((typeof document === 'undefined' && typeof location === 'undefined' ? require('u' + 'rl').pathToFileURL(__filename).href : typeof document === 'undefined' ? location.href : (_documentCurrentScript && _documentCurrentScript.src || new URL('index.js', document.baseURI).href))),fs=require$1("fs"),na=require$1("path");k?r=na.dirname(r)+"/":r=require$1("url").fileURLToPath(new URL("./",(typeof document === 'undefined' && typeof location === 'undefined' ? require('u' + 'rl').pathToFileURL(__filename).href : typeof document === 'undefined' ? location.href : (_documentCurrentScript && _documentCurrentScript.src || new URL('index.js', document.baseURI).href))));ia=(b,c)=>{b=oa(b)?new URL(b):na.normalize(b);return fs.readFileSync(b,c?void 0:"utf8")};ka=b=>{b=ia(b,!0);b.buffer||(b=new Uint8Array(b));u(b.buffer);return b};ja=(b,c,d,f=!0)=>{b=oa(b)?new URL(b):na.normalize(b);fs.readFile(b,f?void 0:"utf8",(h,m)=>{h?d(h):c(f?m.buffer:m);});};!e.thisProgram&&
    1<process.argv.length&&(da=process.argv[1].replace(/\\/g,"/"));process.argv.slice(2);ea=(b,c)=>{process.exitCode=b;throw c;};e.inspect=()=>"[Emscripten Module object]";}else if(ha){if("object"==typeof process&&"function"===typeof require$1||"object"==typeof window||"function"==typeof importScripts)throw Error("not compiled for this environment (did you build to HTML and try to run it not on the web, or set ENVIRONMENT to something - like node - and run it someplace else - like on the web?)");"undefined"!=
    typeof read&&(ia=read);ka=a=>{if("function"==typeof readbuffer)return new Uint8Array(readbuffer(a));a=read(a,"binary");u("object"==typeof a);return a};ja=(a,b)=>{setTimeout(()=>b(ka(a)));};"undefined"==typeof clearTimeout&&(globalThis.clearTimeout=()=>{});"undefined"==typeof setTimeout&&(globalThis.setTimeout=a=>"function"==typeof a?a():g());"function"==typeof quit&&(ea=(a,b)=>{setTimeout(()=>{if(!(b instanceof pa)){let c=b;b&&"object"==typeof b&&b.stack&&(c=[b,b.stack]);z(`exiting due to exception: ${c}`);}quit(a);});
    throw b;});"undefined"!=typeof print&&("undefined"==typeof console&&(console={}),console.log=print,console.warn=console.error="undefined"!=typeof printErr?printErr:print);}else if(fa||k){k?r=self.location.href:"undefined"!=typeof document&&document.currentScript&&(r=document.currentScript.src);_scriptDir&&(r=_scriptDir);0!==r.indexOf("blob:")?r=r.substr(0,r.replace(/[?#].*/,"").lastIndexOf("/")+1):r="";if("object"!=typeof window&&"function"!=typeof importScripts)throw Error("not compiled for this environment (did you build to HTML and try to run it not on the web, or set ENVIRONMENT to something - like node - and run it someplace else - like on the web?)");
    ia=a=>{var b=new XMLHttpRequest;b.open("GET",a,!1);b.send(null);return b.responseText};k&&(ka=a=>{var b=new XMLHttpRequest;b.open("GET",a,!1);b.responseType="arraybuffer";b.send(null);return new Uint8Array(b.response)});ja=(a,b,c)=>{var d=new XMLHttpRequest;d.open("GET",a,!0);d.responseType="arraybuffer";d.onload=()=>{200==d.status||0==d.status&&d.response?b(d.response):c();};d.onerror=c;d.send(null);};}else throw Error("environment detection error");var qa=console.log.bind(console),z=console.error.bind(console);
    Object.assign(e,ca);ca=null;C("ENVIRONMENT");C("GL_MAX_TEXTURE_IMAGE_UNITS");C("SDL_canPlayWithWebAudio");C("SDL_numSimultaneouslyQueuedBuffers");C("INITIAL_MEMORY");C("wasmMemory");C("arguments");C("buffer");C("canvas");C("doNotCaptureKeyboard");C("dynamicLibraries");C("elementPointerLock");C("extraStackTrace");C("forcedAspectRatio");C("instantiateWasm");C("keyboardListeningElement");C("freePreloadedMediaOnUse");C("loadSplitModule");C("logReadFiles");C("mainScriptUrlOrBlob");C("mem");C("monitorRunDependencies");
    C("noExitRuntime");C("noInitialRun");C("onAbort");C("onCustomMessage");C("onExit");C("onFree");C("onFullScreen");C("onMalloc");C("onRealloc");C("onRuntimeInitialized");C("postMainLoop");C("postRun");C("preInit");C("preMainLoop");C("preinitializedWebGLContext");C("memoryInitializerRequest");C("preloadPlugins");C("print");C("printErr");C("quit");C("setStatus");C("statusMessage");C("stderr");C("stdin");C("stdout");C("thisProgram");C("wasm");C("wasmBinary");C("websocket");C("fetchSettings");
    D("arguments","arguments_");D("thisProgram","thisProgram");D("quit","quit_");u("undefined"==typeof e.memoryInitializerPrefixURL,"Module.memoryInitializerPrefixURL option was removed, use Module.locateFile instead");u("undefined"==typeof e.pthreadMainPrefixURL,"Module.pthreadMainPrefixURL option was removed, use Module.locateFile instead");u("undefined"==typeof e.cdInitializerPrefixURL,"Module.cdInitializerPrefixURL option was removed, use Module.locateFile instead");
    u("undefined"==typeof e.filePackagePrefixURL,"Module.filePackagePrefixURL option was removed, use Module.locateFile instead");u("undefined"==typeof e.read,"Module.read option was removed (modify read_ in JS)");u("undefined"==typeof e.readAsync,"Module.readAsync option was removed (modify readAsync in JS)");u("undefined"==typeof e.readBinary,"Module.readBinary option was removed (modify readBinary in JS)");u("undefined"==typeof e.setWindowTitle,"Module.setWindowTitle option was removed (modify emscripten_set_window_title in JS)");
    u("undefined"==typeof e.TOTAL_MEMORY,"Module.TOTAL_MEMORY has been renamed Module.INITIAL_MEMORY");D("asm","wasmExports");D("read","read_");D("readAsync","readAsync");D("readBinary","readBinary");D("setWindowTitle","setWindowTitle");u(!ha,"shell environment detected but not enabled at build time.  Add 'shell' to `-sENVIRONMENT` to enable.");D("wasmBinary","wasmBinary");"object"!=typeof WebAssembly&&g("no native wasm support detected");var ra,sa=!1;
    function u(a,b){a||g("Assertion failed"+(b?": "+b:""));}var E,ta,ua,F,G,va,wa,xa;function ya(){var a=ra.buffer;e.HEAP8=E=new Int8Array(a);e.HEAP16=ua=new Int16Array(a);e.HEAPU8=ta=new Uint8Array(a);e.HEAPU16=new Uint16Array(a);e.HEAP32=F=new Int32Array(a);e.HEAPU32=G=new Uint32Array(a);e.HEAPF32=va=new Float32Array(a);e.HEAPF64=xa=new Float64Array(a);e.HEAP64=wa=new BigInt64Array(a);e.HEAPU64=new BigUint64Array(a);}u(!e.STACK_SIZE,"STACK_SIZE can no longer be set at runtime.  Use -sSTACK_SIZE at link time");
    u("undefined"!=typeof Int32Array&&"undefined"!==typeof Float64Array&&void 0!=Int32Array.prototype.subarray&&void 0!=Int32Array.prototype.set,"JS engine does not provide full typed array support");u(!e.wasmMemory,"Use of `wasmMemory` detected.  Use -sIMPORTED_MEMORY to define wasmMemory externally");u(!e.INITIAL_MEMORY,"Detected runtime INITIAL_MEMORY setting.  Use -sIMPORTED_MEMORY to define wasmMemory dynamically");
    function za(){if(!sa){var a=Aa();0==a&&(a+=4);var b=G[a>>2],c=G[a+4>>2];34821223==b&&2310721022==c||g(`Stack overflow! Stack cookie has been overwritten at ${Ba(a)}, expected hex dwords 0x89BACDFE and 0x2135467, but received ${Ba(c)} ${Ba(b)}`);1668509029!=G[0]&&g("Runtime error: The application has corrupted its heap memory area (address zero)!");}}var Ca=new Int16Array(1),Da=new Int8Array(Ca.buffer);Ca[0]=25459;
    if(115!==Da[0]||99!==Da[1])throw "Runtime error: expected the system to be little-endian! (Run with -sSUPPORT_BIG_ENDIAN to bypass)";var Ea=[],Fa=[],Ga=[],Ha=!1;u(Math.imul,"This browser does not support Math.imul(), build with LEGACY_VM_SUPPORT or POLYFILL_OLD_MATH_FUNCTIONS to add in a polyfill");u(Math.fround,"This browser does not support Math.fround(), build with LEGACY_VM_SUPPORT or POLYFILL_OLD_MATH_FUNCTIONS to add in a polyfill");u(Math.clz32,"This browser does not support Math.clz32(), build with LEGACY_VM_SUPPORT or POLYFILL_OLD_MATH_FUNCTIONS to add in a polyfill");
    u(Math.trunc,"This browser does not support Math.trunc(), build with LEGACY_VM_SUPPORT or POLYFILL_OLD_MATH_FUNCTIONS to add in a polyfill");var Ia=0,I=null,Ja=null,Ka={};function La(a){for(var b=a;;){if(!Ka[a])return a;a=b+Math.random();}}
    function Ma(a){Ia++;a?(u(!Ka[a]),Ka[a]=1,null===I&&"undefined"!=typeof setInterval&&(I=setInterval(()=>{if(sa)clearInterval(I),I=null;else {var b=!1,c;for(c in Ka)b||(b=!0,z("still waiting on run dependencies:")),z(`dependency: ${c}`);b&&z("(end of list)");}},1E4))):z("warning: run dependency added without ID");}function Na(a){Ia--;a?(u(Ka[a]),delete Ka[a]):z("warning: run dependency removed without ID");0==Ia&&(null!==I&&(clearInterval(I),I=null),Ja&&(a=Ja,Ja=null,a()));}
    function g(a){a="Aborted("+a+")";z(a);sa=!0;a=new WebAssembly.RuntimeError(a);ba(a);throw a;}var Oa=a=>a.startsWith("data:application/octet-stream;base64,"),oa=a=>a.startsWith("file://");function J(a){return function(){u(Ha,`native function \`${a}\` called before runtime initialization`);var b=L[a];u(b,`exported native function \`${a}\` not found`);return b.apply(null,arguments)}}var M;
    if(e.locateFile){if(M="glue.wasm",!Oa(M)){var Pa=M;M=e.locateFile?e.locateFile(Pa,r):r+Pa;}}else M=(new URL("glue.wasm",(typeof document === 'undefined' && typeof location === 'undefined' ? require('u' + 'rl').pathToFileURL(__filename).href : typeof document === 'undefined' ? location.href : (_documentCurrentScript && _documentCurrentScript.src || new URL('index.js', document.baseURI).href)))).href;function Qa(a){if(ka)return ka(a);throw "both async and sync fetching of the wasm failed";}
    function Ra(a){if(fa||k){if("function"==typeof fetch&&!oa(a))return fetch(a,{credentials:"same-origin"}).then(b=>{if(!b.ok)throw "failed to load wasm binary file at '"+a+"'";return b.arrayBuffer()}).catch(()=>Qa(a));if(ja)return new Promise((b,c)=>{ja(a,d=>b(new Uint8Array(d)),c);})}return Promise.resolve().then(()=>Qa(a))}
    function Sa(a,b,c){return Ra(a).then(d=>WebAssembly.instantiate(d,b)).then(d=>d).then(c,d=>{z(`failed to asynchronously prepare wasm: ${d}`);oa(M)&&z(`warning: Loading from a file URI (${M}) is not supported in most browsers. See https://emscripten.org/docs/getting_started/FAQ.html#how-do-i-run-a-local-webserver-for-testing-why-does-my-program-stall-in-downloading-or-preparing`);g(d);})}
    function Ta(a,b){var c=M;return "function"!=typeof WebAssembly.instantiateStreaming||Oa(c)||oa(c)||n||"function"!=typeof fetch?Sa(c,a,b):fetch(c,{credentials:"same-origin"}).then(d=>WebAssembly.instantiateStreaming(d,a).then(b,function(f){z(`wasm streaming compile failed: ${f}`);z("falling back to ArrayBuffer instantiation");return Sa(c,a,b)}))}
    function D(a,b){Object.getOwnPropertyDescriptor(e,a)||Object.defineProperty(e,a,{configurable:!0,get(){g(`\`Module.${a}\` has been replaced by \`${b}\``+" (the initial value can be provided on Module, but after startup the value is only looked for on a local variable of that name)");}});}function C(a){Object.getOwnPropertyDescriptor(e,a)&&g(`\`Module.${a}\` was supplied but \`${a}\` not included in INCOMING_MODULE_JS_API`);}
    function Ua(a){return "FS_createPath"===a||"FS_createDataFile"===a||"FS_createPreloadedFile"===a||"FS_unlink"===a||"addRunDependency"===a||"FS_createLazyFile"===a||"FS_createDevice"===a||"removeRunDependency"===a}function Va(a,b){"undefined"!==typeof globalThis&&Object.defineProperty(globalThis,a,{configurable:!0,get(){Wa(`\`${a}\` is not longer defined by emscripten. ${b}`);}});}Va("buffer","Please use HEAP8.buffer or wasmMemory.buffer");Va("asm","Please use wasmExports instead");
    function Xa(a){Object.getOwnPropertyDescriptor(e,a)||Object.defineProperty(e,a,{configurable:!0,get(){var b=`'${a}' was not exported. add it to EXPORTED_RUNTIME_METHODS (see the Emscripten FAQ)`;Ua(a)&&(b+=". Alternatively, forcing filesystem support (-sFORCE_FILESYSTEM) can export this for you");g(b);}});}function pa(a){this.name="ExitStatus";this.message=`Program terminated with exit(${a})`;this.status=a;}
    var Ba=a=>{u("number"===typeof a);return "0x"+(a>>>0).toString(16).padStart(8,"0")},Wa=a=>{Ya||={};Ya[a]||(Ya[a]=1,n&&(a="warning: "+a),z(a));},Ya,Za=(a,b)=>{for(var c=0,d=a.length-1;0<=d;d--){var f=a[d];"."===f?a.splice(d,1):".."===f?(a.splice(d,1),c++):c&&(a.splice(d,1),c--);}if(b)for(;c;c--)a.unshift("..");return a},P=a=>{var b="/"===a.charAt(0),c="/"===a.substr(-1);(a=Za(a.split("/").filter(d=>!!d),!b).join("/"))||b||(a=".");a&&c&&(a+="/");return (b?"/":"")+a},$a=a=>{var b=/^(\/?|)([\s\S]*?)((?:\.{1,2}|[^\/]+?|)(\.[^.\/]*|))(?:[\/]*)$/.exec(a).slice(1);
    a=b[0];b=b[1];if(!a&&!b)return ".";b&&=b.substr(0,b.length-1);return a+b},Q=a=>{if("/"===a)return "/";a=P(a);a=a.replace(/\/$/,"");var b=a.lastIndexOf("/");return -1===b?a:a.substr(b+1)},ab=(a,b)=>P(a+"/"+b),bb=()=>{if("object"==typeof crypto&&"function"==typeof crypto.getRandomValues)return c=>crypto.getRandomValues(c);if(n)try{var a=require$1("crypto");if(a.randomFillSync)return c=>a.randomFillSync(c);var b=a.randomBytes;return c=>(c.set(b(c.byteLength)),c)}catch(c){}g("no cryptographic support found for randomDevice. consider polyfilling it if you want to use something insecure like Math.random(), e.g. put this in a --pre-js: var crypto = { getRandomValues: (array) => { for (var i = 0; i < array.length; i++) array[i] = (Math.random()*256)|0 } };");},
    cb=a=>(cb=bb())(a);function db(){for(var a="",b=!1,c=arguments.length-1;-1<=c&&!b;c--){b=0<=c?arguments[c]:R.cwd();if("string"!=typeof b)throw new TypeError("Arguments to path.resolve must be strings");if(!b)return "";a=b+"/"+a;b="/"===b.charAt(0);}a=Za(a.split("/").filter(d=>!!d),!b).join("/");return (b?"/":"")+a||"."}
    var eb=(a,b)=>{function c(m){for(var p=0;p<m.length&&""===m[p];p++);for(var y=m.length-1;0<=y&&""===m[y];y--);return p>y?[]:m.slice(p,y-p+1)}a=db(a).substr(1);b=db(b).substr(1);a=c(a.split("/"));b=c(b.split("/"));for(var d=Math.min(a.length,b.length),f=d,h=0;h<d;h++)if(a[h]!==b[h]){f=h;break}d=[];for(h=f;h<a.length;h++)d.push("..");d=d.concat(b.slice(f));return d.join("/")},gb="undefined"!=typeof TextDecoder?new TextDecoder("utf8"):void 0,hb=(a,b)=>{for(var c=b+NaN,d=b;a[d]&&!(d>=c);)++d;if(16<d-
    b&&a.buffer&&gb)return gb.decode(a.subarray(b,d));for(c="";b<d;){var f=a[b++];if(f&128){var h=a[b++]&63;if(192==(f&224))c+=String.fromCharCode((f&31)<<6|h);else {var m=a[b++]&63;224==(f&240)?f=(f&15)<<12|h<<6|m:(240!=(f&248)&&Wa("Invalid UTF-8 leading byte "+Ba(f)+" encountered when deserializing a UTF-8 string in wasm memory to a JS string!"),f=(f&7)<<18|h<<12|m<<6|a[b++]&63);65536>f?c+=String.fromCharCode(f):(f-=65536,c+=String.fromCharCode(55296|f>>10,56320|f&1023));}}else c+=String.fromCharCode(f);}return c},
    ib=[],jb=a=>{for(var b=0,c=0;c<a.length;++c){var d=a.charCodeAt(c);127>=d?b++:2047>=d?b+=2:55296<=d&&57343>=d?(b+=4,++c):b+=3;}return b},kb=(a,b,c,d)=>{u("string"===typeof a,`stringToUTF8Array expects a string (got ${typeof a})`);if(!(0<d))return 0;var f=c;d=c+d-1;for(var h=0;h<a.length;++h){var m=a.charCodeAt(h);if(55296<=m&&57343>=m){var p=a.charCodeAt(++h);m=65536+((m&1023)<<10)|p&1023;}if(127>=m){if(c>=d)break;b[c++]=m;}else {if(2047>=m){if(c+1>=d)break;b[c++]=192|m>>6;}else {if(65535>=m){if(c+2>=d)break;
    b[c++]=224|m>>12;}else {if(c+3>=d)break;1114111<m&&Wa("Invalid Unicode code point "+Ba(m)+" encountered when serializing a JS string to a UTF-8 string in wasm memory! (Valid unicode code points should be in range 0-0x10FFFF).");b[c++]=240|m>>18;b[c++]=128|m>>12&63;}b[c++]=128|m>>6&63;}b[c++]=128|m&63;}}b[c]=0;return c-f};function lb(a,b){var c=Array(jb(a)+1);a=kb(a,c,0,c.length);b&&(c.length=a);return c}var mb=[];function nb(a,b){mb[a]={input:[],output:[],K:b};ob(a,pb);}
    var pb={open(a){var b=mb[a.node.rdev];if(!b)throw new R.g(43);a.tty=b;a.seekable=!1;},close(a){a.tty.K.fsync(a.tty);},fsync(a){a.tty.K.fsync(a.tty);},read(a,b,c,d){if(!a.tty||!a.tty.K.ra)throw new R.g(60);for(var f=0,h=0;h<d;h++){try{var m=a.tty.K.ra(a.tty);}catch(p){throw new R.g(29);}if(void 0===m&&0===f)throw new R.g(6);if(null===m||void 0===m)break;f++;b[c+h]=m;}f&&(a.node.timestamp=Date.now());return f},write(a,b,c,d){if(!a.tty||!a.tty.K.ia)throw new R.g(60);try{for(var f=0;f<d;f++)a.tty.K.ia(a.tty,
    b[c+f]);}catch(h){throw new R.g(29);}d&&(a.node.timestamp=Date.now());return f}},qb={ra(){a:{if(!ib.length){var a=null;if(n){var b=Buffer.alloc(256),c=0,d=process.stdin.fd;try{c=fs.readSync(d,b);}catch(f){if(f.toString().includes("EOF"))c=0;else throw f;}0<c?a=b.slice(0,c).toString("utf-8"):a=null;}else "undefined"!=typeof window&&"function"==typeof window.prompt?(a=window.prompt("Input: "),null!==a&&(a+="\n")):"function"==typeof readline&&(a=readline(),null!==a&&(a+="\n"));if(!a){a=null;break a}ib=lb(a,
    !0);}a=ib.shift();}return a},ia(a,b){null===b||10===b?(qa(hb(a.output,0)),a.output=[]):0!=b&&a.output.push(b);},fsync(a){a.output&&0<a.output.length&&(qa(hb(a.output,0)),a.output=[]);},Ia(){return {ab:25856,cb:5,$a:191,bb:35387,Za:[3,28,127,21,4,0,1,0,17,19,26,0,18,15,23,22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]}},Ja(){return 0},Ka(){return [24,80]}},rb={ia(a,b){null===b||10===b?(z(hb(a.output,0)),a.output=[]):0!=b&&a.output.push(b);},fsync(a){a.output&&0<a.output.length&&(z(hb(a.output,0)),a.output=[]);}},sb=
    ()=>{g("internal error: mmapAlloc called but `emscripten_builtin_memalign` native symbol not exported");};function tb(a,b){var c=a.m?a.m.length:0;c>=b||(b=Math.max(b,c*(1048576>c?2:1.125)>>>0),0!=c&&(b=Math.max(b,256)),c=a.m,a.m=new Uint8Array(b),0<a.o&&a.m.set(c.subarray(0,a.o),0));}
    var S={G:null,s(){return S.createNode(null,"/",16895,0)},createNode(a,b,c,d){if(24576===(c&61440)||R.isFIFO(c))throw new R.g(63);S.G||(S.G={dir:{node:{C:S.h.C,v:S.h.v,lookup:S.h.lookup,J:S.h.J,rename:S.h.rename,unlink:S.h.unlink,rmdir:S.h.rmdir,readdir:S.h.readdir,symlink:S.h.symlink},stream:{D:S.l.D}},file:{node:{C:S.h.C,v:S.h.v},stream:{D:S.l.D,read:S.l.read,write:S.l.write,T:S.l.T,S:S.l.S,V:S.l.V}},link:{node:{C:S.h.C,v:S.h.v,readlink:S.h.readlink},stream:{}},na:{node:{C:S.h.C,v:S.h.v},stream:R.Da}});
    c=R.createNode(a,b,c,d);T(c.mode)?(c.h=S.G.dir.node,c.l=S.G.dir.stream,c.m={}):R.isFile(c.mode)?(c.h=S.G.file.node,c.l=S.G.file.stream,c.o=0,c.m=null):40960===(c.mode&61440)?(c.h=S.G.link.node,c.l=S.G.link.stream):8192===(c.mode&61440)&&(c.h=S.G.na.node,c.l=S.G.na.stream);c.timestamp=Date.now();a&&(a.m[b]=c,a.timestamp=c.timestamp);return c},lb(a){return a.m?a.m.subarray?a.m.subarray(0,a.o):new Uint8Array(a.m):new Uint8Array(0)},h:{C(a){var b={};b.dev=8192===(a.mode&61440)?a.id:1;b.ino=a.id;b.mode=
    a.mode;b.nlink=1;b.uid=0;b.gid=0;b.rdev=a.rdev;T(a.mode)?b.size=4096:R.isFile(a.mode)?b.size=a.o:40960===(a.mode&61440)?b.size=a.link.length:b.size=0;b.atime=new Date(a.timestamp);b.mtime=new Date(a.timestamp);b.ctime=new Date(a.timestamp);b.Ba=4096;b.blocks=Math.ceil(b.size/b.Ba);return b},v(a,b){void 0!==b.mode&&(a.mode=b.mode);void 0!==b.timestamp&&(a.timestamp=b.timestamp);if(void 0!==b.size&&(b=b.size,a.o!=b))if(0==b)a.m=null,a.o=0;else {var c=a.m;a.m=new Uint8Array(b);c&&a.m.set(c.subarray(0,
    Math.min(b,a.o)));a.o=b;}},lookup(){throw R.da[44];},J(a,b,c,d){return S.createNode(a,b,c,d)},rename(a,b,c){if(T(a.mode)){try{var d=U(b,c);}catch(h){}if(d)for(var f in d.m)throw new R.g(55);}delete a.parent.m[a.name];a.parent.timestamp=Date.now();a.name=c;b.m[c]=a;b.timestamp=a.parent.timestamp;a.parent=b;},unlink(a,b){delete a.m[b];a.timestamp=Date.now();},rmdir(a,b){var c=U(a,b),d;for(d in c.m)throw new R.g(55);delete a.m[b];a.timestamp=Date.now();},readdir(a){var b=[".",".."],c;for(c in a.m)a.m.hasOwnProperty(c)&&
    b.push(c);return b},symlink(a,b,c){a=S.createNode(a,b,41471,0);a.link=c;return a},readlink(a){if(40960!==(a.mode&61440))throw new R.g(28);return a.link}},l:{read(a,b,c,d,f){var h=a.node.m;if(f>=a.node.o)return 0;a=Math.min(a.node.o-f,d);u(0<=a);if(8<a&&h.subarray)b.set(h.subarray(f,f+a),c);else for(d=0;d<a;d++)b[c+d]=h[f+d];return a},write(a,b,c,d,f,h){u(!(b instanceof ArrayBuffer));b.buffer===E.buffer&&(h=!1);if(!d)return 0;a=a.node;a.timestamp=Date.now();if(b.subarray&&(!a.m||a.m.subarray)){if(h)return u(0===
    f,"canOwn must imply no weird position inside the file"),a.m=b.subarray(c,c+d),a.o=d;if(0===a.o&&0===f)return a.m=b.slice(c,c+d),a.o=d;if(f+d<=a.o)return a.m.set(b.subarray(c,c+d),f),d}tb(a,f+d);if(a.m.subarray&&b.subarray)a.m.set(b.subarray(c,c+d),f);else for(h=0;h<d;h++)a.m[f+h]=b[c+h];a.o=Math.max(a.o,f+d);return d},D(a,b,c){1===c?b+=a.position:2===c&&R.isFile(a.node.mode)&&(b+=a.node.o);if(0>b)throw new R.g(28);return b},T(a,b,c){tb(a.node,b+c);a.node.o=Math.max(a.node.o,b+c);},S(a,b,c,d,f){if(!R.isFile(a.node.mode))throw new R.g(43);
    a=a.node.m;if(f&2||a.buffer!==E.buffer){if(0<c||c+b<a.length)a.subarray?a=a.subarray(c,c+b):a=Array.prototype.slice.call(a,c,c+b);c=!0;b=sb();if(!b)throw new R.g(48);E.set(a,b);}else c=!1,b=a.byteOffset;return {Ra:b,Aa:c}},V(a,b,c,d){S.l.write(a,b,0,d,c,!1);return 0}}},ub=(a,b,c)=>{var d=La(`al ${a}`);ja(a,f=>{u(f,`Loading data file "${a}" failed (no arrayBuffer).`);b(new Uint8Array(f));d&&Na(d);},()=>{if(c)c();else throw `Loading data file "${a}" failed.`;});d&&Ma(d);},vb=[],wb=(a,b,c,d)=>{"undefined"!=
    typeof Browser&&Browser.R();var f=!1;vb.forEach(h=>{!f&&h.canHandle(b)&&(h.handle(a,b,c,d),f=!0);});return f},xb=(a,b)=>{var c=0;a&&(c|=365);b&&(c|=146);return c},yb={0:"Success",1:"Arg list too long",2:"Permission denied",3:"Address already in use",4:"Address not available",5:"Address family not supported by protocol family",6:"No more processes",7:"Socket already connected",8:"Bad file number",9:"Trying to read unreadable message",10:"Mount device busy",11:"Operation canceled",12:"No children",13:"Connection aborted",
    14:"Connection refused",15:"Connection reset by peer",16:"File locking deadlock error",17:"Destination address required",18:"Math arg out of domain of func",19:"Quota exceeded",20:"File exists",21:"Bad address",22:"File too large",23:"Host is unreachable",24:"Identifier removed",25:"Illegal byte sequence",26:"Connection already in progress",27:"Interrupted system call",28:"Invalid argument",29:"I/O error",30:"Socket is already connected",31:"Is a directory",32:"Too many symbolic links",33:"Too many open files",
    34:"Too many links",35:"Message too long",36:"Multihop attempted",37:"File or path name too long",38:"Network interface is not configured",39:"Connection reset by network",40:"Network is unreachable",41:"Too many open files in system",42:"No buffer space available",43:"No such device",44:"No such file or directory",45:"Exec format error",46:"No record locks available",47:"The link has been severed",48:"Not enough core",49:"No message of desired type",50:"Protocol not available",51:"No space left on device",
    52:"Function not implemented",53:"Socket is not connected",54:"Not a directory",55:"Directory not empty",56:"State not recoverable",57:"Socket operation on non-socket",59:"Not a typewriter",60:"No such device or address",61:"Value too large for defined data type",62:"Previous owner died",63:"Not super-user",64:"Broken pipe",65:"Protocol error",66:"Unknown protocol",67:"Protocol wrong type for socket",68:"Math result not representable",69:"Read only file system",70:"Illegal seek",71:"No such process",
    72:"Stale file handle",73:"Connection timed out",74:"Text file busy",75:"Cross-device link",100:"Device not a stream",101:"Bad font file fmt",102:"Invalid slot",103:"Invalid request code",104:"No anode",105:"Block device required",106:"Channel number out of range",107:"Level 3 halted",108:"Level 3 reset",109:"Link number out of range",110:"Protocol driver not attached",111:"No CSI structure available",112:"Level 2 halted",113:"Invalid exchange",114:"Invalid request descriptor",115:"Exchange full",
    116:"No data (for no delay io)",117:"Timer expired",118:"Out of streams resources",119:"Machine is not on the network",120:"Package not installed",121:"The object is remote",122:"Advertise error",123:"Srmount error",124:"Communication error on send",125:"Cross mount point (not really error)",126:"Given log. name not unique",127:"f.d. invalid for this operation",128:"Remote address changed",129:"Can   access a needed shared lib",130:"Accessing a corrupted shared lib",131:".lib section in a.out corrupted",
    132:"Attempting to link in too many libs",133:"Attempting to exec a shared library",135:"Streams pipe error",136:"Too many users",137:"Socket type not supported",138:"Not supported",139:"Protocol family not supported",140:"Can't send after socket shutdown",141:"Too many references",142:"Host is down",148:"No medium (in tape drive)",156:"Level 2 not synchronized"},zb={EPERM:63,ENOENT:44,ESRCH:71,EINTR:27,EIO:29,ENXIO:60,E2BIG:1,ENOEXEC:45,EBADF:8,ECHILD:12,EAGAIN:6,EWOULDBLOCK:6,ENOMEM:48,EACCES:2,
    EFAULT:21,ENOTBLK:105,EBUSY:10,EEXIST:20,EXDEV:75,ENODEV:43,ENOTDIR:54,EISDIR:31,EINVAL:28,ENFILE:41,EMFILE:33,ENOTTY:59,ETXTBSY:74,EFBIG:22,ENOSPC:51,ESPIPE:70,EROFS:69,EMLINK:34,EPIPE:64,EDOM:18,ERANGE:68,ENOMSG:49,EIDRM:24,ECHRNG:106,EL2NSYNC:156,EL3HLT:107,EL3RST:108,ELNRNG:109,EUNATCH:110,ENOCSI:111,EL2HLT:112,EDEADLK:16,ENOLCK:46,EBADE:113,EBADR:114,EXFULL:115,ENOANO:104,EBADRQC:103,EBADSLT:102,EDEADLOCK:16,EBFONT:101,ENOSTR:100,ENODATA:116,ETIME:117,ENOSR:118,ENONET:119,ENOPKG:120,EREMOTE:121,
    ENOLINK:47,EADV:122,ESRMNT:123,ECOMM:124,EPROTO:65,EMULTIHOP:36,EDOTDOT:125,EBADMSG:9,ENOTUNIQ:126,EBADFD:127,EREMCHG:128,ELIBACC:129,ELIBBAD:130,ELIBSCN:131,ELIBMAX:132,ELIBEXEC:133,ENOSYS:52,ENOTEMPTY:55,ENAMETOOLONG:37,ELOOP:32,EOPNOTSUPP:138,EPFNOSUPPORT:139,ECONNRESET:15,ENOBUFS:42,EAFNOSUPPORT:5,EPROTOTYPE:67,ENOTSOCK:57,ENOPROTOOPT:50,ESHUTDOWN:140,ECONNREFUSED:14,EADDRINUSE:3,ECONNABORTED:13,ENETUNREACH:40,ENETDOWN:38,ETIMEDOUT:73,EHOSTDOWN:142,EHOSTUNREACH:23,EINPROGRESS:26,EALREADY:7,EDESTADDRREQ:17,
    EMSGSIZE:35,EPROTONOSUPPORT:66,ESOCKTNOSUPPORT:137,EADDRNOTAVAIL:4,ENETRESET:39,EISCONN:30,ENOTCONN:53,ETOOMANYREFS:141,EUSERS:136,EDQUOT:19,ESTALE:72,ENOTSUP:138,ENOMEDIUM:148,EILSEQ:25,EOVERFLOW:61,ECANCELED:11,ENOTRECOVERABLE:56,EOWNERDEAD:62,ESTRPIPE:135},Ab=a=>a.replace(/\b_Z[\w\d_]+/g,function(b){Wa("warning: build with -sDEMANGLE_SUPPORT to link in libcxxabi demangling");return b===b?b:b+" ["+b+"]"});function ob(a,b){R.pa[a]={l:b};}function T(a){return 16384===(a&61440)}
    function U(a,b){var c;if(c=(c=Bb(a,"x"))?c:a.h.lookup?0:2)throw new R.g(c,a);for(c=R.F[Cb(a.id,b)];c;c=c.N){var d=c.name;if(c.parent.id===a.id&&d===b)return c}return R.lookup(a,b)}
    function V(a,b={}){a=db(a);if(!a)return {path:"",node:null};b=Object.assign({ba:!0,ka:0},b);if(8<b.ka)throw new R.g(32);a=a.split("/").filter(m=>!!m);for(var c=R.root,d="/",f=0;f<a.length;f++){var h=f===a.length-1;if(h&&b.parent)break;c=U(c,a[f]);d=P(d+"/"+a[f]);c.A&&(!h||h&&b.ba)&&(c=c.A.root);if(!h||b.B)for(h=0;40960===(c.mode&61440);)if(c=R.readlink(d),d=db($a(d),c),c=V(d,{ka:b.ka+1}).node,40<h++)throw new R.g(32);}return {path:d,node:c}}
    function Db(a){for(var b;;){if(R.Z(a))return a=a.s.ua,b?"/"!==a[a.length-1]?`${a}/${b}`:a+b:a;b=b?`${a.name}/${b}`:a.name;a=a.parent;}}function Cb(a,b){for(var c=0,d=0;d<b.length;d++)c=(c<<5)-c+b.charCodeAt(d)|0;return (a+c>>>0)%R.F.length}function Eb(a){var b=Cb(a.parent.id,a.name);a.N=R.F[b];R.F[b]=a;}function Fb(a){var b=Cb(a.parent.id,a.name);if(R.F[b]===a)R.F[b]=a.N;else for(b=R.F[b];b;){if(b.N===a){b.N=a.N;break}b=b.N;}}function Gb(a){var b=["r","w","rw"][a&3];a&512&&(b+="w");return b}
    function Bb(a,b){if(R.ta)return 0;if(!b.includes("r")||a.mode&292){if(b.includes("w")&&!(a.mode&146)||b.includes("x")&&!(a.mode&73))return 2}else return 2;return 0}function Hb(a,b){try{return U(a,b),20}catch(c){}return Bb(a,"wx")}function Ib(a,b,c){try{var d=U(a,b);}catch(f){return f.u}if(a=Bb(a,"wx"))return a;if(c){if(!T(d.mode))return 54;if(R.Z(d)||Db(d)===R.cwd())return 10}else if(T(d.mode))return 31;return 0}function Jb(){for(var a=0;a<=R.xa;a++)if(!R.streams[a])return a;throw new R.g(33);}
    function W(a){a=R.qa(a);if(!a)throw new R.g(8);return a}function Kb(a,b=-1){R.X||(R.X=function(){this.I={};},R.X.prototype={},Object.defineProperties(R.X.prototype,{object:{get(){return this.node},set(c){this.node=c;}},flags:{get(){return this.I.flags},set(c){this.I.flags=c;}},position:{get(){return this.I.position},set(c){this.I.position=c;}}}));a=Object.assign(new R.X,a);-1==b&&(b=Jb());a.fd=b;return R.streams[b]=a}
    function Lb(a){var b=[];for(a=[a];a.length;){var c=a.pop();b.push(c);a.push.apply(a,c.U);}return b}function Mb(a,b,c){"undefined"==typeof c&&(c=b,b=438);return R.J(a,b|8192,c)}
    function Nb(){R.g||(R.g=function(a,b){this.name="ErrnoError";this.node=b;this.Sa=function(c){this.u=c;for(var d in zb)if(zb[d]===c){this.code=d;break}};this.Sa(a);this.message=yb[a];this.stack&&(Object.defineProperty(this,"stack",{value:Error().stack,writable:!0}),this.stack=Ab(this.stack));},R.g.prototype=Error(),R.g.prototype.constructor=R.g,[44].forEach(a=>{R.da[a]=new R.g(a);R.da[a].stack="<generic error, no stack>";}));}
    function Ob(a,b){try{var c=V(a,{B:!b});a=c.path;}catch(f){}var d={Z:!1,exists:!1,error:0,name:null,path:null,object:null,Oa:!1,Qa:null,Pa:null};try{c=V(a,{parent:!0}),d.Oa=!0,d.Qa=c.path,d.Pa=c.node,d.name=Q(a),c=V(a,{B:!b}),d.exists=!0,d.path=c.path,d.object=c.node,d.name=c.node.name,d.Z="/"===c.path;}catch(f){d.error=f.u;}return d}function Pb(a,b,c,d){a="string"==typeof a?a:Db(a);b=P(a+"/"+b);return R.create(b,xb(c,d))}
    function Qb(a){if(!(a.La||a.Ma||a.link||a.m)){if("undefined"!=typeof XMLHttpRequest)throw Error("Lazy loading should have been performed (contents set) in createLazyFile, but it was not. Lazy loading only works in web workers. Use --embed-file or --preload-file in emcc on the main thread.");if(ia)try{a.m=lb(ia(a.url),!0),a.o=a.m.length;}catch(b){throw new R.g(29);}else throw Error("Cannot load without read() or XMLHttpRequest.");}}
    var R={root:null,U:[],pa:{},streams:[],Na:1,F:null,oa:"/",Y:!1,ta:!0,g:null,da:{},Fa:null,W:0,createNode(a,b,c,d){u("object"==typeof a);a=new R.wa(a,b,c,d);Eb(a);return a},Z(a){return a===a.parent},isFile(a){return 32768===(a&61440)},isFIFO(a){return 4096===(a&61440)},isSocket(a){return 49152===(a&49152)},xa:4096,qa:a=>R.streams[a],Da:{open(a){a.l=R.Ga(a.node.rdev).l;a.l.open&&a.l.open(a);},D(){throw new R.g(70);}},ha:a=>a>>8,nb:a=>a&255,M:(a,b)=>a<<8|b,Ga:a=>R.pa[a],va(a,b){function c(m){u(0<R.W);
    R.W--;return b(m)}function d(m){if(m){if(!d.Ea)return d.Ea=!0,c(m)}else ++h>=f.length&&c(null);}"function"==typeof a&&(b=a,a=!1);R.W++;1<R.W&&z(`warning: ${R.W} FS.syncfs operations in flight at once, probably just doing extra work`);var f=Lb(R.root.s),h=0;f.forEach(m=>{if(!m.type.va)return d(null);m.type.va(m,a,d);});},s(a,b,c){if("string"==typeof a)throw a;var d="/"===c,f=!c;if(d&&R.root)throw new R.g(10);if(!d&&!f){var h=V(c,{ba:!1});c=h.path;h=h.node;if(h.A)throw new R.g(10);if(!T(h.mode))throw new R.g(54);
    }b={type:a,rb:b,ua:c,U:[]};a=a.s(b);a.s=b;b.root=a;d?R.root=a:h&&(h.A=b,h.s&&h.s.U.push(b));return a},xb(a){a=V(a,{ba:!1});if(!a.node.A)throw new R.g(28);a=a.node;var b=a.A,c=Lb(b);Object.keys(R.F).forEach(d=>{for(d=R.F[d];d;){var f=d.N;c.includes(d.s)&&Fb(d);d=f;}});a.A=null;b=a.s.U.indexOf(b);u(-1!==b);a.s.U.splice(b,1);},lookup(a,b){return a.h.lookup(a,b)},J(a,b,c){var d=V(a,{parent:!0}).node;a=Q(a);if(!a||"."===a||".."===a)throw new R.g(28);var f=Hb(d,a);if(f)throw new R.g(f);if(!d.h.J)throw new R.g(63);
    return d.h.J(d,a,b,c)},create(a,b){return R.J(a,(void 0!==b?b:438)&4095|32768,0)},mkdir(a,b){return R.J(a,(void 0!==b?b:511)&1023|16384,0)},ob(a,b){a=a.split("/");for(var c="",d=0;d<a.length;++d)if(a[d]){c+="/"+a[d];try{R.mkdir(c,b);}catch(f){if(20!=f.u)throw f;}}},symlink(a,b){if(!db(a))throw new R.g(44);var c=V(b,{parent:!0}).node;if(!c)throw new R.g(44);b=Q(b);var d=Hb(c,b);if(d)throw new R.g(d);if(!c.h.symlink)throw new R.g(63);return c.h.symlink(c,b,a)},rename(a,b){var c=$a(a),d=$a(b),f=Q(a),
    h=Q(b);var m=V(a,{parent:!0});var p=m.node;m=V(b,{parent:!0});m=m.node;if(!p||!m)throw new R.g(44);if(p.s!==m.s)throw new R.g(75);var y=U(p,f);a=eb(a,d);if("."!==a.charAt(0))throw new R.g(28);a=eb(b,c);if("."!==a.charAt(0))throw new R.g(55);try{var q=U(m,h);}catch(x){}if(y!==q){b=T(y.mode);if(f=Ib(p,f,b))throw new R.g(f);if(f=q?Ib(m,h,b):Hb(m,h))throw new R.g(f);if(!p.h.rename)throw new R.g(63);if(y.A||q&&q.A)throw new R.g(10);if(m!==p&&(f=Bb(p,"w")))throw new R.g(f);Fb(y);try{p.h.rename(y,m,h);}catch(x){throw x;
    }finally{Eb(y);}}},rmdir(a){var b=V(a,{parent:!0}).node;a=Q(a);var c=U(b,a),d=Ib(b,a,!0);if(d)throw new R.g(d);if(!b.h.rmdir)throw new R.g(63);if(c.A)throw new R.g(10);b.h.rmdir(b,a);Fb(c);},readdir(a){a=V(a,{B:!0}).node;if(!a.h.readdir)throw new R.g(54);return a.h.readdir(a)},unlink(a){var b=V(a,{parent:!0}).node;if(!b)throw new R.g(44);a=Q(a);var c=U(b,a),d=Ib(b,a,!1);if(d)throw new R.g(d);if(!b.h.unlink)throw new R.g(63);if(c.A)throw new R.g(10);b.h.unlink(b,a);Fb(c);},readlink(a){a=V(a).node;if(!a)throw new R.g(44);
    if(!a.h.readlink)throw new R.g(28);return db(Db(a.parent),a.h.readlink(a))},stat(a,b){a=V(a,{B:!b}).node;if(!a)throw new R.g(44);if(!a.h.C)throw new R.g(63);return a.h.C(a)},lstat(a){return R.stat(a,!0)},chmod(a,b,c){a="string"==typeof a?V(a,{B:!c}).node:a;if(!a.h.v)throw new R.g(63);a.h.v(a,{mode:b&4095|a.mode&-4096,timestamp:Date.now()});},lchmod(a,b){R.chmod(a,b,!0);},fchmod(a,b){a=W(a);R.chmod(a.node,b);},chown(a,b,c,d){a="string"==typeof a?V(a,{B:!d}).node:a;if(!a.h.v)throw new R.g(63);a.h.v(a,
    {timestamp:Date.now()});},lchown(a,b,c){R.chown(a,b,c,!0);},fchown(a,b,c){a=W(a);R.chown(a.node,b,c);},truncate(a,b){if(0>b)throw new R.g(28);a="string"==typeof a?V(a,{B:!0}).node:a;if(!a.h.v)throw new R.g(63);if(T(a.mode))throw new R.g(31);if(!R.isFile(a.mode))throw new R.g(28);var c=Bb(a,"w");if(c)throw new R.g(c);a.h.v(a,{size:b,timestamp:Date.now()});},kb(a,b){a=W(a);if(0===(a.flags&2097155))throw new R.g(28);R.truncate(a.node,b);},yb(a,b,c){a=V(a,{B:!0}).node;a.h.v(a,{timestamp:Math.max(b,c)});},open(a,
    b,c){if(""===a)throw new R.g(44);if("string"==typeof b){var d={r:0,"r+":2,w:577,"w+":578,a:1089,"a+":1090}[b];if("undefined"==typeof d)throw Error(`Unknown file open mode: ${b}`);b=d;}c=b&64?("undefined"==typeof c?438:c)&4095|32768:0;if("object"==typeof a)var f=a;else {a=P(a);try{f=V(a,{B:!(b&131072)}).node;}catch(h){}}d=!1;if(b&64)if(f){if(b&128)throw new R.g(20);}else f=R.J(a,c,0),d=!0;if(!f)throw new R.g(44);8192===(f.mode&61440)&&(b&=-513);if(b&65536&&!T(f.mode))throw new R.g(54);if(!d&&(c=f?40960===
    (f.mode&61440)?32:T(f.mode)&&("r"!==Gb(b)||b&512)?31:Bb(f,Gb(b)):44))throw new R.g(c);b&512&&!d&&R.truncate(f,0);b&=-131713;f=Kb({node:f,path:Db(f),flags:b,seekable:!0,position:0,l:f.l,Xa:[],error:!1});f.l.open&&f.l.open(f);!e.logReadFiles||b&1||(R.ja||(R.ja={}),a in R.ja||(R.ja[a]=1));return f},close(a){if(null===a.fd)throw new R.g(8);a.ea&&(a.ea=null);try{a.l.close&&a.l.close(a);}catch(b){throw b;}finally{R.streams[a.fd]=null;}a.fd=null;},D(a,b,c){if(null===a.fd)throw new R.g(8);if(!a.seekable||!a.l.D)throw new R.g(70);
    if(0!=c&&1!=c&&2!=c)throw new R.g(28);a.position=a.l.D(a,b,c);a.Xa=[];return a.position},read(a,b,c,d,f){u(0<=c);if(0>d||0>f)throw new R.g(28);if(null===a.fd)throw new R.g(8);if(1===(a.flags&2097155))throw new R.g(8);if(T(a.node.mode))throw new R.g(31);if(!a.l.read)throw new R.g(28);var h="undefined"!=typeof f;if(!h)f=a.position;else if(!a.seekable)throw new R.g(70);b=a.l.read(a,b,c,d,f);h||(a.position+=b);return b},write(a,b,c,d,f,h){u(0<=c);if(0>d||0>f)throw new R.g(28);if(null===a.fd)throw new R.g(8);
    if(0===(a.flags&2097155))throw new R.g(8);if(T(a.node.mode))throw new R.g(31);if(!a.l.write)throw new R.g(28);a.seekable&&a.flags&1024&&R.D(a,0,2);var m="undefined"!=typeof f;if(!m)f=a.position;else if(!a.seekable)throw new R.g(70);b=a.l.write(a,b,c,d,f,h);m||(a.position+=b);return b},T(a,b,c){if(null===a.fd)throw new R.g(8);if(0>b||0>=c)throw new R.g(28);if(0===(a.flags&2097155))throw new R.g(8);if(!R.isFile(a.node.mode)&&!T(a.node.mode))throw new R.g(43);if(!a.l.T)throw new R.g(138);a.l.T(a,b,c);},
    S(a,b,c,d,f){if(0!==(d&2)&&0===(f&2)&&2!==(a.flags&2097155))throw new R.g(2);if(1===(a.flags&2097155))throw new R.g(2);if(!a.l.S)throw new R.g(43);return a.l.S(a,b,c,d,f)},V(a,b,c,d,f){u(0<=c);return a.l.V?a.l.V(a,b,c,d,f):0},qb:()=>0,fa(a,b,c){if(!a.l.fa)throw new R.g(59);return a.l.fa(a,b,c)},readFile(a,b={}){b.flags=b.flags||0;b.encoding=b.encoding||"binary";if("utf8"!==b.encoding&&"binary"!==b.encoding)throw Error(`Invalid encoding type "${b.encoding}"`);var c,d=R.open(a,b.flags);a=R.stat(a).size;
    var f=new Uint8Array(a);R.read(d,f,0,a,0);"utf8"===b.encoding?c=hb(f,0):"binary"===b.encoding&&(c=f);R.close(d);return c},writeFile(a,b,c={}){c.flags=c.flags||577;a=R.open(a,c.flags,c.mode);if("string"==typeof b){var d=new Uint8Array(jb(b)+1);b=kb(b,d,0,d.length);R.write(a,d,0,b,void 0,c.Ca);}else if(ArrayBuffer.isView(b))R.write(a,b,0,b.byteLength,void 0,c.Ca);else throw Error("Unsupported data type");R.close(a);},cwd:()=>R.oa,chdir(a){a=V(a,{B:!0});if(null===a.node)throw new R.g(44);if(!T(a.node.mode))throw new R.g(54);
    var b=Bb(a.node,"x");if(b)throw new R.g(b);R.oa=a.path;},R(a,b,c){u(!R.R.Y,"FS.init was previously called. If you want to initialize later with custom parameters, remove any earlier calls (note that one is automatically added to the generated code)");R.R.Y=!0;Nb();e.stdin=a||e.stdin;e.stdout=b||e.stdout;e.stderr=c||e.stderr;e.stdin?R.L("/dev","stdin",e.stdin):R.symlink("/dev/tty","/dev/stdin");e.stdout?R.L("/dev","stdout",null,e.stdout):R.symlink("/dev/tty","/dev/stdout");e.stderr?R.L("/dev","stderr",
    null,e.stderr):R.symlink("/dev/tty1","/dev/stderr");a=R.open("/dev/stdin",0);b=R.open("/dev/stdout",1);c=R.open("/dev/stderr",1);u(0===a.fd,`invalid handle for stdin (${a.fd})`);u(1===b.fd,`invalid handle for stdout (${b.fd})`);u(2===c.fd,`invalid handle for stderr (${c.fd})`);},sb(){R.R.Y=!1;Rb(0);for(var a=0;a<R.streams.length;a++){var b=R.streams[a];b&&R.close(b);}},jb(a,b){a=Ob(a,b);return a.exists?a.object:null},hb(a,b){a="string"==typeof a?a:Db(a);for(b=b.split("/").reverse();b.length;){var c=
    b.pop();if(c){var d=P(a+"/"+c);try{R.mkdir(d);}catch(f){}a=d;}}return d},L(a,b,c,d){a=ab("string"==typeof a?a:Db(a),b);b=xb(!!c,!!d);R.L.ha||(R.L.ha=64);var f=R.M(R.L.ha++,0);ob(f,{open(h){h.seekable=!1;},close(){d&&d.buffer&&d.buffer.length&&d(10);},read(h,m,p,y){for(var q=0,x=0;x<y;x++){try{var t=c();}catch(B){throw new R.g(29);}if(void 0===t&&0===q)throw new R.g(6);if(null===t||void 0===t)break;q++;m[p+x]=t;}q&&(h.node.timestamp=Date.now());return q},write(h,m,p,y){for(var q=0;q<y;q++)try{d(m[p+q]);}catch(x){throw new R.g(29);
    }y&&(h.node.timestamp=Date.now());return q}});return Mb(a,b,f)},fb(a,b,c,d,f){function h(){this.ga=!1;this.I=[];}function m(t,B,l,w,v){t=t.node.m;if(v>=t.length)return 0;w=Math.min(t.length-v,w);u(0<=w);if(t.slice)for(var A=0;A<w;A++)B[l+A]=t[v+A];else for(A=0;A<w;A++)B[l+A]=t.get(v+A);return w}h.prototype.get=function(t){if(!(t>this.length-1||0>t)){var B=t%this.chunkSize;return this.sa(t/this.chunkSize|0)[B]}};h.prototype.Ha=function(t){this.sa=t;};h.prototype.ma=function(){var t=new XMLHttpRequest;
    t.open("HEAD",c,!1);t.send(null);if(!(200<=t.status&&300>t.status||304===t.status))throw Error("Couldn't load "+c+". Status: "+t.status);var B=Number(t.getResponseHeader("Content-length")),l,w=(l=t.getResponseHeader("Accept-Ranges"))&&"bytes"===l;t=(l=t.getResponseHeader("Content-Encoding"))&&"gzip"===l;var v=1048576;w||(v=B);var A=this;A.Ha(H=>{var N=H*v,O=(H+1)*v-1;O=Math.min(O,B-1);if("undefined"==typeof A.I[H]){var fb=A.I;if(N>O)throw Error("invalid range ("+N+", "+O+") or no bytes requested!");
    if(O>B-1)throw Error("only "+B+" bytes available! programmer error!");var K=new XMLHttpRequest;K.open("GET",c,!1);B!==v&&K.setRequestHeader("Range","bytes="+N+"-"+O);K.responseType="arraybuffer";K.overrideMimeType&&K.overrideMimeType("text/plain; charset=x-user-defined");K.send(null);if(!(200<=K.status&&300>K.status||304===K.status))throw Error("Couldn't load "+c+". Status: "+K.status);N=void 0!==K.response?new Uint8Array(K.response||[]):lb(K.responseText||"",!0);fb[H]=N;}if("undefined"==typeof A.I[H])throw Error("doXHR failed!");
    return A.I[H]});if(t||!B)v=B=1,v=B=this.sa(0).length,qa("LazyFiles on gzip forces download of the whole file when length is accessed");this.za=B;this.ya=v;this.ga=!0;};if("undefined"!=typeof XMLHttpRequest){if(!k)throw "Cannot do synchronous binary XHRs outside webworkers in modern browsers. Use --embed-file or --preload-file in emcc";var p=new h;Object.defineProperties(p,{length:{get:function(){this.ga||this.ma();return this.za}},chunkSize:{get:function(){this.ga||this.ma();return this.ya}}});var y=
    void 0;}else y=c,p=void 0;var q=Pb(a,b,d,f);p?q.m=p:y&&(q.m=null,q.url=y);Object.defineProperties(q,{o:{get:function(){return this.m.length}}});var x={};Object.keys(q.l).forEach(t=>{var B=q.l[t];x[t]=function(){Qb(q);return B.apply(null,arguments)};});x.read=(t,B,l,w,v)=>{Qb(q);return m(t,B,l,w,v)};x.S=(t,B,l)=>{Qb(q);var w=sb();if(!w)throw new R.g(48);m(t,E,w,B,l);return {Ra:w,Aa:!0}};q.l=x;return q},Ya(){g("FS.absolutePath has been removed; use PATH_FS.resolve instead");},eb(){g("FS.createFolder has been removed; use FS.mkdir instead");},
    gb(){g("FS.createLink has been removed; use FS.symlink instead");},mb(){g("FS.joinPath has been removed; use PATH.join instead");},pb(){g("FS.mmapAlloc has been replaced by the top level function mmapAlloc");},vb(){g("FS.standardizePath has been removed; use PATH.normalize instead");}},X=a=>{u("number"==typeof a,`UTF8ToString expects a number (got ${typeof a})`);return a?hb(ta,a):""};
    function Sb(a,b){if("/"===b.charAt(0))return b;a=-100===a?R.cwd():W(a).path;if(0==b.length)throw new R.g(44);return P(a+"/"+b)}var Tb=void 0;function Y(){u(void 0!=Tb);var a=F[+Tb>>2];Tb+=4;return a}
    var Ub=(a,b,c)=>{u("number"==typeof c,"stringToUTF8(str, outPtr, maxBytesToWrite) is missing the third parameter that specifies the length of the output buffer!");return kb(a,ta,b,c)},Vb=a=>0===a%4&&(0!==a%100||0===a%400),Wb=[0,31,60,91,121,152,182,213,244,274,305,335],Xb=[0,31,59,90,120,151,181,212,243,273,304,334],Zb=a=>{var b=jb(a)+1,c=Yb(b);c&&Ub(a,c,b);return c},$b={},bc=()=>{if(!ac){var a={USER:"web_user",LOGNAME:"web_user",PATH:"/",PWD:"/",HOME:"/home/web_user",LANG:("object"==typeof navigator&&
    navigator.languages&&navigator.languages[0]||"C").replace("-","_")+".UTF-8",_:da||"./this.program"},b;for(b in $b)void 0===$b[b]?delete a[b]:a[b]=$b[b];var c=[];for(b in a)c.push(`${b}=${a[b]}`);ac=c;}return ac},ac,cc=[31,29,31,30,31,30,31,31,30,31,30,31],dc=[31,28,31,30,31,30,31,31,30,31,30,31],ec=(a,b)=>{u(0<=a.length,"writeArrayToMemory array must have a length (should be an array or typed array)");E.set(a,b);},fc=[],Z,gc=a=>{var b=fc[a];b||(a>=fc.length&&(fc.length=a+1),fc[a]=b=Z.get(a));u(Z.get(a)==
    b,"JavaScript-side Wasm function table mirror is out of date!");return b},hc=a=>{var b=e["_"+a];u(b,"Cannot call unknown function "+a+", make sure it is exported");return b},ic,jc=[];function kc(a,b,c,d){a||=this;this.parent=a;this.s=a.s;this.A=null;this.id=R.Na++;this.name=b;this.mode=c;this.h={};this.l={};this.rdev=d;}
    Object.defineProperties(kc.prototype,{read:{get:function(){return 365===(this.mode&365)},set:function(a){a?this.mode|=365:this.mode&=-366;}},write:{get:function(){return 146===(this.mode&146)},set:function(a){a?this.mode|=146:this.mode&=-147;}},Ma:{get:function(){return T(this.mode)}},La:{get:function(){return 8192===(this.mode&61440)}}});R.wa=kc;
    R.ib=(a,b,c,d,f,h,m,p,y,q)=>{function x(l){function w(v){q&&q();if(!p){var A=a,H=b;A&&(A="string"==typeof A?A:Db(A),H=b?P(A+"/"+b):A);A=xb(d,f);H=R.create(H,A);if(v){if("string"==typeof v){for(var N=Array(v.length),O=0,fb=v.length;O<fb;++O)N[O]=v.charCodeAt(O);v=N;}R.chmod(H,A|146);N=R.open(H,577);R.write(N,v,0,v.length,0,y);R.close(N);R.chmod(H,A);}}h&&h();Na(B);}wb(l,t,w,()=>{m&&m();Na(B);})||w(l);}var t=b?db(P(a+"/"+b)):a,B=La(`cp ${t}`);Ma(B);"string"==typeof c?ub(c,l=>x(l),m):x(c);};Nb();R.F=Array(4096);
    R.s(S,{},"/");R.mkdir("/tmp");R.mkdir("/home");R.mkdir("/home/web_user");(function(){R.mkdir("/dev");ob(R.M(1,3),{read:()=>0,write:(d,f,h,m)=>m});Mb("/dev/null",R.M(1,3));nb(R.M(5,0),qb);nb(R.M(6,0),rb);Mb("/dev/tty",R.M(5,0));Mb("/dev/tty1",R.M(6,0));var a=new Uint8Array(1024),b=0,c=()=>{0===b&&(b=cb(a).byteLength);return a[--b]};R.L("/dev","random",c);R.L("/dev","urandom",c);R.mkdir("/dev/shm");R.mkdir("/dev/shm/tmp");})();
    (function(){R.mkdir("/proc");var a=R.mkdir("/proc/self");R.mkdir("/proc/self/fd");R.s({s(){var b=R.createNode(a,"fd",16895,73);b.h={lookup(c,d){var f=W(+d);c={parent:null,s:{ua:"fake"},h:{readlink:()=>f.path}};return c.parent=c}};return b}},{},"/proc/self/fd");})();R.Fa={MEMFS:S};
    var oc={__syscall_dup3:function(a,b,c){try{var d=W(a);u(!c);if(d.fd===b)return -28;var f=R.qa(b);f&&R.close(f);return Kb(d,b).fd}catch(h){if("undefined"==typeof R||"ErrnoError"!==h.name)throw h;return -h.u}},__syscall_fcntl64:function(a,b,c){Tb=c;try{var d=W(a);switch(b){case 0:var f=Y();if(0>f)return -28;for(;R.streams[f];)f++;return Kb(d,f).fd;case 1:case 2:return 0;case 3:return d.flags;case 4:return f=Y(),d.flags|=f,0;case 5:return f=Y(),ua[f+0>>1]=2,0;case 6:case 7:return 0;case 16:case 8:return -28;
    case 9:return F[lc()>>2]=28,-1;default:return -28}}catch(h){if("undefined"==typeof R||"ErrnoError"!==h.name)throw h;return -h.u}},__syscall_ioctl:function(a,b,c){Tb=c;try{var d=W(a);switch(b){case 21509:return d.tty?0:-59;case 21505:if(!d.tty)return -59;if(d.tty.K.Ia){a=[3,28,127,21,4,0,1,0,17,19,26,0,18,15,23,22,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];var f=Y();F[f>>2]=25856;F[f+4>>2]=5;F[f+8>>2]=191;F[f+12>>2]=35387;for(var h=0;32>h;h++)E[f+h+17>>0]=a[h]||0;}return 0;case 21510:case 21511:case 21512:return d.tty?
    0:-59;case 21506:case 21507:case 21508:if(!d.tty)return -59;if(d.tty.K.Ja)for(f=Y(),a=[],h=0;32>h;h++)a.push(E[f+h+17>>0]);return 0;case 21519:if(!d.tty)return -59;f=Y();return F[f>>2]=0;case 21520:return d.tty?-28:-59;case 21531:return f=Y(),R.fa(d,b,f);case 21523:if(!d.tty)return -59;d.tty.K.Ka&&(h=[24,80],f=Y(),ua[f>>1]=h[0],ua[f+2>>1]=h[1]);return 0;case 21524:return d.tty?0:-59;case 21515:return d.tty?0:-59;default:return -28}}catch(m){if("undefined"==typeof R||"ErrnoError"!==m.name)throw m;return -m.u}},
    __syscall_openat:function(a,b,c,d){Tb=d;try{b=X(b);b=Sb(a,b);var f=d?Y():0;return R.open(b,c,f).fd}catch(h){if("undefined"==typeof R||"ErrnoError"!==h.name)throw h;return -h.u}},__syscall_readlinkat:function(a,b,c,d){try{b=X(b);b=Sb(a,b);if(0>=d)return -28;var f=R.readlink(b),h=Math.min(d,jb(f)),m=E[c+h];Ub(f,c,d+1);E[c+h]=m;return h}catch(p){if("undefined"==typeof R||"ErrnoError"!==p.name)throw p;return -p.u}},__syscall_renameat:function(a,b,c,d){try{return b=X(b),d=X(d),b=Sb(a,b),d=Sb(c,d),R.rename(b,
    d),0}catch(f){if("undefined"==typeof R||"ErrnoError"!==f.name)throw f;return -f.u}},__syscall_rmdir:function(a){try{return a=X(a),R.rmdir(a),0}catch(b){if("undefined"==typeof R||"ErrnoError"!==b.name)throw b;return -b.u}},__syscall_unlinkat:function(a,b,c){try{return b=X(b),b=Sb(a,b),0===c?R.unlink(b):512===c?R.rmdir(b):g("Invalid flags passed to unlinkat"),0}catch(d){if("undefined"==typeof R||"ErrnoError"!==d.name)throw d;return -d.u}},_emscripten_get_now_is_monotonic:()=>1,_emscripten_throw_longjmp:()=>
    {throw Infinity;},_gmtime_js:function(a,b){a=-9007199254740992>a||9007199254740992<a?NaN:Number(a);a=new Date(1E3*a);F[b>>2]=a.getUTCSeconds();F[b+4>>2]=a.getUTCMinutes();F[b+8>>2]=a.getUTCHours();F[b+12>>2]=a.getUTCDate();F[b+16>>2]=a.getUTCMonth();F[b+20>>2]=a.getUTCFullYear()-1900;F[b+24>>2]=a.getUTCDay();F[b+28>>2]=(a.getTime()-Date.UTC(a.getUTCFullYear(),0,1,0,0,0,0))/864E5|0;},_localtime_js:function(a,b){a=-9007199254740992>a||9007199254740992<a?NaN:Number(a);a=new Date(1E3*a);F[b>>2]=a.getSeconds();
    F[b+4>>2]=a.getMinutes();F[b+8>>2]=a.getHours();F[b+12>>2]=a.getDate();F[b+16>>2]=a.getMonth();F[b+20>>2]=a.getFullYear()-1900;F[b+24>>2]=a.getDay();F[b+28>>2]=(Vb(a.getFullYear())?Wb:Xb)[a.getMonth()]+a.getDate()-1|0;F[b+36>>2]=-(60*a.getTimezoneOffset());var c=(new Date(a.getFullYear(),6,1)).getTimezoneOffset(),d=(new Date(a.getFullYear(),0,1)).getTimezoneOffset();F[b+32>>2]=(c!=d&&a.getTimezoneOffset()==Math.min(d,c))|0;},_mktime_js:function(a){var b=new Date(F[a+20>>2]+1900,F[a+16>>2],F[a+12>>
    2],F[a+8>>2],F[a+4>>2],F[a>>2],0),c=F[a+32>>2],d=b.getTimezoneOffset(),f=(new Date(b.getFullYear(),6,1)).getTimezoneOffset(),h=(new Date(b.getFullYear(),0,1)).getTimezoneOffset(),m=Math.min(h,f);0>c?F[a+32>>2]=Number(f!=h&&m==d):0<c!=(m==d)&&(f=Math.max(h,f),b.setTime(b.getTime()+6E4*((0<c?m:f)-d)));F[a+24>>2]=b.getDay();F[a+28>>2]=(Vb(b.getFullYear())?Wb:Xb)[b.getMonth()]+b.getDate()-1|0;F[a>>2]=b.getSeconds();F[a+4>>2]=b.getMinutes();F[a+8>>2]=b.getHours();F[a+12>>2]=b.getDate();F[a+16>>2]=b.getMonth();
    F[a+20>>2]=b.getYear();a=b.getTime();isNaN(a)?(F[lc()>>2]=61,a=-1):a/=1E3;return BigInt(a)},_tzset_js:(a,b,c)=>{function d(y){return (y=y.toTimeString().match(/\(([A-Za-z ]+)\)$/))?y[1]:"GMT"}var f=(new Date).getFullYear(),h=new Date(f,0,1),m=new Date(f,6,1);f=h.getTimezoneOffset();var p=m.getTimezoneOffset();G[a>>2]=60*Math.max(f,p);F[b>>2]=Number(f!=p);a=d(h);b=d(m);a=Zb(a);b=Zb(b);p<f?(G[c>>2]=a,G[c+4>>2]=b):(G[c>>2]=b,G[c+4>>2]=a);},abort:()=>{g("native code called abort()");},emscripten_date_now:()=>
    Date.now(),emscripten_get_now:()=>performance.now(),emscripten_resize_heap:a=>{var b=ta.length;a>>>=0;u(a>b);if(2147483648<a)return z(`Cannot enlarge memory, requested ${a} bytes, but the limit is ${2147483648} bytes!`),!1;for(var c=1;4>=c;c*=2){var d=b*(1+.2/c);d=Math.min(d,a+100663296);var f=Math;d=Math.max(a,d);f=f.min.call(f,2147483648,d+(65536-d%65536)%65536);a:{d=f;var h=ra.buffer,m=(d-h.byteLength+65535)/65536;try{ra.grow(m);ya();var p=1;break a}catch(y){z(`growMemory: Attempted to grow heap from ${h.byteLength} bytes to ${d} bytes, but got error: ${y}`);}p=
    void 0;}if(p)return !0}z(`Failed to grow the heap from ${b} bytes to ${f} bytes, not enough memory!`);return !1},environ_get:(a,b)=>{var c=0;bc().forEach((d,f)=>{var h=b+c;f=G[a+4*f>>2]=h;for(h=0;h<d.length;++h)u(d.charCodeAt(h)===(d.charCodeAt(h)&255)),E[f++>>0]=d.charCodeAt(h);E[f>>0]=0;c+=d.length+1;});return 0},environ_sizes_get:(a,b)=>{var c=bc();G[a>>2]=c.length;var d=0;c.forEach(f=>d+=f.length+1);G[b>>2]=d;return 0},exit:a=>{mc();sa=!0;ea(a,new pa(a));},fd_close:function(a){try{var b=W(a);R.close(b);
    return 0}catch(c){if("undefined"==typeof R||"ErrnoError"!==c.name)throw c;return c.u}},fd_read:function(a,b,c,d){try{a:{var f=W(a);a=b;for(var h,m=b=0;m<c;m++){var p=G[a>>2],y=G[a+4>>2];a+=8;var q=R.read(f,E,p,y,h);if(0>q){var x=-1;break a}b+=q;if(q<y)break;"undefined"!==typeof h&&(h+=q);}x=b;}G[d>>2]=x;return 0}catch(t){if("undefined"==typeof R||"ErrnoError"!==t.name)throw t;return t.u}},fd_seek:function(a,b,c,d){b=-9007199254740992>b||9007199254740992<b?NaN:Number(b);try{if(isNaN(b))return 61;var f=
    W(a);R.D(f,b,c);wa[d>>3]=BigInt(f.position);f.ea&&0===b&&0===c&&(f.ea=null);return 0}catch(h){if("undefined"==typeof R||"ErrnoError"!==h.name)throw h;return h.u}},fd_write:function(a,b,c,d){try{a:{var f=W(a);a=b;for(var h,m=b=0;m<c;m++){var p=G[a>>2],y=G[a+4>>2];a+=8;var q=R.write(f,E,p,y,h);if(0>q){var x=-1;break a}b+=q;"undefined"!==typeof h&&(h+=q);}x=b;}G[d>>2]=x;return 0}catch(t){if("undefined"==typeof R||"ErrnoError"!==t.name)throw t;return t.u}},invoke_vii:nc,strftime:(a,b,c,d)=>{function f(l,
    w,v){for(l="number"==typeof l?l.toString():l||"";l.length<w;)l=v[0]+l;return l}function h(l,w){return f(l,w,"0")}function m(l,w){function v(H){return 0>H?-1:0<H?1:0}var A;0===(A=v(l.getFullYear()-w.getFullYear()))&&0===(A=v(l.getMonth()-w.getMonth()))&&(A=v(l.getDate()-w.getDate()));return A}function p(l){switch(l.getDay()){case 0:return new Date(l.getFullYear()-1,11,29);case 1:return l;case 2:return new Date(l.getFullYear(),0,3);case 3:return new Date(l.getFullYear(),0,2);case 4:return new Date(l.getFullYear(),
    0,1);case 5:return new Date(l.getFullYear()-1,11,31);case 6:return new Date(l.getFullYear()-1,11,30)}}function y(l){var w=l.O;for(l=new Date((new Date(l.P+1900,0,1)).getTime());0<w;){var v=l.getMonth(),A=(Vb(l.getFullYear())?cc:dc)[v];if(w>A-l.getDate())w-=A-l.getDate()+1,l.setDate(1),11>v?l.setMonth(v+1):(l.setMonth(0),l.setFullYear(l.getFullYear()+1));else {l.setDate(l.getDate()+w);break}}v=new Date(l.getFullYear()+1,0,4);w=p(new Date(l.getFullYear(),0,4));v=p(v);return 0>=m(w,l)?0>=m(v,l)?l.getFullYear()+
    1:l.getFullYear():l.getFullYear()-1}var q=G[d+40>>2];d={Va:F[d>>2],Ua:F[d+4>>2],$:F[d+8>>2],la:F[d+12>>2],aa:F[d+16>>2],P:F[d+20>>2],H:F[d+24>>2],O:F[d+28>>2],wb:F[d+32>>2],Ta:F[d+36>>2],Wa:q?X(q):""};c=X(c);q={"%c":"%a %b %d %H:%M:%S %Y","%D":"%m/%d/%y","%F":"%Y-%m-%d","%h":"%b","%r":"%I:%M:%S %p","%R":"%H:%M","%T":"%H:%M:%S","%x":"%m/%d/%y","%X":"%H:%M:%S","%Ec":"%c","%EC":"%C","%Ex":"%m/%d/%y","%EX":"%H:%M:%S","%Ey":"%y","%EY":"%Y","%Od":"%d","%Oe":"%e","%OH":"%H","%OI":"%I","%Om":"%m","%OM":"%M",
    "%OS":"%S","%Ou":"%u","%OU":"%U","%OV":"%V","%Ow":"%w","%OW":"%W","%Oy":"%y"};for(var x in q)c=c.replace(new RegExp(x,"g"),q[x]);var t="Sunday Monday Tuesday Wednesday Thursday Friday Saturday".split(" "),B="January February March April May June July August September October November December".split(" ");q={"%a":l=>t[l.H].substring(0,3),"%A":l=>t[l.H],"%b":l=>B[l.aa].substring(0,3),"%B":l=>B[l.aa],"%C":l=>h((l.P+1900)/100|0,2),"%d":l=>h(l.la,2),"%e":l=>f(l.la,2," "),"%g":l=>y(l).toString().substring(2),
    "%G":l=>y(l),"%H":l=>h(l.$,2),"%I":l=>{l=l.$;0==l?l=12:12<l&&(l-=12);return h(l,2)},"%j":l=>{for(var w=0,v=0;v<=l.aa-1;w+=(Vb(l.P+1900)?cc:dc)[v++]);return h(l.la+w,3)},"%m":l=>h(l.aa+1,2),"%M":l=>h(l.Ua,2),"%n":()=>"\n","%p":l=>0<=l.$&&12>l.$?"AM":"PM","%S":l=>h(l.Va,2),"%t":()=>"\t","%u":l=>l.H||7,"%U":l=>h(Math.floor((l.O+7-l.H)/7),2),"%V":l=>{var w=Math.floor((l.O+7-(l.H+6)%7)/7);2>=(l.H+371-l.O-2)%7&&w++;if(w)53==w&&(v=(l.H+371-l.O)%7,4==v||3==v&&Vb(l.P)||(w=1));else {w=52;var v=(l.H+7-l.O-1)%
    7;(4==v||5==v&&Vb(l.P%400-1))&&w++;}return h(w,2)},"%w":l=>l.H,"%W":l=>h(Math.floor((l.O+7-(l.H+6)%7)/7),2),"%y":l=>(l.P+1900).toString().substring(2),"%Y":l=>l.P+1900,"%z":l=>{l=l.Ta;var w=0<=l;l=Math.abs(l)/60;return (w?"+":"-")+String("0000"+(l/60*100+l%60)).slice(-4)},"%Z":l=>l.Wa,"%%":()=>"%"};c=c.replace(/%%/g,"\x00\x00");for(x in q)c.includes(x)&&(c=c.replace(new RegExp(x,"g"),q[x](d)));c=c.replace(/\0\0/g,"%");x=lb(c,!1);if(x.length>b)return 0;ec(x,a);return x.length-1},system:a=>{if(n){if(!a)return 1;
    a=X(a);if(!a.length)return 0;a=require$1("child_process").ub(a,[],{tb:!0,stdio:"inherit"});var b=(c,d)=>c<<8|d;return null===a.status?b(0,(c=>{switch(c){case "SIGHUP":return 1;case "SIGQUIT":return 3;case "SIGFPE":return 8;case "SIGKILL":return 9;case "SIGALRM":return 14;case "SIGTERM":return 15}return 2})(a.signal)):a.status<<8|0}if(!a)return 0;F[lc()>>2]=52;return -1}},L=function(){var a={env:oc,wasi_snapshot_preview1:oc};Ma("wasm-instantiate");var b=e;Ta(a,function(c){u(e===b,"the Module object should not be replaced during async compilation - perhaps the order of HTML elements is wrong?");
    b=null;L=c.instance.exports;ra=L.memory;u(ra,"memory not found in wasm exports");ya();Z=L.__indirect_function_table;u(Z,"table not found in wasm exports");Fa.unshift(L.__wasm_call_ctors);Na("wasm-instantiate");}).catch(ba);return {}}();e._lua_checkstack=J("lua_checkstack");e._lua_xmove=J("lua_xmove");e._lua_atpanic=J("lua_atpanic");e._lua_version=J("lua_version");e._lua_absindex=J("lua_absindex");e._lua_gettop=J("lua_gettop");e._lua_settop=J("lua_settop");e._lua_closeslot=J("lua_closeslot");
    e._lua_rotate=J("lua_rotate");e._lua_copy=J("lua_copy");e._lua_pushvalue=J("lua_pushvalue");e._lua_type=J("lua_type");e._lua_typename=J("lua_typename");e._lua_iscfunction=J("lua_iscfunction");e._lua_isinteger=J("lua_isinteger");e._lua_isnumber=J("lua_isnumber");e._lua_isstring=J("lua_isstring");e._lua_isuserdata=J("lua_isuserdata");e._lua_rawequal=J("lua_rawequal");e._lua_arith=J("lua_arith");e._lua_compare=J("lua_compare");e._lua_stringtonumber=J("lua_stringtonumber");e._lua_tonumberx=J("lua_tonumberx");
    e._lua_tointegerx=J("lua_tointegerx");e._lua_toboolean=J("lua_toboolean");e._lua_tolstring=J("lua_tolstring");e._lua_rawlen=J("lua_rawlen");e._lua_tocfunction=J("lua_tocfunction");e._lua_touserdata=J("lua_touserdata");e._lua_tothread=J("lua_tothread");e._lua_topointer=J("lua_topointer");e._lua_pushnil=J("lua_pushnil");e._lua_pushnumber=J("lua_pushnumber");e._lua_pushinteger=J("lua_pushinteger");e._lua_pushlstring=J("lua_pushlstring");e._lua_pushstring=J("lua_pushstring");e._lua_pushcclosure=J("lua_pushcclosure");
    e._lua_pushboolean=J("lua_pushboolean");e._lua_pushlightuserdata=J("lua_pushlightuserdata");e._lua_pushthread=J("lua_pushthread");e._lua_getglobal=J("lua_getglobal");e._lua_gettable=J("lua_gettable");e._lua_getfield=J("lua_getfield");e._lua_geti=J("lua_geti");e._lua_rawget=J("lua_rawget");e._lua_rawgeti=J("lua_rawgeti");e._lua_rawgetp=J("lua_rawgetp");e._lua_createtable=J("lua_createtable");e._lua_getmetatable=J("lua_getmetatable");e._lua_getiuservalue=J("lua_getiuservalue");e._lua_setglobal=J("lua_setglobal");
    e._lua_settable=J("lua_settable");e._lua_setfield=J("lua_setfield");e._lua_seti=J("lua_seti");e._lua_rawset=J("lua_rawset");e._lua_rawsetp=J("lua_rawsetp");e._lua_rawseti=J("lua_rawseti");e._lua_setmetatable=J("lua_setmetatable");e._lua_setiuservalue=J("lua_setiuservalue");e._lua_callk=J("lua_callk");e._lua_pcallk=J("lua_pcallk");e._lua_load=J("lua_load");e._lua_dump=J("lua_dump");e._lua_status=J("lua_status");e._lua_error=J("lua_error");e._lua_next=J("lua_next");e._lua_toclose=J("lua_toclose");
    e._lua_concat=J("lua_concat");e._lua_len=J("lua_len");e._lua_getallocf=J("lua_getallocf");e._lua_setallocf=J("lua_setallocf");e._lua_setwarnf=J("lua_setwarnf");e._lua_warning=J("lua_warning");e._lua_newuserdatauv=J("lua_newuserdatauv");e._lua_getupvalue=J("lua_getupvalue");e._lua_setupvalue=J("lua_setupvalue");e._lua_upvalueid=J("lua_upvalueid");e._lua_upvaluejoin=J("lua_upvaluejoin");e._luaL_traceback=J("luaL_traceback");e._lua_getstack=J("lua_getstack");e._lua_getinfo=J("lua_getinfo");
    e._luaL_buffinit=J("luaL_buffinit");e._luaL_addstring=J("luaL_addstring");e._luaL_prepbuffsize=J("luaL_prepbuffsize");e._luaL_addvalue=J("luaL_addvalue");e._luaL_pushresult=J("luaL_pushresult");e._luaL_argerror=J("luaL_argerror");e._luaL_typeerror=J("luaL_typeerror");e._luaL_getmetafield=J("luaL_getmetafield");e._luaL_where=J("luaL_where");e._luaL_fileresult=J("luaL_fileresult");var lc=J("__errno_location");e._luaL_execresult=J("luaL_execresult");e._luaL_newmetatable=J("luaL_newmetatable");
    e._luaL_setmetatable=J("luaL_setmetatable");e._luaL_testudata=J("luaL_testudata");e._luaL_checkudata=J("luaL_checkudata");e._luaL_optlstring=J("luaL_optlstring");e._luaL_checklstring=J("luaL_checklstring");e._luaL_checkstack=J("luaL_checkstack");e._luaL_checktype=J("luaL_checktype");e._luaL_checkany=J("luaL_checkany");e._luaL_checknumber=J("luaL_checknumber");e._luaL_optnumber=J("luaL_optnumber");e._luaL_checkinteger=J("luaL_checkinteger");e._luaL_optinteger=J("luaL_optinteger");
    e._luaL_setfuncs=J("luaL_setfuncs");e._luaL_addlstring=J("luaL_addlstring");e._luaL_pushresultsize=J("luaL_pushresultsize");e._luaL_buffinitsize=J("luaL_buffinitsize");e._luaL_ref=J("luaL_ref");e._luaL_unref=J("luaL_unref");e._luaL_loadfilex=J("luaL_loadfilex");e._luaL_loadbufferx=J("luaL_loadbufferx");e._luaL_loadstring=J("luaL_loadstring");e._luaL_callmeta=J("luaL_callmeta");e._luaL_len=J("luaL_len");e._luaL_tolstring=J("luaL_tolstring");e._luaL_getsubtable=J("luaL_getsubtable");
    e._luaL_requiref=J("luaL_requiref");e._luaL_addgsub=J("luaL_addgsub");e._luaL_gsub=J("luaL_gsub");e._luaL_newstate=J("luaL_newstate");e._lua_newstate=J("lua_newstate");e._free=J("free");e._realloc=J("realloc");var Rb=e._fflush=J("fflush");e._luaL_checkversion_=J("luaL_checkversion_");e._luaopen_base=J("luaopen_base");e._luaopen_coroutine=J("luaopen_coroutine");e._lua_newthread=J("lua_newthread");e._lua_yieldk=J("lua_yieldk");e._lua_isyieldable=J("lua_isyieldable");e._lua_resetthread=J("lua_resetthread");
    e._lua_resume=J("lua_resume");e._luaopen_debug=J("luaopen_debug");e._lua_gethookmask=J("lua_gethookmask");e._lua_gethook=J("lua_gethook");e._lua_gethookcount=J("lua_gethookcount");e._lua_getlocal=J("lua_getlocal");e._lua_sethook=J("lua_sethook");e._lua_setlocal=J("lua_setlocal");e._lua_setcstacklimit=J("lua_setcstacklimit");var Yb=e._malloc=J("malloc");e._luaL_openlibs=J("luaL_openlibs");e._luaopen_package=J("luaopen_package");e._luaopen_table=J("luaopen_table");e._luaopen_io=J("luaopen_io");
    e._luaopen_os=J("luaopen_os");e._luaopen_string=J("luaopen_string");e._luaopen_math=J("luaopen_math");e._luaopen_utf8=J("luaopen_utf8");e._lua_close=J("lua_close");var pc=J("setThrew"),qc=()=>(qc=L.emscripten_stack_init)(),Aa=()=>(Aa=L.emscripten_stack_get_end)(),rc=J("stackSave"),sc=J("stackRestore"),tc=J("stackAlloc");function nc(a,b,c){var d=rc();try{gc(a)(b,c);}catch(f){sc(d);if(f!==f+0)throw f;pc(1,0);}}e.ENV=$b;
    e.ccall=(a,b,c,d)=>{var f={string:q=>{var x=0;if(null!==q&&void 0!==q&&0!==q){x=jb(q)+1;var t=tc(x);Ub(q,t,x);x=t;}return x},array:q=>{var x=tc(q.length);ec(q,x);return x}};a=hc(a);var h=[],m=0;u("array"!==b,'Return type should not be "array".');if(d)for(var p=0;p<d.length;p++){var y=f[c[p]];y?(0===m&&(m=rc()),h[p]=y(d[p])):h[p]=d[p];}c=a.apply(null,h);return c=function(q){0!==m&&sc(m);return "string"===b?X(q):"boolean"===b?!!q:q}(c)};
    e.addFunction=(a,b)=>{u("undefined"!=typeof a);if(!ic){ic=new WeakMap;var c=Z.length;if(ic)for(var d=0;d<0+c;d++){var f=gc(d);f&&ic.set(f,d);}}if(c=ic.get(a)||0)return c;if(jc.length)c=jc.pop();else {try{Z.grow(1);}catch(p){if(!(p instanceof RangeError))throw p;throw "Unable to grow wasm table. Set ALLOW_TABLE_GROWTH.";}c=Z.length-1;}try{d=c,Z.set(d,a),fc[d]=Z.get(d);}catch(p){if(!(p instanceof TypeError))throw p;u("undefined"!=typeof b,"Missing signature argument to addFunction: "+a);if("function"==typeof WebAssembly.Function){d=
    WebAssembly.Function;f={i:"i32",j:"i64",f:"f32",d:"f64",e:"externref",p:"i32"};for(var h={parameters:[],results:"v"==b[0]?[]:[f[b[0]]]},m=1;m<b.length;++m)u(b[m]in f,"invalid signature char: "+b[m]),h.parameters.push(f[b[m]]);b=new d(h,a);}else {d=[1];f=b.slice(0,1);b=b.slice(1);h={i:127,p:127,j:126,f:125,d:124,e:111};d.push(96);m=b.length;u(16384>m);128>m?d.push(m):d.push(m%128|128,m>>7);for(m=0;m<b.length;++m)u(b[m]in h,"invalid signature char: "+b[m]),d.push(h[b[m]]);"v"==f?d.push(0):d.push(1,h[f]);
    b=[0,97,115,109,1,0,0,0,1];f=d.length;u(16384>f);128>f?b.push(f):b.push(f%128|128,f>>7);b.push.apply(b,d);b.push(2,7,1,1,101,1,102,0,0,7,5,1,1,102,0,0);b=new WebAssembly.Module(new Uint8Array(b));b=(new WebAssembly.Instance(b,{e:{f:a}})).exports.f;}d=c;Z.set(d,b);fc[d]=Z.get(d);}ic.set(a,c);return c};e.removeFunction=a=>{ic.delete(gc(a));Z.set(a,null);fc[a]=Z.get(a);jc.push(a);};
    e.setValue=function(a,b,c="i8"){c.endsWith("*")&&(c="*");switch(c){case "i1":E[a>>0]=b;break;case "i8":E[a>>0]=b;break;case "i16":ua[a>>1]=b;break;case "i32":F[a>>2]=b;break;case "i64":wa[a>>3]=BigInt(b);break;case "float":va[a>>2]=b;break;case "double":xa[a>>3]=b;break;case "*":G[a>>2]=b;break;default:g(`invalid type for setValue: ${c}`);}};
    e.getValue=function(a,b="i8"){b.endsWith("*")&&(b="*");switch(b){case "i1":return E[a>>0];case "i8":return E[a>>0];case "i16":return ua[a>>1];case "i32":return F[a>>2];case "i64":return wa[a>>3];case "float":return va[a>>2];case "double":return xa[a>>3];case "*":return G[a>>2];default:g(`invalid type for getValue: ${b}`);}};e.stringToUTF8=Ub;e.lengthBytesUTF8=jb;e.stringToNewUTF8=Zb;e.FS=R;
    "writeI53ToI64 writeI53ToI64Clamped writeI53ToI64Signaling writeI53ToU64Clamped writeI53ToU64Signaling readI53FromI64 readI53FromU64 convertI32PairToI53 convertI32PairToI53Checked convertU32PairToI53 inetPton4 inetNtop4 inetPton6 inetNtop6 readSockaddr writeSockaddr getHostByName getCallstack emscriptenLog convertPCtoSourceLocation readEmAsmArgs jstoi_q jstoi_s listenOnce autoResumeAudioContext getDynCaller dynCall handleException runtimeKeepalivePush runtimeKeepalivePop callUserCallback maybeExit asmjsMangle handleAllocatorInit HandleAllocator getNativeTypeSize STACK_SIZE STACK_ALIGN POINTER_SIZE ASSERTIONS cwrap reallyNegative unSign strLen reSign formatString intArrayToString AsciiToString UTF16ToString stringToUTF16 lengthBytesUTF16 UTF32ToString stringToUTF32 lengthBytesUTF32 registerKeyEventCallback maybeCStringToJsString findEventTarget findCanvasEventTarget getBoundingClientRect fillMouseEventData registerMouseEventCallback registerWheelEventCallback registerUiEventCallback registerFocusEventCallback fillDeviceOrientationEventData registerDeviceOrientationEventCallback fillDeviceMotionEventData registerDeviceMotionEventCallback screenOrientation fillOrientationChangeEventData registerOrientationChangeEventCallback fillFullscreenChangeEventData registerFullscreenChangeEventCallback JSEvents_requestFullscreen JSEvents_resizeCanvasForFullscreen registerRestoreOldStyle hideEverythingExceptGivenElement restoreHiddenElements setLetterbox softFullscreenResizeWebGLRenderTarget doRequestFullscreen fillPointerlockChangeEventData registerPointerlockChangeEventCallback registerPointerlockErrorEventCallback requestPointerLock fillVisibilityChangeEventData registerVisibilityChangeEventCallback registerTouchEventCallback fillGamepadEventData registerGamepadEventCallback registerBeforeUnloadEventCallback fillBatteryEventData battery registerBatteryEventCallback setCanvasElementSize getCanvasElementSize jsStackTrace stackTrace checkWasiClock wasiRightsToMuslOFlags wasiOFlagsToMuslOFlags createDyncallWrapper safeSetTimeout setImmediateWrapped clearImmediateWrapped polyfillSetImmediate getPromise makePromise idsToPromises makePromiseCallback setMainLoop getSocketFromFD getSocketAddress FS_unlink FS_mkdirTree _setNetworkCallback".split(" ").forEach(function(a){"undefined"===typeof globalThis||
    Object.getOwnPropertyDescriptor(globalThis,a)||Object.defineProperty(globalThis,a,{configurable:!0,get(){var b=`\`${a}\` is a library symbol and not included by default; add it to your library.js __deps or to DEFAULT_LIBRARY_FUNCS_TO_INCLUDE on the command line`,c=a;c.startsWith("_")||(c="$"+a);b+=` (e.g. -sDEFAULT_LIBRARY_FUNCS_TO_INCLUDE='${c}')`;Ua(a)&&(b+=". Alternatively, forcing filesystem support (-sFORCE_FILESYSTEM) can export this for you");Wa(b);}});Xa(a);});"run addOnPreRun addOnInit addOnPreMain addOnExit addOnPostRun addRunDependency removeRunDependency FS_createFolder FS_createPath FS_createLazyFile FS_createLink FS_createDevice FS_readFile out err callMain abort wasmMemory wasmExports stackAlloc stackSave stackRestore getTempRet0 setTempRet0 writeStackCookie checkStackCookie MAX_INT53 MIN_INT53 bigintToI53Checked ptrToString zeroMemory exitJS getHeapMax growMemory MONTH_DAYS_REGULAR MONTH_DAYS_LEAP MONTH_DAYS_REGULAR_CUMULATIVE MONTH_DAYS_LEAP_CUMULATIVE isLeapYear ydayFromDate arraySum addDays ERRNO_CODES ERRNO_MESSAGES setErrNo DNS Protocols Sockets initRandomFill randomFill timers warnOnce UNWIND_CACHE readEmAsmArgsArray getExecutableName keepRuntimeAlive asyncLoad alignMemory mmapAlloc wasmTable noExitRuntime getCFunc uleb128Encode sigToWasmTypes generateFuncType convertJsFunctionToWasm freeTableIndexes functionsInTableMap getEmptyTableSlot updateTableMap getFunctionAddress PATH PATH_FS UTF8Decoder UTF8ArrayToString UTF8ToString stringToUTF8Array intArrayFromString stringToAscii UTF16Decoder stringToUTF8OnStack writeArrayToMemory JSEvents specialHTMLTargets currentFullscreenStrategy restoreOldWindowedStyle demangle demangleAll ExitStatus getEnvStrings doReadv doWritev promiseMap Browser wget SYSCALLS preloadPlugins FS_createPreloadedFile FS_modeStringToFlags FS_getMode FS_stdin_getChar_buffer FS_stdin_getChar FS_createDataFile MEMFS TTY PIPEFS SOCKFS".split(" ").forEach(Xa);
    var uc;Ja=function vc(){uc||wc();uc||(Ja=vc);};
    function wc(){if(!(0<Ia)){qc();var a=Aa();u(0==(a&3));0==a&&(a+=4);G[a>>2]=34821223;G[a+4>>2]=2310721022;G[0]=1668509029;if(e.preRun)for("function"==typeof e.preRun&&(e.preRun=[e.preRun]);e.preRun.length;)a=e.preRun.shift(),Ea.unshift(a);for(;0<Ea.length;)Ea.shift()(e);if(!(0<Ia)){if(!uc&&(uc=!0,e.calledRun=!0,!sa)){u(!Ha);Ha=!0;za();e.noFSInit||R.R.Y||R.R();for(R.ta=!1;0<Fa.length;)Fa.shift()(e);aa(e);u(!e._main,'compiled without a main, but one is present. if you added it from JS, use Module["onRuntimeInitialized"]');for(za();0<
    Ga.length;)Ga.shift()(e);}za();}}}function mc(){var a=qa,b=z,c=!1;qa=z=()=>{c=!0;};try{Rb(0),["stdout","stderr"].forEach(function(d){(d=Ob("/dev/"+d))&&(d=mb[d.object.rdev])&&d.output&&d.output.length&&(c=!0);});}catch(d){}qa=a;z=b;c&&Wa("stdio streams had content in them that was not flushed. you should set EXIT_RUNTIME to 1 (see the Emscripten FAQ), or make sure to emit a newline when you printf etc.");}wc();


      return moduleArg.ready
    }
    );
    })();

    class LuaWasm {
        static async initialize(customWasmFileLocation, environmentVariables) {
            const module = await initWasmModule({
                locateFile: (path, scriptDirectory) => {
                    return customWasmFileLocation || scriptDirectory + path;
                },
                preRun: (initializedModule) => {
                    if (typeof environmentVariables === 'object') {
                        Object.entries(environmentVariables).forEach(([k, v]) => (initializedModule.ENV[k] = v));
                    }
                },
            });
            return new LuaWasm(module);
        }
        constructor(module) {
            this.referenceTracker = new WeakMap();
            this.referenceMap = new Map();
            this.availableReferences = [];
            this.module = module;
            this.luaL_checkversion_ = this.cwrap('luaL_checkversion_', null, ['number', 'number', 'number']);
            this.luaL_getmetafield = this.cwrap('luaL_getmetafield', 'number', ['number', 'number', 'string']);
            this.luaL_callmeta = this.cwrap('luaL_callmeta', 'number', ['number', 'number', 'string']);
            this.luaL_tolstring = this.cwrap('luaL_tolstring', 'string', ['number', 'number', 'number']);
            this.luaL_argerror = this.cwrap('luaL_argerror', 'number', ['number', 'number', 'string']);
            this.luaL_typeerror = this.cwrap('luaL_typeerror', 'number', ['number', 'number', 'string']);
            this.luaL_checklstring = this.cwrap('luaL_checklstring', 'string', ['number', 'number', 'number']);
            this.luaL_optlstring = this.cwrap('luaL_optlstring', 'string', ['number', 'number', 'string', 'number']);
            this.luaL_checknumber = this.cwrap('luaL_checknumber', 'number', ['number', 'number']);
            this.luaL_optnumber = this.cwrap('luaL_optnumber', 'number', ['number', 'number', 'number']);
            this.luaL_checkinteger = this.cwrap('luaL_checkinteger', 'number', ['number', 'number']);
            this.luaL_optinteger = this.cwrap('luaL_optinteger', 'number', ['number', 'number', 'number']);
            this.luaL_checkstack = this.cwrap('luaL_checkstack', null, ['number', 'number', 'string']);
            this.luaL_checktype = this.cwrap('luaL_checktype', null, ['number', 'number', 'number']);
            this.luaL_checkany = this.cwrap('luaL_checkany', null, ['number', 'number']);
            this.luaL_newmetatable = this.cwrap('luaL_newmetatable', 'number', ['number', 'string']);
            this.luaL_setmetatable = this.cwrap('luaL_setmetatable', null, ['number', 'string']);
            this.luaL_testudata = this.cwrap('luaL_testudata', 'number', ['number', 'number', 'string']);
            this.luaL_checkudata = this.cwrap('luaL_checkudata', 'number', ['number', 'number', 'string']);
            this.luaL_where = this.cwrap('luaL_where', null, ['number', 'number']);
            this.luaL_fileresult = this.cwrap('luaL_fileresult', 'number', ['number', 'number', 'string']);
            this.luaL_execresult = this.cwrap('luaL_execresult', 'number', ['number', 'number']);
            this.luaL_ref = this.cwrap('luaL_ref', 'number', ['number', 'number']);
            this.luaL_unref = this.cwrap('luaL_unref', null, ['number', 'number', 'number']);
            this.luaL_loadfilex = this.cwrap('luaL_loadfilex', 'number', ['number', 'string', 'string']);
            this.luaL_loadbufferx = this.cwrap('luaL_loadbufferx', 'number', ['number', 'string|number', 'number', 'string|number', 'string']);
            this.luaL_loadstring = this.cwrap('luaL_loadstring', 'number', ['number', 'string']);
            this.luaL_newstate = this.cwrap('luaL_newstate', 'number', []);
            this.luaL_len = this.cwrap('luaL_len', 'number', ['number', 'number']);
            this.luaL_addgsub = this.cwrap('luaL_addgsub', null, ['number', 'string', 'string', 'string']);
            this.luaL_gsub = this.cwrap('luaL_gsub', 'string', ['number', 'string', 'string', 'string']);
            this.luaL_setfuncs = this.cwrap('luaL_setfuncs', null, ['number', 'number', 'number']);
            this.luaL_getsubtable = this.cwrap('luaL_getsubtable', 'number', ['number', 'number', 'string']);
            this.luaL_traceback = this.cwrap('luaL_traceback', null, ['number', 'number', 'string', 'number']);
            this.luaL_requiref = this.cwrap('luaL_requiref', null, ['number', 'string', 'number', 'number']);
            this.luaL_buffinit = this.cwrap('luaL_buffinit', null, ['number', 'number']);
            this.luaL_prepbuffsize = this.cwrap('luaL_prepbuffsize', 'string', ['number', 'number']);
            this.luaL_addlstring = this.cwrap('luaL_addlstring', null, ['number', 'string', 'number']);
            this.luaL_addstring = this.cwrap('luaL_addstring', null, ['number', 'string']);
            this.luaL_addvalue = this.cwrap('luaL_addvalue', null, ['number']);
            this.luaL_pushresult = this.cwrap('luaL_pushresult', null, ['number']);
            this.luaL_pushresultsize = this.cwrap('luaL_pushresultsize', null, ['number', 'number']);
            this.luaL_buffinitsize = this.cwrap('luaL_buffinitsize', 'string', ['number', 'number', 'number']);
            this.lua_newstate = this.cwrap('lua_newstate', 'number', ['number', 'number']);
            this.lua_close = this.cwrap('lua_close', null, ['number']);
            this.lua_newthread = this.cwrap('lua_newthread', 'number', ['number']);
            this.lua_resetthread = this.cwrap('lua_resetthread', 'number', ['number']);
            this.lua_atpanic = this.cwrap('lua_atpanic', 'number', ['number', 'number']);
            this.lua_version = this.cwrap('lua_version', 'number', ['number']);
            this.lua_absindex = this.cwrap('lua_absindex', 'number', ['number', 'number']);
            this.lua_gettop = this.cwrap('lua_gettop', 'number', ['number']);
            this.lua_settop = this.cwrap('lua_settop', null, ['number', 'number']);
            this.lua_pushvalue = this.cwrap('lua_pushvalue', null, ['number', 'number']);
            this.lua_rotate = this.cwrap('lua_rotate', null, ['number', 'number', 'number']);
            this.lua_copy = this.cwrap('lua_copy', null, ['number', 'number', 'number']);
            this.lua_checkstack = this.cwrap('lua_checkstack', 'number', ['number', 'number']);
            this.lua_xmove = this.cwrap('lua_xmove', null, ['number', 'number', 'number']);
            this.lua_isnumber = this.cwrap('lua_isnumber', 'number', ['number', 'number']);
            this.lua_isstring = this.cwrap('lua_isstring', 'number', ['number', 'number']);
            this.lua_iscfunction = this.cwrap('lua_iscfunction', 'number', ['number', 'number']);
            this.lua_isinteger = this.cwrap('lua_isinteger', 'number', ['number', 'number']);
            this.lua_isuserdata = this.cwrap('lua_isuserdata', 'number', ['number', 'number']);
            this.lua_type = this.cwrap('lua_type', 'number', ['number', 'number']);
            this.lua_typename = this.cwrap('lua_typename', 'string', ['number', 'number']);
            this.lua_tonumberx = this.cwrap('lua_tonumberx', 'number', ['number', 'number', 'number']);
            this.lua_tointegerx = this.cwrap('lua_tointegerx', 'number', ['number', 'number', 'number']);
            this.lua_toboolean = this.cwrap('lua_toboolean', 'number', ['number', 'number']);
            this.lua_tolstring = this.cwrap('lua_tolstring', 'string', ['number', 'number', 'number']);
            this.lua_rawlen = this.cwrap('lua_rawlen', 'number', ['number', 'number']);
            this.lua_tocfunction = this.cwrap('lua_tocfunction', 'number', ['number', 'number']);
            this.lua_touserdata = this.cwrap('lua_touserdata', 'number', ['number', 'number']);
            this.lua_tothread = this.cwrap('lua_tothread', 'number', ['number', 'number']);
            this.lua_topointer = this.cwrap('lua_topointer', 'number', ['number', 'number']);
            this.lua_arith = this.cwrap('lua_arith', null, ['number', 'number']);
            this.lua_rawequal = this.cwrap('lua_rawequal', 'number', ['number', 'number', 'number']);
            this.lua_compare = this.cwrap('lua_compare', 'number', ['number', 'number', 'number', 'number']);
            this.lua_pushnil = this.cwrap('lua_pushnil', null, ['number']);
            this.lua_pushnumber = this.cwrap('lua_pushnumber', null, ['number', 'number']);
            this.lua_pushinteger = this.cwrap('lua_pushinteger', null, ['number', 'number']);
            this.lua_pushlstring = this.cwrap('lua_pushlstring', 'string', ['number', 'string|number', 'number']);
            this.lua_pushstring = this.cwrap('lua_pushstring', 'string', ['number', 'string|number']);
            this.lua_pushcclosure = this.cwrap('lua_pushcclosure', null, ['number', 'number', 'number']);
            this.lua_pushboolean = this.cwrap('lua_pushboolean', null, ['number', 'number']);
            this.lua_pushlightuserdata = this.cwrap('lua_pushlightuserdata', null, ['number', 'number']);
            this.lua_pushthread = this.cwrap('lua_pushthread', 'number', ['number']);
            this.lua_getglobal = this.cwrap('lua_getglobal', 'number', ['number', 'string']);
            this.lua_gettable = this.cwrap('lua_gettable', 'number', ['number', 'number']);
            this.lua_getfield = this.cwrap('lua_getfield', 'number', ['number', 'number', 'string']);
            this.lua_geti = this.cwrap('lua_geti', 'number', ['number', 'number', 'number']);
            this.lua_rawget = this.cwrap('lua_rawget', 'number', ['number', 'number']);
            this.lua_rawgeti = this.cwrap('lua_rawgeti', 'number', ['number', 'number', 'number']);
            this.lua_rawgetp = this.cwrap('lua_rawgetp', 'number', ['number', 'number', 'number']);
            this.lua_createtable = this.cwrap('lua_createtable', null, ['number', 'number', 'number']);
            this.lua_newuserdatauv = this.cwrap('lua_newuserdatauv', 'number', ['number', 'number', 'number']);
            this.lua_getmetatable = this.cwrap('lua_getmetatable', 'number', ['number', 'number']);
            this.lua_getiuservalue = this.cwrap('lua_getiuservalue', 'number', ['number', 'number', 'number']);
            this.lua_setglobal = this.cwrap('lua_setglobal', null, ['number', 'string']);
            this.lua_settable = this.cwrap('lua_settable', null, ['number', 'number']);
            this.lua_setfield = this.cwrap('lua_setfield', null, ['number', 'number', 'string']);
            this.lua_seti = this.cwrap('lua_seti', null, ['number', 'number', 'number']);
            this.lua_rawset = this.cwrap('lua_rawset', null, ['number', 'number']);
            this.lua_rawseti = this.cwrap('lua_rawseti', null, ['number', 'number', 'number']);
            this.lua_rawsetp = this.cwrap('lua_rawsetp', null, ['number', 'number', 'number']);
            this.lua_setmetatable = this.cwrap('lua_setmetatable', 'number', ['number', 'number']);
            this.lua_setiuservalue = this.cwrap('lua_setiuservalue', 'number', ['number', 'number', 'number']);
            this.lua_callk = this.cwrap('lua_callk', null, ['number', 'number', 'number', 'number', 'number']);
            this.lua_pcallk = this.cwrap('lua_pcallk', 'number', ['number', 'number', 'number', 'number', 'number', 'number']);
            this.lua_load = this.cwrap('lua_load', 'number', ['number', 'number', 'number', 'string', 'string']);
            this.lua_dump = this.cwrap('lua_dump', 'number', ['number', 'number', 'number', 'number']);
            this.lua_yieldk = this.cwrap('lua_yieldk', 'number', ['number', 'number', 'number', 'number']);
            this.lua_resume = this.cwrap('lua_resume', 'number', ['number', 'number', 'number', 'number']);
            this.lua_status = this.cwrap('lua_status', 'number', ['number']);
            this.lua_isyieldable = this.cwrap('lua_isyieldable', 'number', ['number']);
            this.lua_setwarnf = this.cwrap('lua_setwarnf', null, ['number', 'number', 'number']);
            this.lua_warning = this.cwrap('lua_warning', null, ['number', 'string', 'number']);
            this.lua_error = this.cwrap('lua_error', 'number', ['number']);
            this.lua_next = this.cwrap('lua_next', 'number', ['number', 'number']);
            this.lua_concat = this.cwrap('lua_concat', null, ['number', 'number']);
            this.lua_len = this.cwrap('lua_len', null, ['number', 'number']);
            this.lua_stringtonumber = this.cwrap('lua_stringtonumber', 'number', ['number', 'string']);
            this.lua_getallocf = this.cwrap('lua_getallocf', 'number', ['number', 'number']);
            this.lua_setallocf = this.cwrap('lua_setallocf', null, ['number', 'number', 'number']);
            this.lua_toclose = this.cwrap('lua_toclose', null, ['number', 'number']);
            this.lua_closeslot = this.cwrap('lua_closeslot', null, ['number', 'number']);
            this.lua_getstack = this.cwrap('lua_getstack', 'number', ['number', 'number', 'number']);
            this.lua_getinfo = this.cwrap('lua_getinfo', 'number', ['number', 'string', 'number']);
            this.lua_getlocal = this.cwrap('lua_getlocal', 'string', ['number', 'number', 'number']);
            this.lua_setlocal = this.cwrap('lua_setlocal', 'string', ['number', 'number', 'number']);
            this.lua_getupvalue = this.cwrap('lua_getupvalue', 'string', ['number', 'number', 'number']);
            this.lua_setupvalue = this.cwrap('lua_setupvalue', 'string', ['number', 'number', 'number']);
            this.lua_upvalueid = this.cwrap('lua_upvalueid', 'number', ['number', 'number', 'number']);
            this.lua_upvaluejoin = this.cwrap('lua_upvaluejoin', null, ['number', 'number', 'number', 'number', 'number']);
            this.lua_sethook = this.cwrap('lua_sethook', null, ['number', 'number', 'number', 'number']);
            this.lua_gethook = this.cwrap('lua_gethook', 'number', ['number']);
            this.lua_gethookmask = this.cwrap('lua_gethookmask', 'number', ['number']);
            this.lua_gethookcount = this.cwrap('lua_gethookcount', 'number', ['number']);
            this.lua_setcstacklimit = this.cwrap('lua_setcstacklimit', 'number', ['number', 'number']);
            this.luaopen_base = this.cwrap('luaopen_base', 'number', ['number']);
            this.luaopen_coroutine = this.cwrap('luaopen_coroutine', 'number', ['number']);
            this.luaopen_table = this.cwrap('luaopen_table', 'number', ['number']);
            this.luaopen_io = this.cwrap('luaopen_io', 'number', ['number']);
            this.luaopen_os = this.cwrap('luaopen_os', 'number', ['number']);
            this.luaopen_string = this.cwrap('luaopen_string', 'number', ['number']);
            this.luaopen_utf8 = this.cwrap('luaopen_utf8', 'number', ['number']);
            this.luaopen_math = this.cwrap('luaopen_math', 'number', ['number']);
            this.luaopen_debug = this.cwrap('luaopen_debug', 'number', ['number']);
            this.luaopen_package = this.cwrap('luaopen_package', 'number', ['number']);
            this.luaL_openlibs = this.cwrap('luaL_openlibs', null, ['number']);
        }
        lua_remove(luaState, index) {
            this.lua_rotate(luaState, index, -1);
            this.lua_pop(luaState, 1);
        }
        lua_pop(luaState, count) {
            this.lua_settop(luaState, -count - 1);
        }
        luaL_getmetatable(luaState, name) {
            return this.lua_getfield(luaState, LUA_REGISTRYINDEX, name);
        }
        lua_yield(luaState, count) {
            return this.lua_yieldk(luaState, count, 0, null);
        }
        lua_upvalueindex(index) {
            return LUA_REGISTRYINDEX - index;
        }
        ref(data) {
            const existing = this.referenceTracker.get(data);
            if (existing) {
                existing.refCount++;
                return existing.index;
            }
            const availableIndex = this.availableReferences.pop();
            const index = availableIndex === undefined ? this.referenceMap.size + 1 : availableIndex;
            this.referenceMap.set(index, data);
            this.referenceTracker.set(data, {
                refCount: 1,
                index,
            });
            this.lastRefIndex = index;
            return index;
        }
        unref(index) {
            const ref = this.referenceMap.get(index);
            if (ref === undefined) {
                return;
            }
            const metadata = this.referenceTracker.get(ref);
            if (metadata === undefined) {
                this.referenceTracker.delete(ref);
                this.availableReferences.push(index);
                return;
            }
            metadata.refCount--;
            if (metadata.refCount <= 0) {
                this.referenceTracker.delete(ref);
                this.referenceMap.delete(index);
                this.availableReferences.push(index);
            }
        }
        getRef(index) {
            return this.referenceMap.get(index);
        }
        getLastRefIndex() {
            return this.lastRefIndex;
        }
        printRefs() {
            for (const [key, value] of this.referenceMap.entries()) {
                console.log(key, value);
            }
        }
        cwrap(name, returnType, argTypes) {
            const hasStringOrNumber = argTypes.some((argType) => argType === 'string|number');
            if (!hasStringOrNumber) {
                return (...args) => this.module.ccall(name, returnType, argTypes, args);
            }
            return (...args) => {
                const pointersToBeFreed = [];
                const resolvedArgTypes = argTypes.map((argType, i) => {
                    var _a;
                    if (argType === 'string|number') {
                        if (typeof args[i] === 'number') {
                            return 'number';
                        }
                        else {
                            if (((_a = args[i]) === null || _a === void 0 ? void 0 : _a.length) > 1024) {
                                const bufferPointer = this.module.stringToNewUTF8(args[i]);
                                args[i] = bufferPointer;
                                pointersToBeFreed.push(bufferPointer);
                                return 'number';
                            }
                            else {
                                return 'string';
                            }
                        }
                    }
                    return argType;
                });
                try {
                    return this.module.ccall(name, returnType, resolvedArgTypes, args);
                }
                finally {
                    for (const pointer of pointersToBeFreed) {
                        this.module._free(pointer);
                    }
                }
            };
        }
    }

    var version = '1.16.0';

    class LuaFactory {
        constructor(customWasmUri, environmentVariables) {
            var _a;
            if (customWasmUri === undefined) {
                const isBrowser = (typeof window === 'object' && typeof window.document !== 'undefined') ||
                    (typeof self === 'object' && ((_a = self === null || self === void 0 ? void 0 : self.constructor) === null || _a === void 0 ? void 0 : _a.name) === 'DedicatedWorkerGlobalScope');
                if (isBrowser) {
                    customWasmUri = `https://unpkg.com/wasmoon@${version}/dist/glue.wasm`;
                }
            }
            this.luaWasmPromise = LuaWasm.initialize(customWasmUri, environmentVariables);
        }
        async mountFile(path, content) {
            this.mountFileSync(await this.getLuaModule(), path, content);
        }
        mountFileSync(luaWasm, path, content) {
            const fileSep = path.lastIndexOf('/');
            const file = path.substring(fileSep + 1);
            const body = path.substring(0, path.length - file.length - 1);
            if (body.length > 0) {
                const parts = body.split('/').reverse();
                let parent = '';
                while (parts.length) {
                    const part = parts.pop();
                    if (!part) {
                        continue;
                    }
                    const current = `${parent}/${part}`;
                    try {
                        luaWasm.module.FS.mkdir(current);
                    }
                    catch (err) {
                    }
                    parent = current;
                }
            }
            luaWasm.module.FS.writeFile(path, content);
        }
        async createEngine(options = {}) {
            return new LuaEngine(await this.getLuaModule(), options);
        }
        async getLuaModule() {
            return this.luaWasmPromise;
        }
    }

    exports.Decoration = Decoration;
    exports.LUAI_MAXSTACK = LUAI_MAXSTACK;
    exports.LUA_MULTRET = LUA_MULTRET;
    exports.LUA_REGISTRYINDEX = LUA_REGISTRYINDEX;
    exports.LuaEngine = LuaEngine;
    exports.LuaFactory = LuaFactory;
    exports.LuaGlobal = Global;
    exports.LuaMultiReturn = MultiReturn;
    exports.LuaRawResult = RawResult;
    exports.LuaThread = Thread;
    exports.LuaTimeoutError = LuaTimeoutError;
    exports.LuaTypeExtension = LuaTypeExtension;
    exports.LuaWasm = LuaWasm;
    exports.PointerSize = PointerSize;
    exports.decorate = decorate;
    exports.decorateFunction = decorateFunction;
    exports.decorateProxy = decorateProxy;
    exports.decorateUserdata = decorateUserdata;

}));
