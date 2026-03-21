import * as wasmoonModule from "./wasmoon.js";

const exportsRef = globalThis.wasmoon || wasmoonModule.default || wasmoonModule;

if (!exportsRef || typeof exportsRef.LuaFactory !== "function") {
  throw new Error("Failed to initialize Wasmoon runtime");
}

export const { LuaFactory, LuaReturn, LuaType, LuaTimeoutError } = exportsRef;
