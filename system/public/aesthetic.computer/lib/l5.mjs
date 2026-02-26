const WASM_URL = new URL("../dep/wasmoon/glue.wasm", import.meta.url).href;

let factoryPromise;
let LuaFactoryCtor;

async function ensureLuaFactoryCtor() {
  if (!LuaFactoryCtor) {
    const wasmoon = await import("../dep/wasmoon/index.mjs");
    LuaFactoryCtor = wasmoon.LuaFactory;
  }
  return LuaFactoryCtor;
}

async function ensureFactory() {
  if (!factoryPromise) {
    const LuaFactory = await ensureLuaFactoryCtor();
    factoryPromise = Promise.resolve(new LuaFactory(WASM_URL));
  }
  return factoryPromise;
}

function clampByte(value) {
  if (typeof value !== "number" || !Number.isFinite(value)) return 0;
  if (value < 0) return 0;
  if (value > 255) return 255;
  return Math.round(value);
}

function asNumber(value, fallback = 0) {
  if (typeof value === "number" && Number.isFinite(value)) return value;
  return fallback;
}

function parseColorArgs(args, fallback = [255, 255, 255, 255]) {
  if (!Array.isArray(args) || args.length === 0) return [...fallback];

  if (Array.isArray(args[0])) {
    return parseColorArgs(args[0], fallback);
  }

  if (args.length === 1) {
    const gray = clampByte(args[0]);
    return [gray, gray, gray, 255];
  }

  const r = clampByte(args[0]);
  const g = clampByte(args[1]);
  const b = clampByte(args[2]);
  const a = clampByte(args.length >= 4 ? args[3] : 255);
  return [r, g, b, a];
}

function applyInk($, color) {
  $.ink(color[0], color[1], color[2], color[3]);
}

function safeGetGlobalFn(engine, name) {
  try {
    const fn = engine.global.get(name);
    return typeof fn === "function" ? fn : null;
  } catch {
    return null;
  }
}

function updateInputGlobals(engine, $, state) {
  const mouseX = asNumber($.pen?.x, 0);
  const mouseY = asNumber($.pen?.y, 0);

  state.pmouseX = state.mouseX;
  state.pmouseY = state.mouseY;
  state.mouseX = mouseX;
  state.mouseY = mouseY;

  engine.global.set("mouseX", mouseX);
  engine.global.set("mouseY", mouseY);
  engine.global.set("pmouseX", state.pmouseX);
  engine.global.set("pmouseY", state.pmouseY);
  engine.global.set("movedX", mouseX - state.pmouseX);
  engine.global.set("movedY", mouseY - state.pmouseY);
  engine.global.set("mouseIsPressed", !!state.mouseIsPressed);

  engine.global.set("width", asNumber($.screen?.width, 128));
  engine.global.set("height", asNumber($.screen?.height, 128));
  engine.global.set("frameCount", state.frameCount);
  engine.global.set("deltaTime", state.deltaTime);
  engine.global.set("focused", true);

  engine.global.set("key", state.key);
  engine.global.set("keyCode", state.keyCode);
  engine.global.set("keyIsPressed", !!state.keyIsPressed);
}

function makeErrorModule(message) {
  return {
    boot() {},
    sim() {},
    act() {},
    leave() {},
    paint($) {
      $.wipe(20, 0, 0);
      $.ink(255, 80, 80).write("L5 COMPILE ERROR", { x: 4, y: 4 });
      $.ink(255, 220, 220).write(String(message || "unknown error"), {
        x: 4,
        y: 18,
      });
      return true;
    },
  };
}

export async function module(source) {
  const factory = await ensureFactory();
  const engine = await factory.createEngine({
    injectObjects: false,
    enableProxy: true,
    openStandardLibs: true,
  });

  let activeApi = null;
  let runtimeError = null;
  let closed = false;

  const state = {
    fillEnabled: true,
    strokeEnabled: true,
    fillColor: [255, 255, 255, 255],
    strokeColor: [0, 0, 0, 255],
    strokeWeight: 1,
    textSize: 1,
    looping: true,
    redrawRequested: false,
    hasDrawnOnce: false,
    frameCount: 0,
    deltaTime: 16,
    lastFrameMs: 0,
    mouseX: 0,
    mouseY: 0,
    pmouseX: 0,
    pmouseY: 0,
    mouseIsPressed: false,
    key: "",
    keyCode: 0,
    keyIsPressed: false,
  };

  const withApi = (fn) => (...args) => {
    if (!activeApi) return undefined;
    try {
      return fn(activeApi, ...args);
    } catch (error) {
      runtimeError = error;
      console.error("L5 runtime API error:", error);
      return undefined;
    }
  };

  const drawRect = ($, x, y, w, h) => {
    if (state.fillEnabled) {
      applyInk($, state.fillColor);
      $.box(x, y, w, h, "fill");
    }
    if (state.strokeEnabled) {
      applyInk($, state.strokeColor);
      $.box(x, y, w, h, "outline");
    }
  };

  const drawCircle = ($, x, y, diameter) => {
    const radius = asNumber(diameter, 0) / 2;
    if (state.fillEnabled) {
      applyInk($, state.fillColor);
      $.circle(x, y, radius, true);
    }
    if (state.strokeEnabled) {
      applyInk($, state.strokeColor);
      $.circle(x, y, radius, false, Math.max(1, asNumber(state.strokeWeight, 1)));
    }
  };

  const drawEllipse = ($, x, y, w, h) => {
    const rx = asNumber(w, 0) / 2;
    const ry = asNumber(h, 0) / 2;
    if (state.fillEnabled) {
      applyInk($, state.fillColor);
      $.oval(x, y, rx, ry, true);
    }
    if (state.strokeEnabled) {
      applyInk($, state.strokeColor);
      $.oval(x, y, rx, ry, false, Math.max(1, asNumber(state.strokeWeight, 1)));
    }
  };

  const drawTriangle = ($, x1, y1, x2, y2, x3, y3) => {
    if (state.fillEnabled) {
      applyInk($, state.fillColor);
      $.tri(x1, y1, x2, y2, x3, y3, "fill");
    }
    if (state.strokeEnabled) {
      applyInk($, state.strokeColor);
      $.tri(x1, y1, x2, y2, x3, y3, "outline");
    }
  };

  const drawQuad = ($, x1, y1, x2, y2, x3, y3, x4, y4) => {
    if (state.fillEnabled) {
      applyInk($, state.fillColor);
      $.shape({ points: [[x1, y1], [x2, y2], [x3, y3], [x4, y4]], filled: true });
    }
    if (state.strokeEnabled) {
      applyInk($, state.strokeColor);
      $.poly([[x1, y1], [x2, y2], [x3, y3], [x4, y4], [x1, y1]]);
    }
  };

  engine.global.set("background", withApi(($, ...args) => {
    const color = parseColorArgs(args, [0, 0, 0, 255]);
    $.wipe(color[0], color[1], color[2], color[3]);
  }));

  engine.global.set("clear", withApi(($) => {
    $.wipe(0, 0, 0, 0);
  }));

  engine.global.set("size", withApi(($, w, h) => {
    $.resolution(asNumber(w, 128), asNumber(h, asNumber(w, 128)));
  }));

  engine.global.set("fill", (...args) => {
    state.fillEnabled = true;
    state.fillColor = parseColorArgs(args, state.fillColor);
  });

  engine.global.set("noFill", () => {
    state.fillEnabled = false;
  });

  engine.global.set("stroke", (...args) => {
    state.strokeEnabled = true;
    state.strokeColor = parseColorArgs(args, state.strokeColor);
  });

  engine.global.set("noStroke", () => {
    state.strokeEnabled = false;
  });

  engine.global.set("strokeWeight", (value) => {
    state.strokeWeight = Math.max(1, asNumber(value, 1));
  });

  engine.global.set("point", withApi(($, x, y) => {
    if (!state.strokeEnabled) return;
    applyInk($, state.strokeColor);
    $.plot(asNumber(x), asNumber(y));
  }));

  engine.global.set("line", withApi(($, x1, y1, x2, y2) => {
    if (!state.strokeEnabled) return;
    applyInk($, state.strokeColor);
    $.line(asNumber(x1), asNumber(y1), asNumber(x2), asNumber(y2));
  }));

  engine.global.set("rect", withApi(($, x, y, w, h) => {
    drawRect($, asNumber(x), asNumber(y), asNumber(w), asNumber(h));
  }));

  engine.global.set("square", withApi(($, x, y, size) => {
    const side = asNumber(size);
    drawRect($, asNumber(x), asNumber(y), side, side);
  }));

  engine.global.set("circle", withApi(($, x, y, diameter) => {
    drawCircle($, asNumber(x), asNumber(y), asNumber(diameter));
  }));

  engine.global.set("ellipse", withApi(($, x, y, w, h) => {
    drawEllipse($, asNumber(x), asNumber(y), asNumber(w), asNumber(h));
  }));

  engine.global.set("triangle", withApi(($, x1, y1, x2, y2, x3, y3) => {
    drawTriangle($, asNumber(x1), asNumber(y1), asNumber(x2), asNumber(y2), asNumber(x3), asNumber(y3));
  }));

  engine.global.set("quad", withApi(($, x1, y1, x2, y2, x3, y3, x4, y4) => {
    drawQuad(
      $,
      asNumber(x1),
      asNumber(y1),
      asNumber(x2),
      asNumber(y2),
      asNumber(x3),
      asNumber(y3),
      asNumber(x4),
      asNumber(y4),
    );
  }));

  engine.global.set("text", withApi(($, str, x, y) => {
    if (!state.fillEnabled) return;
    applyInk($, state.fillColor);
    $.write(String(str ?? ""), {
      x: asNumber(x),
      y: asNumber(y),
      size: Math.max(1, asNumber(state.textSize, 1) / 8),
    });
  }));

  engine.global.set("textSize", (value) => {
    state.textSize = Math.max(1, asNumber(value, 8));
  });

  engine.global.set("textWidth", withApi(($, value) => {
    return $.text.width(String(value ?? ""));
  }));

  engine.global.set("frameRate", withApi(($, value) => {
    $.fps?.(asNumber(value));
  }));

  engine.global.set("noLoop", () => {
    state.looping = false;
  });

  engine.global.set("loop", withApi(($) => {
    state.looping = true;
    state.redrawRequested = true;
    $.needsPaint?.();
  }));

  engine.global.set("isLooping", () => state.looping);

  engine.global.set("redraw", withApi(($) => {
    state.redrawRequested = true;
    $.needsPaint?.();
  }));

  engine.global.set("millis", () => performance.now());
  engine.global.set("print", (...args) => console.log(...args));
  engine.global.set("println", (...args) => console.log(...args));

  engine.global.set("random", (a, b) => {
    if (a === undefined) return Math.random();
    const min = b === undefined ? 0 : asNumber(a);
    const max = b === undefined ? asNumber(a, 1) : asNumber(b, 1);
    return min + Math.random() * (max - min);
  });

  engine.global.set("radians", (deg) => (asNumber(deg) * Math.PI) / 180);
  engine.global.set("degrees", (rad) => (asNumber(rad) * 180) / Math.PI);
  engine.global.set("constrain", (v, lo, hi) => Math.max(asNumber(lo), Math.min(asNumber(hi), asNumber(v))));
  engine.global.set("lerp", (a, b, t) => asNumber(a) + (asNumber(b) - asNumber(a)) * asNumber(t));
  engine.global.set("dist", (x1, y1, x2, y2) => {
    const dx = asNumber(x2) - asNumber(x1);
    const dy = asNumber(y2) - asNumber(y1);
    return Math.sqrt(dx * dx + dy * dy);
  });
  engine.global.set("map", (v, inMin, inMax, outMin, outMax) => {
    const n = asNumber(v);
    const a = asNumber(inMin);
    const b = asNumber(inMax);
    if (b === a) return asNumber(outMin);
    const t = (n - a) / (b - a);
    return asNumber(outMin) + t * (asNumber(outMax) - asNumber(outMin));
  });

  engine.global.set("PI", Math.PI);
  engine.global.set("HALF_PI", Math.PI / 2);
  engine.global.set("QUARTER_PI", Math.PI / 4);
  engine.global.set("TWO_PI", Math.PI * 2);
  engine.global.set("TAU", Math.PI * 2);
  engine.global.set("LEFT", "LEFT");
  engine.global.set("RIGHT", "RIGHT");
  engine.global.set("CENTER", "CENTER");
  engine.global.set("TOP", "TOP");
  engine.global.set("BOTTOM", "BOTTOM");
  engine.global.set("CORNER", "CORNER");
  engine.global.set("CORNERS", "CORNERS");
  engine.global.set("RADIUS", "RADIUS");
  engine.global.set("CLOSE", "CLOSE");
  engine.global.set("RGB", "RGB");
  engine.global.set("HSB", "HSB");
  engine.global.set("HSL", "HSL");

  try {
    await engine.doString(source);
  } catch (error) {
    console.error("L5 compile error:", error);
    try {
      engine.global.close();
    } catch {
      // no-op
    }
    return makeErrorModule(error?.message || String(error));
  }

  const setup = safeGetGlobalFn(engine, "setup");
  const draw = safeGetGlobalFn(engine, "draw");
  const simFn = safeGetGlobalFn(engine, "sim");
  const actFn = safeGetGlobalFn(engine, "act");

  const mousePressed = safeGetGlobalFn(engine, "mousePressed");
  const mouseReleased = safeGetGlobalFn(engine, "mouseReleased");
  const mouseMoved = safeGetGlobalFn(engine, "mouseMoved");
  const mouseDragged = safeGetGlobalFn(engine, "mouseDragged");

  const keyPressed = safeGetGlobalFn(engine, "keyPressed");
  const keyReleased = safeGetGlobalFn(engine, "keyReleased");

  const safeCall = (name, fn, ...args) => {
    if (typeof fn !== "function") return;
    try {
      fn(...args);
    } catch (error) {
      runtimeError = error;
      console.error(`L5 runtime error in ${name}:`, error);
    }
  };

  const maybePaintRuntimeError = ($) => {
    if (!runtimeError) return false;
    $.ink(255, 60, 60).box(0, 0, $.screen.width, 14);
    $.ink(255, 240, 240).write(`L5: ${String(runtimeError.message || runtimeError)}`, {
      x: 2,
      y: 2,
    });
    return true;
  };

  const runSetup = async ($) => {
    activeApi = $;
    updateInputGlobals(engine, $, state);
    safeCall("setup", setup);
  };

  const runPaint = ($) => {
    activeApi = $;

    const now = performance.now();
    if (state.lastFrameMs === 0) {
      state.deltaTime = 16;
    } else {
      state.deltaTime = Math.max(0, now - state.lastFrameMs);
    }
    state.lastFrameMs = now;

    updateInputGlobals(engine, $, state);

    const shouldDraw = state.looping || !state.hasDrawnOnce || state.redrawRequested;
    if (!shouldDraw) {
      maybePaintRuntimeError($);
      return false;
    }

    state.redrawRequested = false;
    state.frameCount += 1;
    engine.global.set("frameCount", state.frameCount);

    safeCall("draw", draw);
    state.hasDrawnOnce = true;

    maybePaintRuntimeError($);
    return true;
  };

  const runSim = ($) => {
    activeApi = $;
    updateInputGlobals(engine, $, state);
    safeCall("sim", simFn);
  };

  const runAct = ($) => {
    activeApi = $;
    updateInputGlobals(engine, $, state);

    const event = $.event;
    if (event) {
      const eventName = event.name || "";

      if (eventName.startsWith("keyboard:down")) {
        state.keyIsPressed = true;
        const maybeKey = event.key || event.char || eventName.split(":").pop() || "";
        state.key = String(maybeKey || "");
        state.keyCode = state.key.length ? state.key.charCodeAt(0) : 0;
        engine.global.set("key", state.key);
        engine.global.set("keyCode", state.keyCode);
        engine.global.set("keyIsPressed", true);
        safeCall("keyPressed", keyPressed);
      } else if (eventName.startsWith("keyboard:up")) {
        state.keyIsPressed = false;
        engine.global.set("keyIsPressed", false);
        safeCall("keyReleased", keyReleased);
      }

      const isEvent = (name) => {
        if (typeof event.is === "function") return event.is(name);
        return eventName === name;
      };

      if (isEvent("touch")) {
        state.mouseIsPressed = true;
        engine.global.set("mouseIsPressed", true);
        safeCall("mousePressed", mousePressed);
      } else if (isEvent("lift")) {
        state.mouseIsPressed = false;
        engine.global.set("mouseIsPressed", false);
        safeCall("mouseReleased", mouseReleased);
      } else if (isEvent("move")) {
        safeCall("mouseMoved", mouseMoved);
      } else if (isEvent("draw")) {
        state.mouseIsPressed = true;
        engine.global.set("mouseIsPressed", true);
        safeCall("mouseDragged", mouseDragged);
      }
    }

    safeCall("act", actFn);
  };

  const runLeave = () => {
    if (closed) return;
    closed = true;
    try {
      engine.global.close();
    } catch (error) {
      console.warn("L5 leave cleanup error:", error);
    }
  };

  return {
    boot: runSetup,
    paint: runPaint,
    sim: runSim,
    act: runAct,
    leave: runLeave,
  };
}
