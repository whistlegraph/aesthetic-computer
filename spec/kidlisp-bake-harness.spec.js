import { createBakeHarness } from "./support/kidlisp-bake-harness.js";

describe("KidLisp bake harness", () => {
  it("captures telemetry events without console noise", async () => {
    const harness = await createBakeHarness({ passThroughTelemetry: false });

    harness.kid.tracePaintCommand("line", [0, 0, 10, 10], {
      target: "baked",
      source: "test"
    });

    harness.kid.traceRoutingTransition("switch", {
      from: "screen",
      to: "baked"
    });

    harness.kid.traceFrameRoutingContext?.("snapshot", {
      target: "screen"
    });

    expect(harness.paintEvents.length).toBe(1);
    expect(harness.paintEvents[0]).toEqual({
      commandName: "line",
      args: [0, 0, 10, 10],
      meta: {
        target: "baked",
        source: "test"
      }
    });

    expect(harness.routingEvents.length).toBe(1);
    expect(harness.routingEvents[0]).toEqual({
      eventName: "switch",
      meta: {
        from: "screen",
        to: "baked"
      }
    });

    expect(harness.contextEvents.length).toBe(1);
    expect(harness.contextEvents[0]).toEqual({
      label: "snapshot",
      meta: {
        target: "screen"
      }
    });

    harness.restore();
  });

  it("provides mock paint API stubs for future routing specs", async () => {
    const harness = await createBakeHarness();
    const { paintApi } = harness;

    expect(typeof paintApi).toBe("object");
    expect(paintApi.screen.width).toBeGreaterThan(0);
    expect(paintApi.screen.height).toBeGreaterThan(0);
    expect(typeof paintApi.line).toBe("function");

    paintApi.line(0, 0, 10, 10);
    expect(paintApi.line.calls.length).toBe(1);

    harness.restore();
  });

  it("routes through baked and post-bake layers after (bake)", async () => {
    const harness = await createBakeHarness();
    const { kid, paintApi, paintEvents, routingEvents, contextEvents, reset, restore } = harness;

    const source = `
      (ink 255 255 255 255)
      (line 0 0 32 32)
      (bake)
      (ink 255 0 0 255)
      (line 8 8 64 64)
    `;

    const piece = kid.module(source);
    expect(typeof piece.paint).toBe("function");

    piece.boot({
      wipe: () => {},
      params: [],
      clock: { resync: () => {} },
      screen: paintApi.screen,
      sound: {
        microphone: null,
        enabled: () => false
      },
      delay: () => {},
      pieceCount: () => {},
      net: {},
      backgroundFill: () => {}
    });

    piece.paint(paintApi);

    const firstFrameEvents = routingEvents.map(event => event.eventName);
    expect(firstFrameEvents).toEqual([
      "beginBakedFrameRouting:skip",
      "switchToPostBakeRouting"
    ]);

    const firstFrameContextLabels = contextEvents.map(event => event.label);
    expect(firstFrameContextLabels).toEqual([
      "frame-start",
      "beginBakedFrameRouting:skip",
      "switchToPostBakeRouting",
      "frame-routing-restored",
      "frame-end"
    ]);

    const firstFrameScreenLine = paintEvents.find(event => event.commandName === "line" && event.meta?.target === "screen");
    const firstFrameOverlayLine = paintEvents.find(event => event.commandName === "line" && event.meta?.target === "postBake");
    expect(firstFrameScreenLine).toBeDefined();
    expect(firstFrameOverlayLine).toBeDefined();

    reset();

    piece.paint(paintApi);

    const secondFrameEvents = routingEvents.map(event => event.eventName);
    expect(secondFrameEvents).toEqual([
      "beginBakedFrameRouting",
      "switchToPostBakeRouting",
      "endBakedFrameRouting"
    ]);

    expect(paintEvents.some(event => event.meta?.target === "baked")).toBe(true);
    expect(paintEvents.some(event => event.meta?.target === "postBake")).toBe(true);

    const secondFrameContextLabels = contextEvents.map(event => event.label);
    expect(secondFrameContextLabels).toEqual([
      "frame-start",
      "beginBakedFrameRouting",
      "switchToPostBakeRouting",
      "endBakedFrameRouting:before-reset",
      "endBakedFrameRouting",
      "frame-routing-restored",
      "frame-end"
    ]);

    restore();
  });

  it("executes blur commands before the first bake and mirrors them afterward", async () => {
    const harness = await createBakeHarness();
    const { kid, paintApi, paintEvents, reset, restore } = harness;

    const source = `
      (ink 165 42 42 255)
      (line 0 0 32 32)
      (blur 6)
      (bake)
    `;

    const piece = kid.module(source);
    expect(typeof piece.paint).toBe("function");

    piece.boot({
      wipe: () => {},
      params: [],
      clock: { resync: () => {} },
      screen: paintApi.screen,
      sound: {
        microphone: null,
        enabled: () => false
      },
      delay: () => {},
      pieceCount: () => {},
      net: {},
      backgroundFill: () => {}
    });

    piece.paint(paintApi);

    const firstBlurEvent = paintEvents.find(event => event.commandName === "blur");
    expect(firstBlurEvent).toBeDefined();
    expect(firstBlurEvent.meta?.target).toBe("screen");
    expect(firstBlurEvent.meta?.phase).toBe("direct-fallback");
    expect(paintApi.blur.calls.length).toBe(1);

    reset();

    piece.paint(paintApi);

    const secondBlurEvent = paintEvents.find(event => event.commandName === "blur");
    expect(secondBlurEvent).toBeDefined();
    expect(secondBlurEvent.meta?.target).toBe("baked");
    expect(secondBlurEvent.meta?.phase).toBe("baked-frame");
    expect(paintApi.blur.calls.length).toBe(2);

    restore();
  });

  it("mirrors post-bake erase commands into the baked buffer", async () => {
    const harness = await createBakeHarness();
    const { kid, paintApi, paintEvents, routingEvents, contextEvents, reset, restore } = harness;

    const previousUnwrapped = Object.prototype.hasOwnProperty.call(globalThis, "$paintApiUnwrapped")
      ? globalThis.$paintApiUnwrapped
      : undefined;
    const proxiedUnwrapped = {
      screen: paintApi.screen
    };
    Object.keys(paintApi).forEach((key) => {
      if (typeof paintApi[key] === "function") {
        proxiedUnwrapped[key] = paintApi[key];
      }
    });
    globalThis.$paintApiUnwrapped = proxiedUnwrapped;

    try {
      const source = `
        (ink 255 255 255 255)
        (line 0 0 32 32)
        (bake)
        (ink erase)
        (line 16 16 96 96)
      `;

      const piece = kid.module(source);
      expect(typeof piece.paint).toBe("function");

      piece.boot({
        wipe: () => {},
        params: [],
        clock: { resync: () => {} },
        screen: paintApi.screen,
        sound: {
          microphone: null,
          enabled: () => false
        },
        delay: () => {},
        pieceCount: () => {},
        net: {},
        backgroundFill: () => {}
      });

      piece.paint(paintApi);

      const overlayErase = paintEvents.find(event =>
        event.commandName === "line" &&
        event.meta?.target === "postBake" &&
        event.meta?.details?.mirrorMode === "postBakeErase"
      );
      expect(overlayErase).toBeDefined();
      expect(overlayErase.meta.details?.mirrorTarget).toBe("baked");
      expect(overlayErase.meta.duplicated).toBe(true);

      const mirroredErase = paintEvents.find(event =>
        event.commandName === "line" &&
        event.meta?.target === "baked" &&
        event.meta?.source === "mirrorErase"
      );
      expect(mirroredErase).toBeDefined();
      expect(mirroredErase.meta.details?.mirrorSource).toBe("postBake");
      expect(mirroredErase.meta.metadata?.mirrorStrategy).toBe("erase");
      expect(mirroredErase.meta.duplicated).toBe(true);

      const firstContextLabels = contextEvents.map(event => event.label);
      expect(firstContextLabels).toContain("switchToPostBakeRouting");

      reset();

      piece.paint(paintApi);

      const overlayEraseAgain = paintEvents.find(event =>
        event.commandName === "line" &&
        event.meta?.target === "postBake" &&
        event.meta?.details?.mirrorMode === "postBakeErase"
      );
      expect(overlayEraseAgain).toBeDefined();

      const mirroredEraseAgain = paintEvents.find(event =>
        event.commandName === "line" &&
        event.meta?.target === "baked" &&
        event.meta?.source === "mirrorErase"
      );
      expect(mirroredEraseAgain).toBeDefined();
      expect(mirroredEraseAgain.meta.metadata?.mirrorStrategy).toBe("erase");

      const routingSequence = routingEvents.map(event => event.eventName);
      expect(routingSequence).toEqual([
        "beginBakedFrameRouting",
        "switchToPostBakeRouting",
        "endBakedFrameRouting"
      ]);

      const secondContextLabels = contextEvents.map(event => event.label);
      expect(secondContextLabels).toEqual([
        "frame-start",
        "beginBakedFrameRouting",
        "switchToPostBakeRouting",
        "endBakedFrameRouting:before-reset",
        "endBakedFrameRouting",
        "frame-routing-restored",
        "frame-end"
      ]);
    } finally {
      restore();
      if (previousUnwrapped === undefined) {
        delete globalThis.$paintApiUnwrapped;
      } else {
        globalThis.$paintApiUnwrapped = previousUnwrapped;
      }
    }
  });

  it("keeps unwrapped aggregation while routing erase fallback", async () => {
    const harness = await createBakeHarness();
    const { kid, paintApi, restore } = harness;

    const buffer = {
      width: 8,
      height: 8,
      pixels: new Uint8ClampedArray(8 * 8 * 4)
    };
    kid.layerManager.appendBakedLayer(buffer);
    kid.suppressDrawingBeforeBake = true;

    kid.inkState = ["erase", 64];
    kid.inkStateSet = true;

    const unwrappedInkCalls = [];
    const unwrappedLineCalls = [];

    const previousUnwrapped = Object.prototype.hasOwnProperty.call(globalThis, "$paintApiUnwrapped")
      ? globalThis.$paintApiUnwrapped
      : undefined;

    globalThis.$paintApiUnwrapped = {
      ink: (...args) => {
        unwrappedInkCalls.push(args);
      },
      line: (...args) => {
        unwrappedLineCalls.push(args);
      }
    };

    try {
      const initialInkCallCount = paintApi.ink.calls.length;
      kid.runWithBakedBuffer(paintApi, () => {});
      const newInkCalls = paintApi.ink.calls.slice(initialInkCallCount);
      expect(newInkCalls.some((args) => Array.isArray(args) && args[0] === "erase")).toBe(true);
      expect(unwrappedInkCalls.some((args) => Array.isArray(args) && args[0] === "erase")).toBe(true);

      const initialLineCallCount = paintApi.line.calls.length;
      kid.getGlobalEnv().line(paintApi, [0, 0, 4, 4]);

      expect(unwrappedLineCalls).toEqual([[0, 0, 4, 4]]);
      const newLineCalls = paintApi.line.calls.slice(initialLineCallCount);
      expect(newLineCalls).toEqual([[0, 0, 4, 4]]);
    } finally {
      restore();
      if (previousUnwrapped === undefined) {
        delete globalThis.$paintApiUnwrapped;
      } else {
        globalThis.$paintApiUnwrapped = previousUnwrapped;
      }
    }
  });

  it("reapplies erase ink when mirroring without an unwrapped paint API", async () => {
    const harness = await createBakeHarness();
    const { kid, paintApi, paintEvents, restore } = harness;

    const hadUnwrapped = Object.prototype.hasOwnProperty.call(globalThis, "$paintApiUnwrapped");
    const previousUnwrapped = hadUnwrapped ? globalThis.$paintApiUnwrapped : undefined;
    if (hadUnwrapped) {
      delete globalThis.$paintApiUnwrapped;
    }

    try {
      const source = `
        (ink 255 255 255 255)
        (line 0 0 32 32)
        (bake)
        (ink erase 32)
        (line 8 8 64 64)
      `;

      const piece = kid.module(source);
      expect(typeof piece.paint).toBe("function");

      piece.boot({
        wipe: () => {},
        params: [],
        clock: { resync: () => {} },
        screen: paintApi.screen,
        sound: {
          microphone: null,
          enabled: () => false
        },
        delay: () => {},
        pieceCount: () => {},
        net: {},
        backgroundFill: () => {}
      });

      piece.paint(paintApi);

      const eraseInkCalls = paintApi.ink.calls.filter((args) =>
        Array.isArray(args) && args[0] === "erase" && args[1] === 32
      );
      expect(eraseInkCalls.length).toBe(2);

      const mirroredErase = paintEvents.find(event =>
        event.commandName === "line" &&
        event.meta?.target === "baked" &&
        event.meta?.source === "mirrorErase"
      );
      expect(mirroredErase).toBeDefined();
    } finally {
      restore();
      if (hadUnwrapped) {
        globalThis.$paintApiUnwrapped = previousUnwrapped;
      } else {
        delete globalThis.$paintApiUnwrapped;
      }
    }
  });

  it("retroactively mirrors queued erase commands once routing reactivates", async () => {
    const harness = await createBakeHarness();
    const { kid, paintApi, paintEvents, reset, restore } = harness;

    const source = `
      (ink 255 255 255 255)
      (line 0 0 32 32)
      (bake)
      (ink erase)
      (line 8 8 64 64)
    `;

    const piece = kid.module(source);
    expect(typeof piece.paint).toBe("function");

    piece.boot({
      wipe: () => {},
      params: [],
      clock: { resync: () => {} },
      screen: paintApi.screen,
      sound: {
        microphone: null,
        enabled: () => false
      },
      delay: () => {},
      pieceCount: () => {},
      net: {},
      backgroundFill: () => {}
    });

    piece.paint(paintApi);

    reset();

    // Simulate a deferred erase command captured immediately after (bake)
    kid.inkState = ["erase"];
    kid.inkStateSet = true;
    kid.routingState?.recordRetroactiveErase?.({
      commandName: "line",
      args: [8, 8, 64, 64],
      metadata: {
        layerContext: "postBake",
        queueIndex: 0,
        queueLength: 1,
        source: "spec"
      },
      inkSnapshot: ["erase"]
    });

    kid.flushRetroactiveEraseQueue(paintApi, { reason: "spec" });

    const retroOverlay = paintEvents.find(event =>
      event.meta?.source === "retroactiveOverlay" &&
      event.meta?.target === "postBake"
    );
    expect(retroOverlay).toBeDefined();

    const retroMirror = paintEvents.find(event =>
      event.meta?.source === "retroactiveMirror" &&
      event.meta?.target === "baked"
    );
    expect(retroMirror).toBeDefined();
    expect(retroMirror.meta?.details?.retroactive).toBe(true);

    restore();
  });

  it("replaces the latest baked layer by default instead of stacking", async () => {
    const harness = await createBakeHarness();
    const { kid, paintApi, getBakedTargets, reset, restore } = harness;

    const source = `
      (ink 255 255 255 255)
      (line 0 0 32 32)
      (bake)
    `;

    const piece = kid.module(source);
    expect(typeof piece.paint).toBe("function");

    piece.boot({
      wipe: () => {},
      params: [],
      clock: { resync: () => {} },
      screen: paintApi.screen,
      sound: {
        microphone: null,
        enabled: () => false
      },
      delay: () => {},
      pieceCount: () => {},
      net: {},
      backgroundFill: () => {}
    });

    piece.paint(paintApi);

    let bakedTargets = getBakedTargets();
    expect(bakedTargets.length).toBe(1);
    const firstBuffer = bakedTargets[0];

    const firstLayers = kid.layerManager?.bakedLayers?.() ?? [];
    expect(firstLayers.length).toBe(1);
    expect(firstLayers[0]?.metadata?.callIndex).toBe(1);

    reset();

    piece.paint(paintApi);

    bakedTargets = getBakedTargets();
    expect(bakedTargets.length).toBe(1);
    const secondBuffer = bakedTargets[0];
    expect(secondBuffer).not.toBe(firstBuffer);

    const bakedLayers = kid.layerManager?.bakedLayers?.() ?? [];
    expect(bakedLayers.length).toBe(1);
    expect(bakedLayers[0]?.metadata?.callIndex).toBe(2);

    restore();
  });

  it("does not restore stale ink color before first frame execution", async () => {
    const harness = await createBakeHarness();
    const { kid, paintApi, reset, restore } = harness;

    const source = `
      (ink erase)
      (line 0 0 32 32)
    `;

    const piece = kid.module(source);
    expect(typeof piece.paint).toBe("function");

    piece.boot({
      wipe: () => {},
      params: [],
      clock: { resync: () => {} },
      screen: paintApi.screen,
      sound: {
        microphone: null,
        enabled: () => false
      },
      delay: () => {},
      pieceCount: () => {},
      net: {},
      backgroundFill: () => {}
    });

    piece.paint(paintApi);

    expect(paintApi.ink.calls.length).toBeGreaterThan(0);
    const [firstInkArgs] = paintApi.ink.calls;
    expect(Array.isArray(firstInkArgs)).toBe(true);
    expect(firstInkArgs.length).toBe(1);
    expect(firstInkArgs[0]).toBeUndefined();

    const eraseInkCall = paintApi.ink.calls.find((args) => args.includes("erase"));
    expect(eraseInkCall).toBeDefined();

    reset();
    restore();
  });

  it("restores routing state when switchToPostBakeRouting fails", async () => {
  const harness = await createBakeHarness();
  const { kid, paintApi, getOverlayTarget, restore } = harness;

    const source = `
      (ink 255 255 255 255)
      (line 0 0 16 16)
      (bake)
    `;

    const piece = kid.module(source);
    expect(typeof piece.paint).toBe("function");

    piece.boot({
      wipe: () => {},
      params: [],
      clock: { resync: () => {} },
      screen: paintApi.screen,
      sound: {
        microphone: null,
        enabled: () => false
      },
      delay: () => {},
      pieceCount: () => {},
      net: {},
      backgroundFill: () => {}
    });

    piece.paint(paintApi);

    const failingPage = (...args) => {
      failingPage.calls.push(args);
      throw new Error("page failure");
    };
    failingPage.calls = [];
    paintApi.page = failingPage;

    expect(() => kid.switchToPostBakeRouting(paintApi)).toThrowError("page failure");

    expect(kid.routingState?.currentTarget?.()).toBe("screen");
    expect(kid.routingState?.findLastLayer?.("postBake")).toBeNull();
    expect(kid.postBakeMirroringState).toBeNull();
    expect(failingPage.calls.length).toBeGreaterThanOrEqual(1);
  expect(failingPage.calls[0][0]).toBe(getOverlayTarget());
    const lastCall = failingPage.calls[failingPage.calls.length - 1];
    expect(lastCall[0]).toBe(paintApi.screen);

    restore();
  });
});
