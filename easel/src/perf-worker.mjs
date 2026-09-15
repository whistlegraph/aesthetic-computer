// Executed by perf.mjs in a bounded, permission-restricted child process.
import vm from "node:vm";
import { performance } from "node:perf_hooks";

let input = "";
for await (const chunk of process.stdin) input += chunk;
try {
  const { source, frames, warmup, width, height, seed, timeoutMs } = JSON.parse(input);
  const context = vm.createContext(Object.create(null), {
    codeGeneration: { strings: false, wasm: false },
  });
  vm.runInContext(`
    let randomState = ${seed} || 1;
    Math.random = () => {
      randomState ^= randomState << 13;
      randomState ^= randomState >>> 17;
      randomState ^= randomState << 5;
      return (randomState >>> 0) / 4294967296;
    };
  `, context, { timeout: 100 });
  const module = new vm.SourceTextModule(source, {
    context,
    identifier: "piece.mjs",
    importModuleDynamically: () => { throw new Error("Imports are unavailable in the headless logic benchmark."); },
  });
  await module.link(() => { throw new Error("Imports are unavailable in the headless logic benchmark."); });
  await module.evaluate({ timeout: timeoutMs });
  context.__piece = module.namespace;
  // All callbacks are created inside the guest realm; no host function, fs,
  // process, network client, or constructor is passed through the piece API.
  vm.runInContext(`
    const counts = Object.create(null);
    const drawing = ["wipe", "ink", "line", "circle", "box", "rect", "point", "plot", "polygon", "triangle", "write", "print", "paste"];
    const api = { screen: { width: ${width}, height: ${height} } };
    for (const name of drawing) api[name] = (..._args) => { counts[name] = (counts[name] || 0) + 1; return api; };
    Object.freeze(api.screen);
    Object.freeze(api);
    if (typeof __piece.paint !== "function" && typeof __piece.sim !== "function") throw new Error("This piece has no paint or sim export to benchmark.");
    function call(name) {
      const result = __piece[name]?.(api);
      if (result && typeof result.then === "function") throw new Error("Async lifecycle functions are unavailable in the headless benchmark.");
    }
    call("boot");
    for (let frame = 0; frame < ${warmup}; frame++) { call("sim"); call("paint"); }
    for (const name of Object.keys(counts)) counts[name] = 0;
  `, context, { timeout: timeoutMs });
  const started = performance.now();
  vm.runInContext(`
    for (let frame = 0; frame < ${frames}; frame++) { call("sim"); call("paint"); }
  `, context, { timeout: timeoutMs });
  const elapsedMs = performance.now() - started;
  const totalCalls = JSON.parse(vm.runInContext("JSON.stringify(counts)", context, { timeout: 100 }));
  const drawCalls = Object.fromEntries(Object.entries(totalCalls).map(([name, count]) => [name, count / frames]));
  process.stdout.write(JSON.stringify({ measurement: "headless-logic", frames, warmup, width, height, seed, elapsedMs, msPerFrame: elapsedMs / frames, drawCalls, totalCalls }));
} catch (error) {
  process.stderr.write(String(error.message).slice(0, 4096));
  process.exitCode = 1;
}
