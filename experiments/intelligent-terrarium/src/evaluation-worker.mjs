import { parentPort } from "node:worker_threads";
import { evaluateRasterProgram, evaluateSortProgram } from "./sort-soup.mjs";

if (!parentPort) throw new Error("evaluation worker needs a parent port");

parentPort.on("message", ({ taskId, source, options }) => {
  try {
    const evaluate = String(source).trimStart().startsWith("(raster") ? evaluateRasterProgram : evaluateSortProgram;
    parentPort.postMessage({ taskId, candidate: evaluate(source, options) });
  } catch (error) {
    parentPort.postMessage({ taskId, error: String(error?.message || error) });
  }
});
