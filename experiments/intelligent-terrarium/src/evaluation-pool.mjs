import { Worker } from "node:worker_threads";

const workerUrl = new URL("./evaluation-worker.mjs", import.meta.url);

export class EvaluationPool {
  constructor({ size = 1 } = {}) {
    if (!Number.isInteger(size) || size < 1 || size > 8) throw new RangeError("evaluation worker count must be 1..8");
    this.size = size;
    this.nextTaskId = 1;
    this.queue = [];
    this.closed = false;
    this.startedAt = performance.now();
    this.completed = 0;
    this.busyMs = 0;
    this.peakQueued = 0;
    this.slots = Array.from({ length: size }, () => this.#makeSlot());
  }

  #makeSlot() {
    const slot = { worker: null, task: null, busyStartedAt: 0 };
    this.#spawn(slot);
    return slot;
  }

  #spawn(slot) {
    const worker = new Worker(workerUrl);
    slot.worker = worker;
    worker.on("message", (message) => {
      if (slot.worker !== worker) return;
      const task = slot.task;
      if (!task || message.taskId !== task.taskId) return;
      this.busyMs += performance.now() - slot.busyStartedAt;
      this.completed += 1;
      slot.task = null;
      slot.busyStartedAt = 0;
      task.resolve({ source: task.source, options: task.options,
        candidate: message.candidate || null, error: message.error || null });
      this.#dispatch();
    });
    worker.on("error", (error) => {
      if (slot.worker !== worker) return;
      const task = slot.task;
      if (task) {
        this.busyMs += performance.now() - slot.busyStartedAt;
        this.completed += 1;
      }
      slot.task = null;
      slot.busyStartedAt = 0;
      if (task) task.resolve({ source: task.source, options: task.options, candidate: null, error: error.message });
      slot.worker = null;
      void worker.terminate();
      if (!this.closed) this.#spawn(slot);
      this.#dispatch();
    });
    worker.on("exit", (code) => {
      if (slot.worker !== worker) return;
      const task = slot.task;
      if (task) {
        this.busyMs += performance.now() - slot.busyStartedAt;
        this.completed += 1;
      }
      slot.task = null;
      slot.busyStartedAt = 0;
      if (task) task.resolve({ source: task.source, options: task.options, candidate: null,
        error: `evaluation worker exited with code ${code}` });
      slot.worker = null;
      if (!this.closed) this.#spawn(slot);
      this.#dispatch();
    });
  }

  evaluate({ source, options = {} }) {
    if (this.closed) return Promise.reject(new Error("evaluation pool is closed"));
    return new Promise((resolve) => {
      this.queue.push({ taskId: this.nextTaskId++, source, options, resolve });
      this.peakQueued = Math.max(this.peakQueued, this.queue.length);
      this.#dispatch();
    });
  }

  #dispatch() {
    if (this.closed) return;
    for (const slot of this.slots) {
      if (!slot.worker || slot.task || !this.queue.length) continue;
      slot.task = this.queue.shift();
      slot.busyStartedAt = performance.now();
      slot.worker.postMessage({ taskId: slot.task.taskId, source: slot.task.source, options: slot.task.options });
    }
  }

  snapshot(at = performance.now()) {
    const busy = this.slots.filter((slot) => slot.task).length;
    const activeBusyMs = this.slots.reduce((sum, slot) => sum + (slot.task ? at - slot.busyStartedAt : 0), 0);
    const elapsedMs = Math.max(1, at - this.startedAt);
    return {
      size: this.size,
      busy,
      queued: this.queue.length,
      completed: this.completed,
      peakQueued: this.peakQueued,
      utilization: Math.min(1, (this.busyMs + activeBusyMs) / (elapsedMs * this.size)),
      averageTaskMs: this.completed ? this.busyMs / this.completed : 0,
    };
  }

  async close() {
    this.closed = true;
    for (const task of this.queue.splice(0)) {
      task.resolve({ source: task.source, options: task.options, candidate: null, error: "evaluation pool closed" });
    }
    await Promise.all(this.slots.map((slot) => slot.worker?.terminate()));
  }
}
