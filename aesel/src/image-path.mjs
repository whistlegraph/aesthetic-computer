// Browser-safe image paths shared by Aesel's picture tools and painting surfaces.
// Each path keeps its server's model catalog, authorization and billing contract.
export const imagePaths = Object.freeze({
  picture: Object.freeze({ endpoint: "/api/easel-image", timeout: 330000, id: "jobId" }),
  nopaint: Object.freeze({ endpoint: "/api/nopaint-inference", timeout: 200000, id: "requestId" }),
});

export class ImagePath {
  constructor({ path, request, onchange = () => {} }) {
    if (!Object.hasOwn(imagePaths, path)) throw Error(`Unknown image path: ${path}`);
    this.path = path;
    this.request = request;
    this.onchange = onchange;
    this.state = "blank";
    this.pending = false;
  }

  change(state) {
    this.state = state;
    // A view failing to update must never turn completed work into a retry.
    try { this.onchange(state); } catch {}
  }

  run(input) {
    const route = imagePaths[this.path], id = input?.[route.id];
    if (typeof id !== "string" || !id) return Promise.reject(Error("An image request ID is required."));
    const body = JSON.stringify(input);
    if (this.job?.id === id) {
      return this.job.body === body ? this.job.promise
        : Promise.reject(Error("Image request ID already used for different input."));
    }
    if (this.pending) return Promise.reject(Error("An image request is still processing."));
    const job = { id, body, cancelled: false };
    this.job = job;
    this.pending = true;
    // Assign the promise before notifying observers, including synchronous ones.
    job.promise = Promise.resolve().then(() => this.request(route.endpoint, JSON.parse(body), route.timeout))
      .then(result => {
        this.pending = false;
        this.change(job.cancelled ? "interrupted" : "complete");
        return result;
      }, error => {
        this.pending = false;
        this.change(job.cancelled ? "interrupted" : "stale");
        throw error;
      });
    this.change("working");
    return job.promise;
  }

  cancel() {
    // No rejects the artwork. Keep waiting for its receipt; never replay or
    // imply that stopping the view refunds an already submitted image.
    if (this.pending) this.job.cancelled = true;
    this.change("interrupted");
  }

  reset() {
    if (!this.pending) this.change("blank");
  }
}
