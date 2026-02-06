/**
 * Max/MSP JavaScript for batching audio samples
 * Receives packed [L, R] sample pairs, batches them, and outputs as arrays
 * 
 * Inlets:
 *   0: list of [left, right] sample pair
 *   
 * Outlets:
 *   0: executejavascript message with batched samples
 */

// Configuration
var BATCH_SIZE = 64;  // Sample pairs per batch (~64ms at 1kHz metro)

// State
var batchL = [];
var batchR = [];

// Declare inlets/outlets
inlets = 1;
outlets = 1;

function list(l, r) {
    // Receive packed [L, R] pair
    batchL.push(l);
    batchR.push(r);
    
    // When batch is full, send it
    if (batchL.length >= BATCH_SIZE) {
        sendBatch();
    }
}

function sendBatch() {
    if (batchL.length === 0) return;
    
    // Take samples from buffers
    var left = batchL.splice(0, BATCH_SIZE);
    var right = batchR.splice(0, BATCH_SIZE);
    
    // Format as JSON array strings (compact)
    var leftStr = "[" + left.join(",") + "]";
    var rightStr = "[" + right.join(",") + "]";
    
    // Output executejavascript command
    outlet(0, "executejavascript", "window.acSampleBatch&&window.acSampleBatch(" + leftStr + "," + rightStr + ")");
}

// Reset buffers
function reset() {
    batchL = [];
    batchR = [];
}

// Change batch size
function batchsize(n) {
    if (n > 0 && n <= 4096) {
        BATCH_SIZE = n;
        reset();
        post("Batch size set to " + n + "\n");
    }
}

// Flush any remaining samples
function bang() {
    if (batchL.length > 0) {
        sendBatch();
    }
}
