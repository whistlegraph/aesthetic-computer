/**
 * Max/MSP JavaScript for reading audio from buffer~ and streaming to jweb~
 * 
 * Uses Max's Buffer object to read samples at full sample rate.
 * Runs on a timer to periodically grab chunks and send them.
 * 
 * Arguments:
 *   buffer name (e.g., "ac-audio-L")
 *   
 * Inlets:
 *   0: bang to read next chunk, or "start"/"stop" messages
 *   
 * Outlets:
 *   0: executejavascript message with sample arrays
 */

// Configuration
var CHUNK_SIZE = 512;     // Samples per chunk (~11.6ms at 44.1kHz)
var BUFFER_SIZE = 4096;   // Total buffer size (must match buffer~ size)

// Buffer references (set via arguments or messages)
var bufferNameL = "ac-audio-L";
var bufferNameR = "ac-audio-R";
var bufL = null;
var bufR = null;

// State
var readPos = 0;
var running = false;
var task = null;

// Declare inlets/outlets
inlets = 1;
outlets = 1;

function loadbang() {
    // Initialize buffer references after a short delay
    task = new Task(initBuffers, this);
    task.schedule(100);
}

function initBuffers() {
    bufL = new Buffer(bufferNameL);
    bufR = new Buffer(bufferNameR);
    
    if (bufL.framecount() > 0) {
        BUFFER_SIZE = bufL.framecount();
        post("Buffer initialized: " + BUFFER_SIZE + " frames\n");
    }
}

function start() {
    if (!bufL || !bufR) {
        initBuffers();
    }
    running = true;
    readPos = 0;
    post("Sample streaming started\n");
}

function stop() {
    running = false;
    post("Sample streaming stopped\n");
}

function bang() {
    if (!running) return;
    if (!bufL || !bufR) {
        initBuffers();
        if (!bufL || !bufR) return;
    }
    
    readChunk();
}

function readChunk() {
    var leftSamples = [];
    var rightSamples = [];
    
    // Read CHUNK_SIZE samples from each buffer
    for (var i = 0; i < CHUNK_SIZE; i++) {
        var pos = (readPos + i) % BUFFER_SIZE;
        leftSamples.push(bufL.peek(1, pos, 1));  // channel 1, frame pos, 1 sample
        rightSamples.push(bufR.peek(1, pos, 1));
    }
    
    // Advance read position
    readPos = (readPos + CHUNK_SIZE) % BUFFER_SIZE;
    
    // Format as JSON arrays
    var leftStr = "[" + leftSamples.join(",") + "]";
    var rightStr = "[" + rightSamples.join(",") + "]";
    
    // Output executejavascript command
    outlet(0, "executejavascript", "window.acSampleBatch&&window.acSampleBatch(" + leftStr + "," + rightStr + ")");
}

// Set chunk size
function chunksize(n) {
    if (n > 0 && n <= 4096) {
        CHUNK_SIZE = n;
        post("Chunk size set to " + n + "\n");
    }
}

// Set buffer names
function buffers(nameL, nameR) {
    bufferNameL = nameL;
    bufferNameR = nameR;
    initBuffers();
}
