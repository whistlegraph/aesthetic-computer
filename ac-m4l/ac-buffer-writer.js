// AC Buffer Writer for Max for Live
// Writes incoming samples to buffer~ objects for playback via wave~

autowatch = 1;
inlets = 1;
outlets = 2;

// Ring buffer settings - 1 second at 48kHz
var BUFFER_SIZE_MS = 1000;
var sampleRate = 48000;
var bufferSizeSamples = 48000;

// Write position
var writePos = 0;

// Buffer references
var leftBuf = null;
var rightBuf = null;

var debugCount = 0;
var initialized = false;
var totalSamplesWritten = 0;

function initBuffers() {
    if (initialized) return;
    
    leftBuf = new Buffer("acleft");
    rightBuf = new Buffer("acright");
    
    if (leftBuf && rightBuf) {
        // Get actual buffer size
        bufferSizeSamples = leftBuf.framecount();
        post("AC Buffer Writer: Buffers found! Size: " + bufferSizeSamples + " frames\n");
        
        // Clear buffers
        leftBuf.send("clear");
        rightBuf.send("clear");
        
        initialized = true;
        outlet(0, "initialized", bufferSizeSamples);
    } else {
        post("AC Buffer Writer: Waiting for buffers...\n");
    }
}

// Called by route: first arg is count, rest is alternating L/R samples
function list() {
    var args = arrayfromargs(arguments);
    
    if (args.length < 1) return;
    
    // Initialize on first message
    if (!initialized) {
        initBuffers();
        if (!initialized) return;
    }
    
    var numSamples = args[0];
    
    // Debug: show what we received
    if (totalSamplesWritten === 0) {
        post("AC Buffer: First samples! count=" + numSamples + " args.length=" + args.length + "\n");
    }
    
    // Validate data
    if (args.length < 1 + numSamples * 2) {
        post("AC Buffer: Not enough args! Need " + (1 + numSamples * 2) + " got " + args.length + "\n");
        return;
    }
    
    // Write samples to buffers using poke
    for (var i = 0; i < numSamples; i++) {
        var leftSample = args[1 + i];
        var rightSample = args[1 + numSamples + i];
        
        // poke(channel, frame, value)
        leftBuf.poke(1, writePos, leftSample);
        rightBuf.poke(1, writePos, rightSample);
        
        writePos++;
        if (writePos >= bufferSizeSamples) {
            writePos = 0;
        }
    }
    
    totalSamplesWritten += numSamples;
    
    // Debug every 50 messages
    debugCount++;
    if (debugCount % 50 === 0) {
        post("AC Buffer: wrote " + numSamples + " @ pos " + writePos + " (total: " + totalSamplesWritten + ")\n");
    }
}

// Handle dictionary input (from node.script JSON output)
function anything() {
    var msg = messagename;
    var args = arrayfromargs(arguments);
    
    // Debug incoming message type
    debugCount++;
    if (debugCount <= 5) {
        post("AC Buffer: msg='" + msg + "' args.length=" + args.length + "\n");
    }
    
    // Initialize on first message
    if (!initialized) {
        initBuffers();
        if (!initialized) return;
    }
    
    // Handle "samples" message from route object
    if (msg === "samples") {
        // Args should be: numSamples, left[0], left[1], ..., right[0], right[1], ...
        // Already handled by list() via route, but also handle here
        list.apply(this, args);
    }
}

function bang() {
    initBuffers();
    post("AC Buffer Writer: bang received, initialized=" + initialized + "\n");
}

function loadbang() {
    post("AC Buffer Writer: loadbang\n");
    // Defer initialization to let buffers load first
    var task = new Task(initBuffers);
    task.schedule(500);
}
