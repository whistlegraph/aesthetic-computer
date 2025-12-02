// AC Audio Bridge for Max for Live
// Receives sample data from jweb and writes to buffer~ objects
// 
// Input: [numSamples, ...leftSamples, ...rightSamples]
// Output: writes to buffer~ ac-left and ac-right

autowatch = 1;
inlets = 1;
outlets = 1;

// Ring buffer settings
var BUFFER_SIZE_MS = 500; // 500ms ring buffer
var sampleRate = 48000; // Will be updated from Max
var bufferSizeSamples = Math.floor(BUFFER_SIZE_MS * sampleRate / 1000);

// Write position tracking
var writePos = 0;
var leftBuffer = null;
var rightBuffer = null;

// Initialize buffers
function loadbang() {
    post("AC Audio Bridge: Initializing...\n");
    
    // Get references to buffers
    leftBuffer = new Buffer("ac-left");
    rightBuffer = new Buffer("ac-right");
    
    // Set buffer sizes (500ms at 48kHz = 24000 samples)
    bufferSizeSamples = Math.floor(BUFFER_SIZE_MS * sampleRate / 1000);
    
    // Resize buffers
    leftBuffer.send("sizeinsamps", bufferSizeSamples);
    rightBuffer.send("sizeinsamps", bufferSizeSamples);
    
    // Clear buffers
    leftBuffer.send("clear");
    rightBuffer.send("clear");
    
    // Enable looping on groove~
    outlet(0, "loop", 1);
    
    post("AC Audio Bridge: Ready! Buffer size: " + bufferSizeSamples + " samples\n");
}

// Handle incoming sample data
function list() {
    var args = arrayfromargs(arguments);
    
    if (args.length < 1) return;
    
    var numSamples = args[0];
    
    // Validate we have enough data
    if (args.length < 1 + numSamples * 2) {
        post("AC Audio Bridge: Invalid sample data\n");
        return;
    }
    
    // Make sure buffers are initialized
    if (!leftBuffer || !rightBuffer) {
        leftBuffer = new Buffer("ac-left");
        rightBuffer = new Buffer("ac-right");
    }
    
    // Write samples to buffers using poke
    for (var i = 0; i < numSamples; i++) {
        var leftSample = args[1 + i];
        var rightSample = args[1 + numSamples + i];
        
        // Write to buffer at current position
        leftBuffer.poke(1, writePos, leftSample);
        rightBuffer.poke(1, writePos, rightSample);
        
        // Advance write position (wrap around)
        writePos = (writePos + 1) % bufferSizeSamples;
    }
    
    // Output status periodically
    // outlet(0, "wrote", numSamples, "pos", writePos);
}

// Handle messages
function anything() {
    var msg = messagename;
    var args = arrayfromargs(arguments);
    
    if (msg === "samplerate") {
        sampleRate = args[0];
        bufferSizeSamples = Math.floor(BUFFER_SIZE_MS * sampleRate / 1000);
        post("AC Audio Bridge: Sample rate set to " + sampleRate + "\n");
    } else if (msg === "clear") {
        writePos = 0;
        if (leftBuffer) leftBuffer.send("clear");
        if (rightBuffer) rightBuffer.send("clear");
        post("AC Audio Bridge: Buffers cleared\n");
    } else if (msg === "status") {
        post("AC Audio Bridge: writePos=" + writePos + "/" + bufferSizeSamples + "\n");
    }
}

// Initialize on script load
function bang() {
    loadbang();
}
