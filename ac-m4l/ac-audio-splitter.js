// AC Audio Splitter for Max for Live
// Takes packed sample data [numSamples, ...left, ...right]
// Outputs separate left and right sample lists to list~ objects

autowatch = 1;
inlets = 1;
outlets = 2; // 0 = left samples, 1 = right samples

var debugCount = 0;

function list() {
    var args = arrayfromargs(arguments);
    
    if (args.length < 1) return;
    
    var numSamples = args[0];
    
    // Validate we have enough data
    var expectedLength = 1 + numSamples * 2;
    if (args.length < expectedLength) {
        post("AC Splitter: Invalid data - got " + args.length + " expected " + expectedLength + "\n");
        return;
    }
    
    // Extract left and right samples
    var leftSamples = [];
    var rightSamples = [];
    
    for (var i = 0; i < numSamples; i++) {
        leftSamples.push(args[1 + i]);
        rightSamples.push(args[1 + numSamples + i]);
    }
    
    // Debug output (every 100 messages)
    debugCount++;
    if (debugCount % 100 === 0) {
        post("AC Splitter: Processed " + numSamples + " samples (msg #" + debugCount + ")\n");
    }
    
    // Output to list~ objects
    outlet(0, leftSamples);
    outlet(1, rightSamples);
}

function bang() {
    post("AC Audio Splitter ready!\n");
}

// Handle other messages
function anything() {
    var msg = messagename;
    post("AC Splitter: Unknown message: " + msg + "\n");
}
