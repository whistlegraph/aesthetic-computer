// Blank, 22.10.03.15.13

// Dear Piecemaker,

// 🤸 Welcome to aesthetic.computer!

// There aren't really any docs right now,
// but you can `console.log($api);` or throw in
// a `debugger` statement anywhere in a function
// to explore what's available.

// But what's even faster than that is to just ask
// me directly how to access and control the features
// you're interested in using for a piece.

// Run this piece by typing `@piecemaker/blank`
// on aesthetic.computer or 4esthetic.com for short!

// And debug in the dev console!

// Jeffrey (me@jas.life / digitpain#2262 / @digitpain)

// 🥾 Boot (Runs once before first paint and sim)
function boot({ resize, wipe, ink, line, pan, html }) {
  resize(128, 127);
  wipe(255, 0, 0);

  html`
    <audio id="audio" src="https://example.com/audio.mp3" controls></audio>
    <button id="record">Start Recording</button>
    <div id="zoom">
      <button id="zoom-in">+</button> 5 <button id="zoom-out">-</button>
    </div>
    <script>
      navigator.mediaDevices
        .getUserMedia({ audio: true, video: true })
        .then(function (stream) {
          var audio = document.getElementById("audio");
          audio.play();
          var audioStream = audio.captureStream();
          var combinedStream = new MediaStream([
            stream.getAudioTracks()[0],
            stream.getVideoTracks()[0],
            audioStream.getAudioTracks()[0],
          ]);
          var track = combinedStream.getVideoTracks()[0];
          var recorder = new MediaRecorder(combinedStream);
          var recordButton = document.getElementById("record");
          recordButton.addEventListener("click", function () {
            recorder.start();
            recordButton.style.display = "none";
            setTimeout(function () {
              recorder.stop();
            }, 15000);
          });
          var zoomIn = document.getElementById("zoom-in");
          zoomIn.addEventListener("touchstart", function () {
            track.applyConstraints({
              advanced: [{ zoom: track.getConstraints().zoom.max }],
            });
          });
          zoomIn.addEventListener("touchend", function () {
            track.applyConstraints({
              advanced: [{ zoom: track.getConstraints().zoom.min }],
            });
          });
          var zoomOut = document.getElementById("zoom-out");
          zoomOut.addEventListener("touchstart", function () {
            track.applyConstraints({
              advanced: [{ zoom: track.getConstraints().zoom.min }],
            });
          });
          zoomOut.addEventListener("touchend", function () {
            track.applyConstraints({
              advanced: [{ zoom: track.getConstraints().zoom.max }],
            });
          });
        });
    </script>
  `;
}

let x = 0;

// 🎨 Paint (Executes every display frame)
function paint({ ink, line, pan, unpan, pen }) {
  x += 0.1;
  ink();
  pan(pen.x, pen.y);
  line(0, 0, 50, 50);
  line(0, 50, 50, 0);
  unpan();
}

/*

// ✒ Act (Runs once per user interaction)
function act({ event }) {
  // Respond to user input here.
}

// 🧮 Sim(ulate) (Runs once per logic frame (120fps locked)).
function sim($api) {
  // Crunch numbers outside of rendering here.
}

// 💗 Beat (Runs once per bpm, starting when the audio engine is activated.)
function beat($api) {
  // Make sound here.
}

// 👋 Leave (Runs once before the piece is unloaded)
function leave($api) {
  // Pass data to the next piece here.
}
*/

// 📚 Library (Useful functions used throughout the piece)
// ...

export { boot, paint };

//export { boot, sim, paint, act, beat, leave };
