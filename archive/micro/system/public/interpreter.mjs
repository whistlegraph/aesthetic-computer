// TODO: Load the pieces/line.ac file contents.
//       Into a WASM module of my choice...
//       Begin interpreting it / echo it back.

function compileAndRun() {
  console.log("Compiling and running...");
  WebAssembly.instantiateStreaming(fetch("processor.wasm")).then((obj) => {
    console.log(obj.instance.exports.add(1, 2)); // "3"
  });
}

compileAndRun();

// Live reloading:
const source = new EventSource('/events');
source.onmessage = function (e) {
  if (e.data === 'reload') {
    console.log('Reloading page due to changes...');
    compileAndRun();
  }
};

source.onerror = function (e) {
  console.error('Error occurred', e);
  source.close();
};
