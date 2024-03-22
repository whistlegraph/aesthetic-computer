// TODO: Technically I should be able to write this in any
// language that compiles to wasm and can call or read the
// functions in $api.

let renderedOnce = false;

export function update() {
  if (renderedOnce === false) {
    console.log("Update");
  }
}

export function render($api) {
  const { screen, color, clear, line, penChanged } = $api;

  if (renderedOnce && penChanged === false) {
    return false;
  }
  renderedOnce = true;
  
  console.log("Render");

  color(0, 0, 255);
  clear();
  //graph.noise16(); // This is slow.
  color(255, 0, 0, 255);
  line(0, 0, screen.width, screen.height);
}