// TODO: Move the rest of the nopaint system functions here.

// Also used in `prompt`.
export function nopaint_adjust(screen, sys, painting, store) {
  if (
    screen.width > sys.painting.width ||
    screen.height > sys.painting.height
  ) {
    sys.painting = painting(screen.width, screen.height, (p) => {
      p.wipe(64).paste(sys.painting);
    });
    store["painting"] = sys.painting;
  }
}