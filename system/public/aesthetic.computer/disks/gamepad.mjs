// Gamepad, 2024.11.08.02.41.47.840
// Test your gamepad connectivity.

/* 📝 Notes
  - [x] Add user-controlled panning of the surface.
  - [x] Process all the pixels in the `picture`.
  - [x] Add prototype destructive pan and zoom.
*/

const event = [];
const players = {};
const { keys } = Object;
const { abs } = Math;

let picture;

function boot({ wipe, screen, painting }) {
  picture = painting(screen.width, screen.height, ({ wipe }) => wipe("black"));
}

function paint({ wipe, ink, line, screen, paste, page, help: { choose } }) {
  // wipe("black");
  // ink("purple", 64).box(0, 0, screen.width, screen.height);

  // Pan
  //  {
  //const pixels = picture.pixels;
  //const width = picture.width;
  //const height = picture.height;
  //
  //// Create a temporary array to store the updated pixel data
  //const newPixels = new Uint8ClampedArray(pixels.length);
  //
  //// Number of pixels to pan per frame (adjust for speed)
  //const panAmount = 20;
  //
  //// Iterate over each row
  //for (let y = 0; y < height; y++) {
  //  for (let x = 0; x < width; x++) {
  //    // Calculate the source index
  //    const srcX = (x + panAmount) % width; // Wrap around the left edge
  //    const srcIndex = (y * width + srcX) * 4;
  //
  //    // Calculate the destination index
  //    const destIndex = (y * width + x) * 4;
  //
  //    // Copy pixel data from source to destination
  //    newPixels[destIndex] = pixels[srcIndex]; // r
  //    newPixels[destIndex + 1] = pixels[srcIndex + 1]; // g
  //    newPixels[destIndex + 2] = pixels[srcIndex + 2]; // b
  //    newPixels[destIndex + 3] = pixels[srcIndex + 3]; // a
  //  }
  //}
  //
  //// Replace the old pixels with the updated ones
  //picture.pixels.set(newPixels);
  // }

  //const pixels = picture.pixels;
  //const width = picture.width;
  //const height = picture.height;
  //
  //// Create a temporary array to store the zoomed image
  //const newPixels = new Uint8ClampedArray(pixels.length);
  //
  //// Zoom factor (increases over time for continuous zoom)
  //let zoomFactor = 1.02; // 2% zoom per frame, adjust for speed
  //
  //// Center coordinates
  //const centerX = width / 2;
  //const centerY = height / 2;
  //
  //// Iterate over every pixel in the new image
  //for (let y = 0; y < height; y++) {
  //  for (let x = 0; x < width; x++) {
  //    // Map the destination pixel (x, y) to the source image
  //    const srcX = Math.floor((x - centerX) / zoomFactor + centerX);
  //    const srcY = Math.floor((y - centerY) / zoomFactor + centerY);
  //
  //    // Ensure source coordinates are within bounds
  //    if (srcX >= 0 && srcX < width && srcY >= 0 && srcY < height) {
  //      const srcIndex = (srcY * width + srcX) * 4;
  //      const destIndex = (y * width + x) * 4;
  //
  //      // Copy the pixel data
  //      newPixels[destIndex] = pixels[srcIndex]; // r
  //      newPixels[destIndex + 1] = pixels[srcIndex + 1]; // g
  //      newPixels[destIndex + 2] = pixels[srcIndex + 2]; // b
  //      newPixels[destIndex + 3] = 255; //pixels[srcIndex + 3]; // a
  //    } else {
  //      // Fill with black (or transparent) if outside bounds
  //      const destIndex = (y * width + x) * 4;
  //      newPixels[destIndex] = 0; // r
  //      newPixels[destIndex + 1] = 0; // g
  //      newPixels[destIndex + 2] = 0; // b
  //      newPixels[destIndex + 3] = 255; // a
  //    }
  //  }
  //}
  //
  //// Replace the old pixels with the updated ones
  //picture.pixels.set(newPixels);
  //
  //// Update the zoom factor for the next frame
  //zoomFactor *= 1.02; // Gradually increase the zoom

  //const pixels = picture.pixels;
  //const width = picture.width;
  //const height = picture.height;
  //
  //// Create a new array for the rotated pixels
  //const newPixels = new Uint8ClampedArray(pixels.length);
  //
  //// Rotation angle in radians (increment for continuous rotation)
  //let angle = 3.02; // Adjust for desired rotation speed
  //
  //// Precompute sine and cosine for the current angle
  //const cosTheta = Math.cos(angle);
  //const sinTheta = Math.sin(angle);
  //
  //// Center coordinates (rotation pivot)
  //const centerX = Math.floor(width / 2);
  //const centerY = Math.floor(height / 2);
  //
  //// Bilinear interpolation function (ignoring alpha)
  //function bilinearInterpolation(x, y) {
  //  const x0 = Math.floor(x);
  //  const x1 = Math.min(x0 + 1, width - 1);
  //  const y0 = Math.floor(y);
  //  const y1 = Math.min(y0 + 1, height - 1);
  //
  //  const dx = x - x0;
  //  const dy = y - y0;
  //
  //  const idx00 = (y0 * width + x0) * 4;
  //  const idx01 = (y1 * width + x0) * 4;
  //  const idx10 = (y0 * width + x1) * 4;
  //  const idx11 = (y1 * width + x1) * 4;
  //
  //  const pixel = [0, 0, 0]; // Ignore alpha
  //  for (let i = 0; i < 3; i++) {
  //    pixel[i] =
  //      pixels[idx00 + i] * (1 - dx) * (1 - dy) +
  //      pixels[idx10 + i] * dx * (1 - dy) +
  //      pixels[idx01 + i] * (1 - dx) * dy +
  //      pixels[idx11 + i] * dx * dy;
  //  }
  //  return pixel;
  //}
  //
  //// Iterate over every pixel in the output image
  //for (let y = 0; y < height; y++) {
  //  for (let x = 0; x < width; x++) {
  //    // Map the destination pixel to the source image
  //    const dx = x - centerX;
  //    const dy = y - centerY;
  //
  //    // Apply rotation transformation
  //    const srcX = centerX + dx * cosTheta - dy * sinTheta;
  //    const srcY = centerY + dx * sinTheta + dy * cosTheta;
  //
  //    // Ensure the source coordinates are within bounds
  //    if (srcX >= 0 && srcX < width && srcY >= 0 && srcY < height) {
  //      const destIndex = (y * width + x) * 4;
  //
  //      // Perform bilinear interpolation to get the rotated pixel
  //      const pixel = bilinearInterpolation(srcX, srcY);
  //      newPixels[destIndex] = pixel[0]; // r
  //      newPixels[destIndex + 1] = pixel[1]; // g
  //      newPixels[destIndex + 2] = pixel[2]; // b
  //      newPixels[destIndex + 3] = 255; // Alpha set to fully opaque
  //    } else {
  //      // Fill with black if outside bounds
  //      const destIndex = (y * width + x) * 4;
  //      newPixels[destIndex] = 0; // r
  //      newPixels[destIndex + 1] = 0; // g
  //      newPixels[destIndex + 2] = 0; // b
  //      newPixels[destIndex + 3] = 255; // Alpha set to fully opaque
  //    }
  //  }
  //}
  //
  //// Replace the original pixels with the rotated image
  //picture.pixels.set(newPixels);

  // Increment the angle for the next frame

  paste(picture);

  ink("yellow").write(event[0], { x: 6, y: 20 });
  ink("pink").write(event[1], { x: 6, y: 20 + 12 });

  keys(players).forEach((index) => {
    const player = players[index];
    const len = 4;

    ink(player.color).circle(player.x, player.y, len * 2, "center");

    ink(player.painting ? undefined : "white").line(
      player.x,
      player.y,
      player.x + player.xvel * len,
      player.y + player.yvel * len,
    );

    if (player.painting) {
      page(picture);
      ink(player.color, 192).circle(player.x, player.y, len * 2, "center");
      page(screen);
    }
  });
}

function act({ event: e, screen, sound }) {
  // Respond to user input here.
  if (e.is("gamepad")) {
    [0, 1].forEach((index) => {
      if (e.is(`gamepad:${index}`)) {
        // Instantiate a p1 if they don't exist.
        if (!players[e.gamepad]) {
          players[e.gamepad] = {
            x: screen.width / 2,
            y: screen.height / 2,
            xvel: 0,
            yvel: 0,
            xforce: 0,
            yforce: 0,
            xdec: 0.93,
            ydec: 0.93,
            color: "green",
            painting: false,
          };
        }

        const player = players[e.gamepad];

        event[index] = e.name;

        if (e.is(`gamepad:${index}:axis`)) {
          event[index] += ":" + e.value.toFixed(2);
          // Left Stick
          if (e.axis === 0) player.xforce = e.value; // X
          if (e.axis === 1) player.yforce = e.value; // Y
        }

        if (e.is(`gamepad:${index}:button`)) {
          if (e.button === 0 && e.action === "push") player.color = "green"; // A
          if (e.button === 1 && e.action === "push") player.color = "red"; // B
          if (e.button === 2 && e.action === "push") player.color = "blue"; // X
          if (e.button === 3 && e.action === "push") player.color = "yellow"; // Y
          if (e.button === 6) {
            player.painting = e.action === "push";
            // sound.synth();
          }
        }
      }
    });
  }
}

function sim({ screen }) {
  keys(players).forEach((index) => {
    const player = players[index];
    if (abs(player.xforce / 6) > 0.01) player.xvel += player.xforce / 6;
    if (abs(player.yforce / 6) > 0.01) player.yvel += player.yforce / 6;

    player.xvel *= player.xdec;
    player.yvel *= player.ydec;

    player.x += player.xvel;
    player.y += player.yvel;

    if (abs(player.xforce) < 0.35) player.xforce *= player.xdec;
    if (abs(player.yforce) < 0.35) player.yforce *= player.ydec;

    if (player.x < 0) player.x = screen.width + player.x;
    if (player.x > screen.width) player.x = player.x - screen.width;
    if (player.y < 0) player.y = screen.height + player.y;
    if (player.y > screen.height) player.y = player.y - screen.height;
  });
}

// 📚 Library

// function boot() {
// Runs once at the start.
// }

// function beat() {
//   // Runs once per metronomic BPM.
// }

// function leave() {
//  // Runs once before the piece is unloaded.
// }

// function preview({ ink, wipe }) {
// Render a custom thumbnail image.
// }

// function icon() {
// Render an application icon, aka favicon.
// }

// ⚠️ Also available: `brush` and `filter`.
