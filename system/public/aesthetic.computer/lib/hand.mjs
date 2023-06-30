// Hand, 
import { radians, map } from "../lib/num.mjs";
const { cos, sin } = Math;

/* #region 🏁 todo
  - [] Add flag to enable video feedback to constructor 
  - [] Fix video orientation issues.
  - [] Fix gesture interactions 
#endregion */

export class HandInput {
    timop = [];
    #lastOrigin = [];
    #origin = { x: 0, y: 0, z: 0 }; // Wrist
    #handPalette = {
      w: "#FFFFFFFF", // Wrist, white
      t: [0, 170, 200], // Thumb, teal
      i: [75, 0, 130], // Index, Indigo
      m: "magenta", // Middle finger, magenta
      o: "orange", // Ring, orange
      p: "pink", // Pinky, pink
    };
    #vid; // Stores a video frame.
    #scaled = []; // Stores the screen scaled mediapipe points.
  
    constructor() {
      console.log("New hand input object!");
    }

    sim({hand: { mediapipe }, screen: { width, height}}) {
      // Calculate Hand-tracked 2D Coordinates
      this.#scaled = mediapipe.screen.map((coord) => [
        coord.x * width,
        coord.y * height,
        coord.z,
      ]);
      if (this.#scaled.length > 0) {
        this.timop = [this.#scaled[4], this.#scaled[8], this.#scaled[12], this.#scaled[16], this.#scaled[20]];
      } else {
        this.timop = [];
      }
    }
  
    paint({
      hand: { mediapipe },
      layer,
      wipe,
      ink,
      box,
      pan,
      unpan,
      screen: { created, resized, width, height },
      pen,
      paste,
      paintCount,
      video,
      num,
    }, options) {
      // Start video feed once for webcam hand-tracking on mobile and desktop.
      // (And recalibrate if resized.)
      if (created || resized) {
        this.#vid = video(created ? "camera" : "camera:update", {
          hidden: !options?.video, // Toggle to stop pulling frames.
          hands: true,
          facing: "user", // || "environment",
          width,
          height,
        });
      }

      if (options?.video) {
        const frame = this.#vid(); // Enables video feedback.
        frame ? paste(frame) : wipe(0, 64, 0);
      }

      let timop; 
      const scaled = this.#scaled;

      if (scaled.length > 0) {
        this.#lastOrigin[0] = scaled[0][0];
        this.#lastOrigin[1] = scaled[0][1];
      }

      // Draw scaled coordinates.
      const fadedPalette = { w: 64, t: 64, i: 64, m: 64, o: 64, p: 64 };
      let palette = options?.faded ? fadedPalette : this.#handPalette;

      const boxSize = 5;
      const boxType = "fill*center";
  
      if (scaled.length > 0) {
        // A. Draw lines
        ink(palette.w).poly([
          scaled[0],
          scaled[5],
          scaled[9],
          scaled[13],
          scaled[17],
          scaled[0],
        ]);
  
        ink(palette.t).poly([
          scaled[0],
          scaled[1],
          scaled[2],
          scaled[3],
          scaled[4],
        ]);
  
        ink(palette.i).poly([
          scaled[5],
          scaled[6],
          scaled[7],
          scaled[8],
        ]);

        ink(palette.m).poly([
          scaled[9],
          scaled[10],
          scaled[11],
          scaled[12],
        ]);

        ink(palette.o).poly([
          scaled[13],
          scaled[14],
          scaled[15],
          scaled[16],
        ]);

        ink(palette.p).poly([
          scaled[17],
          scaled[18],
          scaled[19],
          scaled[20],
        ]);
  
        // B. Loop over the scaled points and draw the boxes.
        scaled.forEach((coord, index) => {
          if (index >= 18) {
            ink(palette.p); // Pinky
          } else if (index > 13 && index < 17) {
            ink(palette.o);
          } else if (index > 9 && index < 13) {
            ink(palette.m);
          } else if (index > 5 && index < 9) {
            ink(palette.i);
          } else if (index > 0 && index < 5) {
            ink(palette.t);
          } else {
            if (options?.faded) {
              ink(64);
            } else {
              if (mediapipe.hand === "left") ink(200, 200, 255);
              if (mediapipe.hand === "right") ink(200, 255, 200);
            }
          }
          box(coord[0], coord[1], boxSize, boxType);
        });
 
        // Interactions
        timop = [scaled[4], scaled[8], scaled[12], scaled[16], scaled[20]];
        const { interactions, contactDistances } = this.#touching(timop, num);

        if (options?.faded) return; // Don't paint interactions if faded.
  
        const letterColors = {
          //default populated
          t: "green",
          ti: "red",
          tm: "orange",
          to: "gold",
          tp: "goldenrod",
          im: "green",
          io: "olivedrab",
          ip: "blue",
          mo: "cornflowerblue",
          mp: "darkcyan",
          op: "darkblue",
          tim: "darkslateblue",
          tio: "darkorchid",
          tip: "darkmagenta",
          tmo: "darkviolet",
          tmp: "fuchsia",
          top: "deeppink",
          imo: "hotpink",
          imp: "indianred",
          iop: "lightcoral",
          mop: "lightpink",
          timo: "lightseagreen",
          tmop: "pink",
          timp: "plum",
          tiop: "teal",
          imop: "mediumslateblue",
          timop: "chartreuse",
        };
  
        const outputColors = { ...this.#handPalette }; // Set from above.
  
        // Overwrite the default color on interacting fingers
        if (interactions.length > 0) {
          for (let i = 0; i < interactions.length; i++) {
            let touchLabels = Object.keys(interactions[i].data);
            let comboColor = letterColors[touchLabels.join("")];
            touchLabels.forEach((label) => {
              // Read from letterColors and output the value to individual finger.
              outputColors[label] = comboColor;
            });
          }
        }
  
        // Then, color the fingers
        [..."timop"].forEach((letter, index) => {
          const coord = timop[index].slice(); // Make a copy of the coords.
          coord[0] += -3;
          coord[1] += -5;
          ink(outputColors[letter]).circle(
            ...timop[index].slice(0, -1),
            contactDistances[index] / 2,
            true
          );
          ink("white").write(letter, coord, outputColors[letter]);
        });
  
        // loop through timop and draw all the letters
      } else {
        // 2. Or... default to a generated model of a hand.
        const osc = Math.sin(paintCount * 0.1); // Oscillate a value based on frame.
        // Build base wrist geometry.
        const w = [
          this.#origin,
          this.#crawl(this.#origin, 40 + 2 * osc, 10),
          this.#crawl(this.#origin, 45 + -2 * osc, 25),
          this.#crawl(this.#origin, 50 + 2 * osc, 40),
          this.#crawl(this.#origin, 55 + -2 * osc, 55),
        ];
  
        // Build hand geometry with fingers.
        const hand = {
          w,
          t: this.#digit(w[0], 4, -30, -10 * osc),
          i: this.#digit(w[1], 3, -8, -10 * osc),
          m: this.#digit(w[2], 3, 0, -10 * osc),
          o: this.#digit(w[3], 3, 7, -10 * osc),
          p: this.#digit(w[4], 3, 20, -10 * osc),
        };
  
        const o = { x: -24 + 2 * osc, y: 16 + 2 * osc }; // Offsets and oscilates the entire hand
  
        if (this.#lastOrigin.length > 0) {
          pan(this.#lastOrigin[0] + o.x, this.#lastOrigin[1] + o.y);
        } else {
          pen
            ? pan(pen.x + o.x, pen.y + o.y)
            : pan(width / 2 + o.x, height / 2 + o.y);
        }
        // 🅰️ Hand Lines & Points
        // Draw each component (lines and boxes) of wrist, followed by each of digit.
        ["w", "t", "i", "m", "o", "p"].forEach((char, i) => {
          layer(0); // Lines always under boxes.
          if (char === "w") {
            ink(this.#handPalette.w).poly([...w, w[0]]); // Closed polygon for wrist.
          } else {
            ink(this.#handPalette[char]).poly([w[i - 1], ...hand[char]]);
          }
          layer(1); // Always draw the boxes on top.
          ink(this.#handPalette[char]);
          for (let coord of hand[char]) box(coord.x, coord.y, boxSize, boxType);
        });
  
        unpan(); // Reset the translation.
      }
    }
  
    act({ event }) {
      if (event.is("move")) {
        //anytime a mouse moves
        this.#lastOrigin.length = 0;
      }
    }
  
    // Crawl a point {x, y} dist amount in a direction, returning the new position.
    #crawl(p, dist, dir = 0) {
      dir = radians(dir - 90); // Set 0 degrees to up, convert to radians.
      return { x: p.x + dist * cos(dir), y: p.y + dist * sin(dir) };
    }
  
    // Generate points for a digit given an orientation (deg).
    // from: { x, y }, segCount: n, deg, curve
    #digit(from, segCount, deg = 0, curve = 0) {
      const segs = [];
      let gap = 18;
      for (let s = 0; s < segCount; s += 1) {
        if (s === 0) {
          segs.push(this.#crawl(from, gap, deg));
        } else {
          deg += curve; // Curve a bit on each seg.
          gap *= 0.89; // Decrease gap as well.
          segs.push(this.#crawl(segs[s - 1], gap, deg)); // Crawl from previous seg.
        }
      }
      return segs;
    }
  
    // Track interactions between finger tips
    // Params: Ordered TIMOP tip points, num API
    // Returns: Array of collections of touching tips.
    #touching(tips, num) {
      let touchedTips = [];
      let timop = ["t", "i", "m", "o", "p"];
      let touchGroup = 0;
      let contactDistances = [];
  
      for (let tip = 0; tip < 5; tip++) {
        contactDistances[tip] = map(tips[tip][2], -0.03, -1, 8, 60);
        for (let tc = tip + 1; tc < 5; tc++) {
          const currentTip = tips[tip];
          const tipToCheck = tips[tc];
  
          let distance = num.dist(
            currentTip[0],
            currentTip[1],
            tipToCheck[0],
            tipToCheck[1]
          );
  
          if (distance < contactDistances[tip]) {
            // Create a "touch" to collect all touching tips, starting with these
            const tipId1 = timop[tip];
            const tipId2 = timop[tc];
            let added = false;
  
            touchedTips.forEach((touchedTip) => {
              // Search touchedTips to see if the keys tipId1 is present
              const keys = Object.keys(touchedTip.data);
              if (keys.includes(tipId1)) {
                //if they are, only add tipToCheck
                touchedTip.data[tipId2] = tipToCheck;
                added = true;
              }
            });
  
            if (!added) {
              //Create new touch collection when not updating previous touch
              const touch = { data: {}, group: touchGroup };
              touch.data[tipId1] = currentTip;
              touch.data[tipId2] = tipToCheck;
              touchedTips.push(touch);
              touchGroup++;
            }
            break;
          }
        }
      }
      return { interactions: touchedTips, contactDistances };
    }
  }
  