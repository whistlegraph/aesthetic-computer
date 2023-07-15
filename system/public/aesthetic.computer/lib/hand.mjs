// Hand,
import { radians, map } from "../lib/num.mjs";
const { cos, sin } = Math;

/* #region 🏁 todo
  - [] Clean Up / Refactor gesture interaction code 
  - [] Fix video orientation issues. (Need a video enabled "goal" piece.)
  + Done
  - [x] Add flag to enable video feedback to paint function. 
  - [X] Fix gesture interactions.  (Need a piece that will require it.)

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

  constructor() {}

  sim({ hand: { mediapipe }, screen: { width, height } }) {
    // Calculate Hand-tracked 2D Coordinates
    this.#scaled = mediapipe.screen.map((coord) => [
      coord.x * width,
      coord.y * height,
      coord.z,
    ]);
    if (this.#scaled.length > 0) {
      this.timop = [
        this.#scaled[4],
        this.#scaled[8],
        this.#scaled[12],
        this.#scaled[16],
        this.#scaled[20],
      ];
    } else {
      this.timop = [];
    }
    // Return scaled points in case they are needed in sim.
    return this.#scaled?.length > 0 ? this.#scaled : undefined;
  }

  paint(
    {
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
    },
    options
  ) {
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

    if (options?.hidden) return;

    // TODO: Is everything underneath this, dependent just on the point data?
    // - [] Abstract hand drawing so remote hands can be drawn from a sata set.
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

      ink(palette.i).poly([scaled[5], scaled[6], scaled[7], scaled[8]]);

      ink(palette.m).poly([scaled[9], scaled[10], scaled[11], scaled[12]]);

      ink(palette.o).poly([scaled[13], scaled[14], scaled[15], scaled[16]]);

      ink(palette.p).poly([scaled[17], scaled[18], scaled[19], scaled[20]]);

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

      // console.log(this.#touching(timop, num));
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
        // console.log(interactions[0], interactions[1], interactions[2]);
        for (let i = 0; i < interactions.length; i++) {
          let touchLabels = Object.keys(interactions[i]);
          const order = ["t", "i", "m", "o", "p"];
          let newTL = touchLabels.sort(
            (a, b) => order.indexOf(a) - order.indexOf(b)
          );
          let comboColor = letterColors[newTL.join("")];
          newTL.forEach((label) => {
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
    let touchedPairs = [];
    let timop = ["t", "i", "m", "o", "p"];
    let contactDistances = [];

    // Go through each tip in order from TIMOP...
    for (let tip = 0; tip < 5; tip++) {
      // Defining a hitbox for each tip.
      contactDistances[tip] = map(tips[tip][2], -0.03, -1, 24, 60);

      // Start looking at the next tip after this one to the end.
      // So for T, we look at IMOP, and for I, we look at MOP.
      for (let tc = tip + 1; tc < 5; tc++) {
        // We must already have a touchedTips with "TI".
        const currentTip = tips[tip]; // T: 0
        const tipToCheck = tips[tc]; // M: 2
        let distance = num.dist(
          currentTip[0],
          currentTip[1],
          tipToCheck[0],
          tipToCheck[1]
        );

        if (distance < contactDistances[tip]) {
          const tipA = timop[tip];
          const tipB = timop[tc];
          let touchFound = false;
          // If the current pair of tips is not part of an existing touch, create a new touch
          if (!touchFound) {
            const touch = {};
            touch[tipA] = currentTip;
            touch[tipB] = tipToCheck;
            touchedPairs.push(touch);
          }
        }
      }
    }
    // TODO: Current Problem: when TIMOP, it is splitting into two groups: TPO, IMOP
    let finalTouchedTips = [];
    //const finalFinalTouchedTips = [];

    touchedPairs.forEach((tip) => {
      // console.log("current tip:", tip);
      const lastAdded = finalTouchedTips[finalTouchedTips.length - 1];
      let added = false;

      if (!lastAdded) {
        // if lastAdded is empty...
        finalTouchedTips.push(tip); // Nothing to compare, then add this pair.
        added = true;
      } else {
        // Check all final touched tips for potential duplicates.
        finalTouchedTips.forEach((finalTip) => {
          const existingKeys = Object.keys(finalTip);
          // console.log("master keys:", existingKeys);
          const newKeys = Object.keys(tip);

          // if existingKeys includes anything from newKeys
          const hasSharedKeys = existingKeys.some((key) =>
            newKeys.includes(key)
          );
          // console.log("has Shared keys:", hasSharedKeys);

          if (hasSharedKeys) {
            // then get all the keys that need to be added
            const keysToAdd = newKeys.filter(
              (key) => !finalTip.hasOwnProperty(key)
            );
            // console.log("keys to add:", keysToAdd);
            keysToAdd.forEach((key) => (finalTip[key] = tip[key]));
            // then add everything from newKeys to existingKeys.
            added = true;
          }
        });
      }

      if (!added) {
        finalTouchedTips.push(tip);
      }
    });

    let dupe = {};

    if (finalTouchedTips.length > 1) {
      const firstKeys = Object.keys(finalTouchedTips[0]);
      const secondKeys = Object.keys(finalTouchedTips[1]);
      const hasDuplicate = firstKeys.some((key) => secondKeys.includes(key));

      if (hasDuplicate) {
        const mergedArrayKeys = [...new Set([...firstKeys, ...secondKeys])];

        mergedArrayKeys.forEach((key) => {
          if (finalTouchedTips[0].hasOwnProperty(key)) {
            dupe[key] = finalTouchedTips[0][key];
          } else {
            dupe[key] = finalTouchedTips[1][key];
          }
        });
        finalTouchedTips = [dupe];
      }
    }

    return { interactions: finalTouchedTips, contactDistances };
  }
}
