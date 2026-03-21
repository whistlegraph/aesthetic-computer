// Hand,
import { radians, map } from "../lib/num.mjs";
const { cos, sin } = Math;

/* #region 🏁 todo
  - [] When in dummy mode, all interactions should be taken away
  - [] Clean Up / Refactor gesture interaction code 
  - [] Fix video orientation issues. (Need a video enabled "goal" piece.)
  - [] Add indices for data 
  + Done
  - [x] Add flag to enable video feedback to paint function. 
  - [X] Fix gesture interactions.  (Need a piece that will require it.)

#endregion */

export class HandInput {
  api; // Set in the constructor.
  timop = []; // Coordinates of each tip.
  interactions = [];
  #contactDistances = [];
  dummy = true;
  dummyGesture = "palm";
  dummyPoints = [];
  dummyPan; 
  #dummyOsc;
  #lastOrigin = [];
  indices = {t: 4, i: 8, m: 12, o: 16, p: 20};
  #origin = [ 0, 0, 0 ]; // Wrist
  #handPalette = {
    w: "#FFFFFFFF", // Wrist, white
    t: [0, 170, 200], // Thumb, teal
    i: [75, 0, 130], // Index, Indigo
    m: "magenta", // Middle finger, magenta
    o: "orange", // Ring, orange
    p: "pink", // Pinky, pink
  };
  #vid; // Stores a video frame.
  #points = []; // Stores the screen scaled mediapipe points.


  constructor($) {
    this.api = $;
  }

  #tipsFrom(handPoints) {
    return [
      handPoints[this.indices.t],
      handPoints[this.indices.i],
      handPoints[this.indices.m],
      handPoints[this.indices.o],
      handPoints[this.indices.p],
    ];
  }

  sim({ hand: { mediapipe }, screen: { width, height }, simCount, num }) {
    // Calculate Hand-tracked 2D Coordinates
    this.#points = mediapipe.screen.map((coord) => [
      coord.x * width,
      coord.y * height,
      coord.z,
    ]);

    this.timop = this.#tipsFrom(this.#points);

    this.dummy = this.#points.length === 0;

    if (this.dummy === false) {
      this.#lastOrigin[0] = this.#points[0][0];
      this.#lastOrigin[1] = this.#points[0][1];

      // Interactions
      const { interactions, contactDistances } = this.#touching(this.timop, num);
      this.interactions = interactions; // Update the interactions for the class.
      this.#contactDistances = contactDistances;
    }

    if (this.dummy) {
      this.interactions = [];
      // console.log("In Dummy Mode");
      // 2. Or... default to a generated model of a hand.
      const osc = Math.sin(Number(simCount % 120n) * 0.1); // Oscillate a value based on frame.

      const imoAngleCurve = {
        i: [-8, -10 * osc],
        m: [0, -10 * osc],
        o: [7, -10 * osc],
      };

      if (this.dummyGesture === "shaka") {
        imoAngleCurve.i[0] -= 180;
        imoAngleCurve.m[0] -= 180;
        imoAngleCurve.o[0] -= 180;
      }
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
        i: this.#digit(w[1], 3, ...imoAngleCurve.i),
        m: this.#digit(w[2], 3, ...imoAngleCurve.m),
        o: this.#digit(w[3], 3, ...imoAngleCurve.o),
        p: this.#digit(w[4], 3, 20, -10 * osc),
      };

      this.dummyPoints = [
        hand.w[0],
        ...hand.t,
        hand.w[1],
        ...hand.i,
        hand.w[2],
        ...hand.m,
        hand.w[3],
        ...hand.o,
        hand.w[4],
        ...hand.p,
      ];

      // We need a "standard" property on the class for points.
      // We need a "dummyData" property for additional stuff like `o`.

      this.#points = this.dummyPoints;
      this.#dummyOsc = osc;
    }

    // Return scaled points in case they are needed in sim.
    return this.#points?.length > 0 ? this.#points : undefined;
  }

  paint(
    {
      hand: { mediapipe },
      layer,
      wipe,
      ink,
      poly,
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

    // Draw scaled coordinates.
    const fadedPalette = { w: 64, t: 64, i: 64, m: 64, o: 64, p: 64 };
    let palette = options?.faded ? fadedPalette : this.#handPalette;

    const boxSize = 5;
    const boxType = "fill*center";

    if (this.dummy === false) {
      const points = this.#points;

      const interactions = this.interactions;
      const contactDistances = this.#contactDistances;

      // A. Draw lines
      ink(palette.w).poly([
        points[0],
        points[5],
        points[9],
        points[13],
        points[17],
        points[0],
      ]);

      ink(palette.t).poly([
        points[0],
        points[1],
        points[2],
        points[3],
        points[4],
      ]);

      ink(palette.i).poly([points[5], points[6], points[7], points[8]]);
      ink(palette.m).poly([points[9], points[10], points[11], points[12]]);
      ink(palette.o).poly([points[13], points[14], points[15], points[16]]);
      ink(palette.p).poly([points[17], points[18], points[19], points[20]]);

      // B. Loop over the scaled points and draw the boxes.
      points.forEach((coord, index) => {
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
        const coord = this.timop[index].slice(); // Make a copy of the coords.
        coord[0] += -3;
        coord[1] += -5;
        ink(outputColors[letter]).circle(
          ...this.timop[index].slice(0, -1),
          contactDistances[index] / 2,
          true
        );
        ink("white").write(letter, coord, outputColors[letter]);
      });

      // loop through timop and draw all the letters
    } else if (this.dummy === true && this.#points.length > 0) {
      // Offsets and oscilates the entire hand
      const o = { x: -24 + 2 * this.#dummyOsc, y: 16 + 2 * this.#dummyOsc };

      if (this.#lastOrigin.length > 0) {
        this.dummyPan = [this.#lastOrigin[0] + o.x, this.#lastOrigin[1] + o.y];

      } else {
        pen
          ? this.dummyPan = [pen.x + o.x, pen.y + o.y]
          : this.dummyPan = [width / 2 + o.x, height / 2 + o.y];

      }
      
      pan(...this.dummyPan); 

      // TODO: Eliminate w, and hand, and instead iterate through this.#points;

      const points = this.#points;

      // console.log("Dummy points to render:", points);

      // 🅰️ Hand Lines & Points
      // Draw each component (lines and boxes) of wrist, followed by each of digit.
      ["w", "t", "i", "m", "o", "p"].forEach((char, i) => {
        layer(0); // Lines always under boxes.
        let currentInd = [];
        const indices = {
          t: [0, 5],
          i: [5, 9],
          m: [9, 13],
          o: [13, 17],
          p: [17, 21],
        };
        if (char === "w") {
          if (options?.faded) {
            ink(64).poly([
              points[0],
              points[5],
              points[9],
              points[13],
              points[17],
              points[0],
            ]); // Closed polygon for wrist.
          } else {
            ink(this.#handPalette.w).poly([
              points[0],
              points[5],
              points[9],
              points[13],
              points[17],
              points[0],
            ]); // Closed polygon for wrist.
          }
          // console.log(points[0]);
        } else {
          let currentColor;
          currentInd = indices[char];
          if (options?.faded) { currentColor = 64 }
          else {
            currentColor = this.#handPalette[char];
          }
          const currentPoints = points.slice(currentInd[0], currentInd[1]);
          layer(0);
          ink(currentColor);
          poly(currentPoints);
          layer(1); // TODO: How does layering work with ink?
          ink(currentColor);
          currentPoints.forEach((p) => box(...p, boxSize, boxType));
        }
        layer(1); // Always draw the boxes on top.
        ink(this.#handPalette[char]);
        // for (let coord of hand[char]) box(coord.x, coord.y, boxSize, boxType);
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
    // return { x: p.x + dist * cos(dir), y: p.y + dist * sin(dir) };
    return [ p[0] + dist * cos(dir), p[1] + dist * sin(dir) ];
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

  gesture(handPoints) {
    // TODO: Get tips from handPoints.
    return this.#touching(this.#tipsFrom(handPoints), this.api.num).interactions;
  }

  // Track interactions between finger tips
  // Params: Ordered TIMOP tip points, num API
  // Returns: Array of collections of touching tips.
  #touching(tips, num) {
    let touchedPairs = [];
    const timop = ["t", "i", "m", "o", "p"];
    let contactDistances = [];

    // Go through each tip in order from TIMOP...
    for (let tip = 0; tip < 5; tip++) {
      // Defining a hitbox for each tip.
      contactDistances[tip] = map(tips[tip][2], -0.5, -1, 30, 40);

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
