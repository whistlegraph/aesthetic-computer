// Whistlegraph Cards, 22.12.21.03.21
// 📖 Parameter 1: imab, grow, idni, l8ly, lonr, w0w, ppl, slink, puzz, wiyh, m2w2

// ❓ Made on occasion of Whistlegraph's Feral File exhibition.
// This player currently orchestrates the data for displaying 10 different
// whistlegraphs.

/* #region 🏁 todo
+ Near
- [] Update the url parameter on card-switch similar to freaky-flowers
     so that way you can jump out to another page and then return...
     Or! Make sure to resume video after suspend if opening a tab and returning?
     (Test on iOS)
- [🟡] Write an s3cmd script to rsync the assets directory with the bucket,
     so manual copying and Dropbox can be avoided!
- [] Add more album related cards like a visualizer, or lyrics, the recorder,
     and hit content or TikToks made with the beats.
+ Future
- [] Clean up markup code / think about a potential rewrite of the card
     engine... possibly in a nice, dynamically loaded front-end thing,
     that loads all css externally so that crossorigin="anonymous" can be set
     on the link tag.
- [] Make a better / nicer "resize" event / hide play button and next buttons
     on resize. 
+ Done
- [x] Fix touch-screen link tapping.
- [x] Parse metadata on the server.
- [x] Add some special html link content for the interactive card. 
  - [x] Just link to the pre-save page for Music.
  - [x] Use a custom font from the assets folder / bucket, defined in 
       the CSS here or globally?
- [x] Also link to the Aesthetic CDs. 
- [x] Choose card colors.
- [x] Add in Charlie's intro video along with the poster image for the "score?" 
- [x] Move all wg video assets to an S3 bucket, behind a dev flag that loads them
     locally...
     (Or if they work in production still then use a local web server again?)
  - [x] Write special html code for custom cards for the video / separate out.
       from the old data.
    - [x] Make spinner for m2w2. 
  - [x] Re-encode and add video 'music-2-whistlegraph-2-intro-web'
- [x] Template an "interactive" card type that links somewhere else
      and has a button that isn't the whole card so you can move from
      one to the other.
#endregion */

import { anyKey } from "../lib/help.mjs";

// #region 🧮 data
const shortcuts = {
  imab: "butterfly-cosplayer",
  grow: "time-to-grow",
  idni: "i-dont-need-an-iphone",
  l8ly: "lately-when-i-fly",
  lonr: "loner",
  w0w: "mommy-wow",
  ppl: "people-pleaser",
  sdog: "slinky-dog",
  puzz: "puzzle",
  wiyh: "whats-inside-your-heart",
  m2w2: "music-2-whistlegraph-2",
};

const butterflyCosplayer = {
  title: "Butterfly Cosplayer",
  byline: "Composed by Jeffrey Alan Scudder ⋅ December 29, 2019",
  glow: "rgba(255, 150, 0, 0.4)",
  fuzz: 20n,
  bg: {
    tint: [30, 70, 25], // rgb
    tintAmount: 0.9,
    pixelSaturation: 0.5,
  },
  video: {
    border: 0.25,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(210, 128, 0)",
    boxShadow: "0.5vmin 0.5vmin 2.5vmin rgba(0, 40, 200, 0.83)",
    highlight: "rgba(0, 0, 0, 1)",
  },
  score: {
    border: 0.15,
    outerRadius: 0.15,
    innerRadius: 0.1,
    color: "rgb(170, 170, 150)",
    boxShadow: "1vmin 1vmin 1.5vmin rgba(80, 137, 4, 0.70)",
    highlight: "rgba(0, 0, 0, 1)",
  },
  compilation: {
    border: 0.2,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(20, 20, 20)",
    boxShadow: "1vmin 1vmin 2.5vmin rgba(120, 76, 11, 0.85)",
    highlight: "rgba(120, 120, 120, 1)",
  },
};
const timeToGrow = {
  title: "Time To Grow",
  byline: "Composed by Alex Freundlich ⋅ March 4, 2020",
  glow: "rgba(255, 150, 210, 0.35)",
  fuzz: 18n,
  bg: {
    tint: [20, 10, 3], // rgb
    tintAmount: 0.92,
    pixelSaturation: 0.1,
  },
  video: {
    border: 0.25,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(255, 166, 202)",
    boxShadow: "0.5vmin 0.5vmin 2.5vmin rgba(250, 240, 5, 0.6)",
    highlight: "rgba(127, 127, 127, 1)",
  },
  score: {
    border: 0.15,
    outerRadius: 0.15,
    innerRadius: 0.1,
    color: "rgb(95, 152, 132)",
    boxShadow: "0.5vmin 0.5vmin 2.5vmin rgba(113, 45, 159, 0.85)",
    highlight: "rgba(200, 200, 200, 1)",
  },
  compilation: {
    border: 0.2,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(20, 20, 20)",
    boxShadow: "0.9vmin 0.9vmin 2.5vmin rgba(125, 115, 115, 0.85)",
    highlight: "rgba(120, 120, 120, 1)",
  },
};
const loner = {
  title: "Loner",
  byline: "Composed by Camille Klein ⋅ April 10, 2021",
  glow: "rgba(255, 130, 130, 0)",
  fuzz: 16n,
  bg: {
    tint: [255, 170, 190], // rgb
    tintAmount: 0.9,
    pixelSaturation: 0.8,
  },
  video: {
    border: 0.25,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(10, 38, 88)",
    boxShadow: "0.5vmin 0.5vmin 2vmin rgba(250, 0, 0, 0.9)",
    highlight: "rgba(100, 100, 100, 1)",
  },
  score: {
    border: 0.15,
    outerRadius: 0.15,
    innerRadius: 0.1,
    color: "rgb(233, 195, 2)",
    boxShadow: "0.5vmin 0.5vmin 2.5vmin rgba(132, 64, 12, 0.95)",
    highlight: "rgba(0, 0, 0, 1)",
  },
  compilation: {
    border: 0.2,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(20, 20, 20)",
    boxShadow: "0.7vmin 0.7vmin 2.5vmin rgba(59, 80, 134, 0.75)",
    highlight: "rgba(120, 120, 120, 1)",
  },
};
const iDontNeedAniPhone = {
  title: "I Don't Need an iPhone",
  byline: "Composed by Whistlegraph ⋅ December 9, 2020",
  glow: "rgba(240, 0, 0, 0.45)",
  fuzz: 16n,
  bg: {
    tint: [110, 10, 10], // rgb
    tintAmount: 0.9,
    pixelSaturation: 0.5,
  },
  video: {
    border: 0.25,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(225, 14, 14)",
    boxShadow: "0.35vmin 0.35vmin 2.5vmin rgba(20, 20, 120, 1)",
    highlight: "rgba(0, 0, 0, 1)",
  },
  score: {
    border: 0.15,
    outerRadius: 0.15,
    innerRadius: 0.1,
    color: "rgb(255, 150, 130)",
    boxShadow: "0.5vmin 0.5vmin 2.5vmin rgba(245, 50, 0, 0.88)",
    highlight: "rgba(0, 0, 0, 1)",
  },
  compilation: {
    border: 0.2,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(20, 20, 20)",
    boxShadow: "0.5vmin 0.5vmin 2vmin rgba(255, 0, 80, 0.95)",
    highlight: "rgba(120, 120, 120, 1)",
  },
};
const latelyWhenIFly = {
  title: "Lately When I Fly",
  byline: "Composed by Jeffrey Alan Scudder ⋅ February 1, 2020",
  glow: "rgba(90, 5, 230, 0.95)",
  fuzz: 12n,
  bg: {
    tint: [20, 5, 40], // rgb
    tintAmount: 0.93,
    pixelSaturation: 0.2,
  },
  video: {
    border: 0.25,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(90, 5, 165)",
    boxShadow: "0vmin 0vmin 2.5vmin rgba(0, 15, 115, 0.95)",
    highlight: "rgba(80, 80, 80, 1)",
  },
  score: {
    border: 0.15,
    outerRadius: 0.15,
    innerRadius: 0.1,
    color: "rgb(160, 140, 240)",
    boxShadow: "0.25vmin 0.25vmin 2vmin rgba(161, 68, 153, 0.84)",
    highlight: "rgba(80, 80, 80, 1)",
  },
  compilation: {
    border: 0.2,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(20, 20, 20)",
    boxShadow: "0.5vmin 0.5vmin 4vmin rgba(101, 14, 14, 0.75)",
    highlight: "rgba(120, 120, 120, 1)",
  },
};
const puzzle = {
  title: "Puzzle",
  byline: "Composed by Camille Klein ⋅ October 11, 2021",
  glow: "rgba(48, 200, 252, 0.45)",
  fuzz: 12n,
  bg: {
    tint: [100, 150, 255], // rgb
    tintAmount: 0.7,
    pixelSaturation: 1,
  },
  video: {
    border: 0.25,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(48, 200, 212)",
    boxShadow: "0.5vmin 0.5vmin 3vmin rgba(50, 0, 200, 1)",
    highlight: "rgba(0, 0, 0, 1)",
  },
  score: {
    border: 0.15,
    outerRadius: 0.15,
    innerRadius: 0.1,
    color: "rgb(150, 208, 150)",
    boxShadow: "0.1vmin 0.1vmin 2vmin rgba(255, 100, 100, 1)",
    highlight: "rgba(0, 0, 0, 1)",
  },
  compilation: {
    border: 0.2,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(20, 20, 20)",
    boxShadow: "0.5vmin 0.5vmin 3vmin rgba(100, 80, 228, 0.99)",
    highlight: "rgba(200, 200, 200, 1)",
  },
};
const slinkyDog = {
  title: "Slinky Dog",
  byline: "Composed by Alex Freundlich ⋅ May 13, 2021",
  glow: "rgba(0, 0, 0, 0.75)",
  fuzz: 16n,
  bg: {
    tint: [247 - 40, 247 - 40, 255 - 40], // rgb
    tintAmount: 0.9,
    pixelSaturation: 0.1,
  },
  video: {
    border: 0.25,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(0, 0, 5)",
    boxShadow: "0.75vmin 0.75vmin 2.5v8min rgba(200, 70, 0, 1)",
    highlight: "rgba(180, 180, 180, 1)",
  },
  score: {
    border: 0.15,
    outerRadius: 0.15,
    innerRadius: 0.1,
    color: "rgb(90, 90, 110)",
    boxShadow: "0.1vmin 0.1vmin 2.5vmin rgba(255, 255, 255, 1)",
    highlight: "rgba(0, 0, 0, 1)",
  },
  compilation: {
    border: 0.2,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(20, 20, 20)",
    boxShadow: "0.5vmin 0.5vmin 3vmin rgba(226, 252, 220, 0.7)",
    highlight: "rgba(190, 190, 190, 1)",
  },
};
const mommyWow = {
  title: "Mommy Wow",
  byline: "Composed by Whistlegraph ⋅ July 27, 2021",
  glow: "rgba(255, 200, 0, 0.9)",
  fuzz: 6n,
  bg: {
    tint: [10, 10, 30], // rgb
    tintAmount: 0.85,
    pixelSaturation: 0.9,
  },
  video: {
    border: 0.25,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(80, 82, 110)",
    boxShadow: "0.5vmin 0.5vmin 3vmin rgba(120, 45, 100, 1)",
    highlight: "rgba(0, 0, 0, 1)",
  },
  score: {
    border: 0.15,
    outerRadius: 0.15,
    innerRadius: 0.1,
    color: "rgb(250, 246, 65)",
    boxShadow: "0vmin 0vmin 3vmin rgba(255, 160, 0, 1)",
    highlight: "rgba(180, 180, 180, 1)",
  },
  compilation: {
    border: 0.2,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(20, 20, 20)",
    boxShadow: "0vmin 0vmin 3vmin rgba(255, 160, 0, 0.5)",
    highlight: "rgba(120, 120, 120, 1)",
  },
};
const peoplePleaser = {
  title: "People Pleaser",
  byline: "Composed by Jeffrey Alan Scudder ⋅ January 14, 2021",
  glow: "rgba(190, 80, 220, 0.75)",
  fuzz: 17n,
  bg: {
    tint: [130 - 20, 80 - 20, 80 - 20], // rgb
    tintAmount: 0.94,
    pixelSaturation: 0.2,
  },
  video: {
    border: 0.25,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(190, 176, 205)",
    boxShadow: "0.75vmin 0.75vmin 3vmin rgba(180, 126, 200, 1)",
    highlight: "rgba(0, 0, 0, 1)",
  },
  score: {
    border: 0.15,
    outerRadius: 0.15,
    innerRadius: 0.1,
    color: "rgb(255, 187, 187)",
    boxShadow: "0.25vmin 0.25vmin 2.5vmin rgba(230, 0, 30, 1)",
    highlight: "rgba(0, 0, 0, 1)",
  },
  compilation: {
    border: 0.2,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(20, 20, 20)",
    boxShadow: "0vmin 0vmin 3vmin rgba(200, 150, 206, 0.8)",
    highlight: "rgba(180, 180, 180, 1)",
  },
};
const whatsInsideYourHeart = {
  title: "What's Inside Your Heat?",
  byline: "Composed by Whistlegraph ⋅ February 6, 2021",
  glow: "rgba(0, 0, 245, 1)",
  fuzz: 5n,
  bg: {
    tint: [0, 10, 70], // rgb
    tintAmount: 0.65,
    pixelSaturation: 1,
  },
  video: {
    border: 0.25,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(20, 20, 40)",
    boxShadow: "0.25vmin 0.25vmin 4vmin rgba(255, 10, 10, 0.7)",
    highlight: "rgba(80, 80, 80, 1)",
  },
  score: {
    border: 0.15,
    outerRadius: 0.15,
    innerRadius: 0.1,
    color: "rgb(40, 20, 20)",
    boxShadow: "0.25vmin 0.25vmin 2.5vmin rgba(0, 40, 255, 1)",
    highlight: "rgba(80, 80, 80, 1)",
  },
  compilation: {
    border: 0.2,
    outerRadius: 0.25,
    innerRadius: 0.15,
    color: "rgb(20, 20, 20)",
    boxShadow: "0.2vmin 0.2vmin 2vmin rgba(250, 250, 250, 0.8)",
    highlight: "rgba(120, 120, 120, 1)",
  },
};
const music2Whistlegraph2 = {
  title: "Music 2 Whistlegraph 2",
  byline: "Composed by Charlie Kamin-Allen ⋅ December 22, 2022",
  glow: "rgba(50, 50, 255, 1)",
  fuzz: 8n,
  bg: {
    tint: [100, 100, 250], // rgb
    tintAmount: 0.8,
    pixelSaturation: 1,
  },
  videos: [
    {
      ratio: "1x1",
      slug: "intro",
      border: 0.25,
      outerRadius: 0.25,
      innerRadius: 0.15,
      color: "rgb(20, 20, 60)",
      boxShadow: "0.25vmin 0.25vmin 4vmin rgba(50, 10, 255, 0.7)",
      highlight: "rgba(80, 80, 80, 1)",
    },
  ],
  activities: [
    {
      ratio: "3x2",
      type: "html", // or "iframe"
      markup: `
      <div id="wg-card-listen-now" style="position: absolute; z-index: 1;">
        <h1 id="wg-card-listen-now-links">
          <a href="https://distrokid.com/hyperfollow/whistlegraph/music-2-whistlegraph-2">Songs</a>
          <br>
          <a href="https://shop.whistlegraph.com">Aesthetic CDs</a>
        </h1>
      </div>
      <style>
      #wg-card-listen-now {
        width: 100%;
        height: 100%;
        display: flex;
      }
      #wg-card-listen-now-links {
        margin: auto;
        text-align: center;
        line-height: 18vmin;
        font-size: 10vmin;
      }
      #wg-card-listen-now-links a {
        color: rgb(0, 100, 0);
        display: inline-block;
        margin: auto;
        font-family: YWFTProcessing-Regular, sans-serif;
      }
      #wg-card-listen-now-links a:hover {
        filter: brightness(1.2);
      }
      #wg-card-listen-now-links a:active {
        filter: brightness(1.3);
        transform: scale(1.05);
      }
      </style>
      `,
      // url: "https://open.spotify.com/embed/album/579pQc7XBrVrVnV360zrAU?utm_source=generator",
      backgroundColor:
        "linear-gradient(rgba(166, 233, 133, 0.8), rgba(166, 233, 133, 0.8))",
      backgroundImageSlug: "spines",
      bgImageSize: "100%",
      bgImagePosition: "50% 50%",
      border: 0.25,
      outerRadius: 0.25,
      innerRadius: 0.15,
      color: "rgb(20, 20, 40)",
      boxShadow: "0.25vmin 0.25vmin 4vmin rgba(30, 200, 10, 0.7)",
      highlight: "rgba(80, 80, 80, 1)",
    },
  ],
  images: [
    {
      ratio: "605x961",
      slug: "poster",
      border: 0.25,
      outerRadius: 0.25,
      innerRadius: 0.15,
      color: "rgb(40, 20, 30)",
      boxShadow: "0.25vmin 0.25vmin 4vmin rgba(255, 10, 200, 0.7)",
      highlight: "rgba(80, 80, 80, 1)",
    },
  ],
};

const whistlegraphs = {
  "butterfly-cosplayer": butterflyCosplayer,
  "time-to-grow": timeToGrow,
  loner,
  "i-dont-need-an-iphone": iDontNeedAniPhone,
  "lately-when-i-fly": latelyWhenIFly,
  puzzle: puzzle,
  "slinky-dog": slinkyDog,
  "mommy-wow": mommyWow,
  "people-pleaser": peoplePleaser,
  "whats-inside-your-heart": whatsInsideYourHeart,
  "music-2-whistlegraph-2": music2Whistlegraph2,
};
// #endregion

// Choose a random whistlegraph in case none are specified when the player loads.
const defaultWhistlegraph = anyKey(whistlegraphs);

let whistlegraph;
let fuzzy = false;

// 💁 Metadata (Placed in the head and parsed on the server)
export function meta({ params }) {
  const { whistlegraph } = parse(params);
  return {
    title: whistlegraph.title,
    desc: whistlegraph?.byline,
  };
}

// 🥾 Boot (Runs once before first paint and sim)
function boot({
  cursor,
  params,
  dom: { html },
  net: { waitForPreload },
  resolution,
  debug,
}) {
  // Add assetPath here.
  const assetPath = debug
    ? "/assets/whistlegraph"
    : "https://assets.aesthetic.computer/whistlegraph";

  resolution({ gap: 0 });
  waitForPreload();
  cursor("native");

  // Decide what whistlegraph to use either directly or via `shortcuts`.
  const parsed = parse(params);
  const wg = parsed.wg;
  whistlegraph = parsed.whistlegraph;

  let cardsMarkup = "";

  const playButton = `
    <div id="card-play">
      <img src="${assetPath}/play-circle.svg" crossorigin="anonymous">
      <img src="${assetPath}/play-triangle.svg" crossorigin="anonymous">
    </div>
  `;

  // #region headers
  console.log(
    `%cWhistlegraph → ${whistlegraph.title}`,
    `background-color: rgb(50, 50, 0);
   color: yellow;
   padding: 0 0.25em;
   border-radius: 0.25em;
   font-size: 120%;`
  );

  console.log(
    `%c${whistlegraph.byline}`,
    `background-color: rgb(25, 25, 0);
   color: rgb(100, 100, 0);
   padding: 0 0.25em;
   border-radius: 0.25em;
   font-size: 100%;
   border-left: 0.75px solid rgb(120, 120, 0);
   border-right: 0.75px solid rgb(120, 120, 0);`
  );
  // #endregion

  // TODO: Cards must be added in back to front order, and then
  //       need to be stacked with a z-index.
  //       Only the top card can be "active".

  if (wg === "music-2-whistlegraph-2") {
    let zIndex = 0;

    whistlegraph.images?.forEach((card, index) => {
      cardsMarkup += `
      <div
        class="card-view"
        data-type="image"
        data-outer-radius="${card.outerRadius}"
        data-inner-radius="${card.innerRadius}"
        data-border-setting="${card.border}"
        style="z-index: 1"
      >
        <div class="card" data-type="image" data-ratio="${card.ratio}"
          style="background: ${card.color}; box-shadow: ${card.boxShadow};">
          <img
            class="card-content"
            src="${assetPath}/${wg}/${wg}-${card.slug}.webp"
            crossorigin="anonymous">
          <div
            class="card-outline"
            style="border-color: ${card.highlight}"
          ></div>
        </div>
      </div>
    `;
      zIndex += 1;
    });

    const cardNextMarkup = (color) => `
      <div class="card-next" style="background: ${color}; z-index: 2;">
        <img src="${assetPath}/next-arrow.svg" crossorigin="anonymous">
      </div>
    `;

    whistlegraph.activities?.forEach((card, index) => {
      let markup;
      if (card.type === "iframe") {
        markup = `
          <iframe class="card-content" width="100%" height="100%"
          allow="autoplay; clipboard-write; encrypted-media; fullscreen; picture-in-picture"
          loading="lazy" src="${card.url}"></iframe>
          ${cardNextMarkup(card.color)}
        `;
      } else if (card.type === "html") {
        markup = `
        <div class="card-content interactive-content"
         width="100%" height="100%" style="background: ${card.backgroundColor}">
          ${card.markup}
          <img
           crossorigin="anonymous"
           style="position: absolute; top: 0; left: 0; object-fit: cover;
                  width: 100%; height: 100%; opacity: 0.25;"
           class='card-content-background-image' src="${assetPath}/${wg}/${wg}-${
          card.backgroundImageSlug
        }.webp">
        </div>
        ${cardNextMarkup(card.color)}
        `;
      }

      cardsMarkup += `
        <div
          class="card-view"
          data-type="interactive"
          data-outer-radius="${card.outerRadius}"
          data-inner-radius="${card.innerRadius}"
          data-border-setting="${card.border}"
          style="z-index: ${zIndex}">
          <div class="card" data-type="interactive" data-ratio="${card.ratio}"
           style="background: ${card.color}; box-shadow: ${card.boxShadow};">
            ${markup}
            <div class="card-cover"></div>
            <div
              class="card-outline"
              style="border-color: ${card.highlight}"
            ></div>
          </div>
        </div>
      `;
      zIndex += 1;
    });

    whistlegraph.videos?.forEach((video, index) => {
      cardsMarkup += `
        <div
          class="card-view active"
          data-type="video"
          data-outer-radius="${video.outerRadius}"
          data-inner-radius="${video.innerRadius}"
          data-border-setting="${video.border}"
          style="z-index: ${zIndex}"
        >
          <div class="card" data-type="video" data-ratio="${video.ratio}"
           style="background: ${video.color}; box-shadow: ${video.boxShadow};">
            <video
              class="card-content"
              width="100%"
              height="100%"
              preload="auto"
              playsinline
              disablepictureinpicture
              src="${assetPath}/${wg}/${wg}-${
        video.slug
      }.mp4" crossorigin="anonymous"></video>
            <div class="card-cover"></div>
            <div
              class="card-outline"
              style="border-color: ${video.highlight}"
            ></div>
            ${index === 0 ? playButton : ""}
          </div>
        </div>
      `;
      zIndex += 1;
    });
  }

  // TODO: [] These can be considered legacy now, where "music-2-whistlegraph-2" above represents a mode generic model. 22.12.21.18.22
  if (whistlegraph.compilation) {
    cardsMarkup += `
      <div
        class="card-view"
        data-type="compilation"
        data-outer-radius="${whistlegraph.compilation.outerRadius}"
        data-inner-radius="${whistlegraph.compilation.innerRadius}"
        data-border-setting="${whistlegraph.compilation.border}"
        style="z-index: 0"
      >
        <div class="card" data-type="compilation" data-ratio="720x1280"
          style="background: ${whistlegraph.compilation.color}; box-shadow: ${whistlegraph.compilation.boxShadow};">
          <video
            class="card-content"
            width="100%"
            height="100%"
            preload="auto"
            playsinline
            disablepictureinpicture
            src="${assetPath}/${wg}/${wg}-tt-compilation.mp4"
            crossorigin="anonymous"
            type="video/mp4"
          ></video>
          <div class="card-cover"></div>
          <div
            class="card-outline"
            style="border-color: ${whistlegraph.compilation.highlight}"
          ></div>
        </div>
      </div>
    `;
  }

  if (whistlegraph.score) {
    cardsMarkup += `
      <div
        class="card-view"
        data-type="score"
        data-outer-radius="${whistlegraph.score.outerRadius}"
        data-inner-radius="${whistlegraph.score.innerRadius}"
        data-border-setting="${whistlegraph.score.border}"
        style="z-index: 1"
      >
        <div class="card" data-type="score" data-ratio="8.5x11"
          style="background: ${whistlegraph.score.color}; box-shadow: ${whistlegraph.score.boxShadow};">
          <img
            class="card-content"
            src="${assetPath}/${wg}/${wg}-score.png"
            crossorigin="anonymous">
          <div
            class="card-outline"
            style="border-color: ${whistlegraph.score.highlight}"
          ></div>
        </div>
      </div>
    `;
  }

  if (whistlegraph.video) {
    cardsMarkup += `
      <div
        class="card-view active"
        data-type="video"
        data-outer-radius="${whistlegraph.video.outerRadius}"
        data-inner-radius="${whistlegraph.video.innerRadius}"
        data-border-setting="${whistlegraph.video.border}"
        style="z-index: 2"
      >
        <div class="card" data-type="video" data-ratio="4x5"
          style="background: ${whistlegraph.video.color}; box-shadow: ${whistlegraph.video.boxShadow};">
          <video
            class="card-content"
            width="100%"
            height="100%"
            preload="auto"
            playsinline
            disablepictureinpicture
            src="${assetPath}/${wg}/${wg}-web.mp4"
            crossorigin="anonymous"
          ></video>
          <div class="card-cover"></div>
          <div
            class="card-outline"
            style="border-color: ${whistlegraph.video.highlight}"
          ></div>
          ${playButton}
        </div>
      </div>
    `;
  }

  html`
    <div class="card-deck loading">
      ${cardsMarkup}
      <div id="card-deck-loading">
        <div
          id="spinner"
          style="filter: brightness(0.9) drop-shadow(0 0 1vmin ${whistlegraph.glow})"
        >
          <img
            width="1000"
            height="1000"
            src="${assetPath}/${wg}/${wg}.webp"
            crossorigin="anonymous"
          />
          <canvas width="1000" height="1000" id="spinner-canvas"></canvas>
        </div>
      </div>
    </div>
    <img 
      id="wg-logo" 
      src="https://assets.aesthetic.computer/wipppps/wippppps.webp" 
      crossorigin="anonymous"
    />
    <script
      src="/aesthetic.computer/disks/whistlegraph/whistlegraph-cards.js"
      type="module"
      defer
    ></script>
    <style>
      #content .card-deck {
        width: 100%;
        height: 100%;
        box-sizing: border-box;
        user-select: none;
        -webkit-user-select: none;
        font-size: 32px;
        display: flex;
        /* This fixes a rendering bug specific to Google Chrome on Windows.
         It doesn't affect the look, but forces a different rendering stack in
         which everything seems to work fine. 2022.05.02.20.49 */
        transform: rotate(0.00001deg);
      }

      #content .card-view {
        width: 100%;
        height: 100%;
        box-sizing: border-box;
        position: absolute;
        pointer-events: none;
      }

      /*
      #content .card-view:not(.active):not(.running) {
        transition: 1s ease-out transform;
      }
      */

      #card-deck-loading {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0, 0, 0, 1);
        z-index: 100;
        display: flex;
      }

      #card-deck-loading #spinner {
        display: block;
        margin: auto;
        width: 40vmin;
        height: 40vmin;
        position: relative;
      }

      #spinner img {
        width: 100%;
        height: 100%;
        position: absolute;
      }

      #spinner canvas {
        width: 100%;
        height: 100%;
        position: absolute;
        display: none;
      }

      .card-deck:not(.loading) #spinner img {
        display: none;
      }
      .card-deck:not(.loading) #spinner canvas {
        display: block;
      }

      .card-deck:not(.loading) #card-deck-loading {
        transform: scale(2);
        opacity: 0;
        transition: 0.25s transform ease-out, 0.25s opacity ease-in;
      }

      .card-deck.loading #card-play {
        display: none;
      }

      #card-play.played {
        opacity: 0;
        transform: scale(2);
        transition: 0.25s opacity, 0.25s transform;
      }

      #card-play {
        position: absolute;
        top: 50%;
        left: 50%;
        pointer-events: none;
      }

      #card-play img {
        position: absolute;
        width: 25vmin;
        margin-left: -12.5vmin;
        margin-top: -12.5vmin;
      }

      #card-play img:nth-child(1) {
        filter: brightness(0);
      }
      #card-play img:nth-child(2) {
        filter: brightness(1);
      }

      .card.touch #card-play img {
        transform: scale(0.95);
      }

      .card-view.active .card {
        cursor: pointer;
      }

      .card-view.active .card.running {
        cursor: alias;
      }

      .card-view.active .card[data-type="score"],
      .card-view.active .card[data-type="image"] {
        cursor: alias;
      }

      .card {
        box-sizing: border-box;
        position: relative;
        box-shadow: 4px 4px 12px rgba(0, 0, 0, 0.75);
        pointer-events: all;
      }

      .card-view.active.pressed {
        transform: scale(0.99);
        animation: bounce 0.15s ease-out;
        animation-fill-mode: forwards;
      }

      @keyframes bounce {
        0% {
          transform: scale(0.99);
        }
        50% {
          transform: scale(0.96);
        }
        100% {
          transform: scale(0.99);
        }
      }

      .card-view.active.pressed .card {
        box-shadow: 0px 0px 48px rgba(0, 0, 0, 0.35),
          0px 0px 24px rgba(0, 0, 0, 0.35), 0px 0px 12px rgba(0, 0, 0, 0.35) !important;
      }

      .card.touch {
        box-shadow: 0px 0px 48px rgba(0, 0, 0, 0.5),
          0px 0px 48px rgba(0, 0, 0, 0.5) !important;
      }

      .card.hover {
        box-shadow: 0px 0px 24px rgba(0, 0, 0, 0.75) !important;
      }

      .card.touch .card-outline {
        display: block;
        box-sizing: border-box;
        position: absolute;
        border-style: solid;
        border-color: rgba(0, 0, 0, 1);
        border-width: 1vmin;
        top: -0.5vmin;
        left: -0.5vmin;
        width: calc(100% + 1vmin);
        height: calc(100% + 1vmin);
      }

      .card[data-type="score"].touch .card-outline,
      .card[data-type="image"].touch .card-outline {
        border-width: 0.75vmin;
        top: -0.375vmin;
        left: -0.375vmin;
        width: calc(100% + 0.75vmin);
        height: calc(100% + 0.75vmin);
      }

      .card[data-type="compilation"].touch .card-outline {
        border-width: 0.75vmin;
        top: -0.375vmin;
        left: -0.375vmin;
        width: calc(100% + 0.75vmin);
        height: calc(100% + 0.75vmin);
      }

      .card.animating .card-outline {
        display: none !important;
      }

      .card-outline {
        display: none;
        pointer-events: none;
      }

      .card-view .card-content {
        position: absolute;
      }

      .card-cover {
        /* A black screen that gets removed once videos are loaded. */
        background: black;
        position: absolute;
      }

      .card-deck:not(.loading) .card-cover {
        display: none;
      }

      /* Card types */
      .card-view[data-type="activity"] .card iframe,
      .card-view[data-type="video"] .card video,
      .card-view[data-type="compilation"] .card video {
        object-fit: cover;
        pointer-events: none;
      }

      iframe.card-content {
        border: none;
        pointer-events: all;
      }

      .card-content.interactive-content {
        pointer-events: all;
        cursor: default;
      }

      /* Contents inside each card */
      .card-view[data-type="score"] .card img,
      .card-view[data-type="image"] .card img {
        box-sizing: border-box;
        object-fit: cover;
        margin: auto;
        pointer-events: none;
      }

      .card-next {
        --divisor: 1.5;
        position: absolute;
        bottom: calc(2vmin / var(--divisor));
        right: calc(2vmin / var(--divisor));
        width: calc(15vmin / var(--divisor));
        height: calc(15vmin / var(--divisor));
        border-radius: calc(3vmin / var(--divisor)) 0 0 0;
      }

      .card-next img {
        position: absolute;
        pointer-events: none;
        width: 100%;
        margin-top: 0.25vmin;
        margin-left: 0.25vmin;
        transform: scale(1.07);
      }

      .card.touch .card-next img {
        transform: scale(1);
      }
    </style>
  `;
}

function sim({ simCount, needsPaint }) {
  if (fuzzy && whistlegraph.fuzz && simCount % whistlegraph.fuzz === 0n)
    needsPaint();
}

// 🎨 Paint (Executes every display frame)
function paint({ noiseTinted }) {
  noiseTinted(
    whistlegraph.bg.tint,
    whistlegraph.bg.tintAmount,
    whistlegraph.bg.pixelSaturation
  );
  return false;
}

function act({ event: e, net: { preloaded } }) {
  if (e.is("signal") && e.signal.includes("whistlegraph:started")) fuzzy = true;
  if (e.is("signal") && e.signal.includes("whistlegraph:preloaded"))
    preloaded();
}

export { boot, sim, paint, act };

// 📚 Library (Useful classes & functions used throughout the piece)

function parse(params) {
  const out = { wg: params[0] };
  if (whistlegraphs[out.wg] === undefined)
    out.wg = shortcuts[out.wg] || defaultWhistlegraph;
  out.whistlegraph = whistlegraphs[out.wg] || defaultWhistlegraph;
  return out;
}
