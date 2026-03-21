import { randIntRange } from "../../lib/num.mjs";
import { shuffleInPlace, choose } from "../../lib/help.mjs";

const { min, random, floor } = Math;
const iOS = /(iPad|iPhone|iPod)/g.test(navigator.userAgent);

const deck = document.querySelector(".card-deck");
const cardViews = deck.querySelectorAll(".card-deck .card-view");
const cards = deck.querySelectorAll(".card-deck .card-view .card");
const layerOrder = [...cards].map((c) => c.dataset.type).reverse();

const videos = document.querySelectorAll("#content .card-deck .card video");
const loadingScreen = deck.querySelector("#card-deck-loading");
const spinnerCtx = deck.querySelector("#spinner-canvas").getContext("2d");
const spinnerImg = deck.querySelector("#spinner img");
const initialCardScale = 0.95;
const cardScale = 0.9;

let pointerDown = false;
let videosReady = 0;
let multipleTouches = false;
let activated = false;
let deactivateTimeout;
//let volumeOutInterval, volumeInInterval;
let volumeIntervals = {};

// Preload videos and animate away a loading spinner when complete.
videos.forEach((video) => {
  video.load();
  video.addEventListener("canplaythrough", () => {
    videosReady += 1;
    if (videosReady === videos.length) {
      // console.log("📹 Whistlegraph videos are ready to play!");
      setTimeout(() => {
        deck.classList.remove("loading");
        spinnerCtx.drawImage(spinnerImg, 0, 0);
      }, 500);
    }
  });
});

loadingScreen.addEventListener(
  "transitionend",
  () => {
    loadingScreen.style.display = "none";
    signal("whistlegraph:preloaded");
  },
  { once: true }
);

// 1️⃣ Input Events
// Hover states for cards when using only a mouse, and active states for mouse
// and touch.

deck.addEventListener("click", (event) => {
  if (!event.target.closest(".card-content.interactive-content")) {
    // Prevent default clicks on the deck if we are not inside of
    // an interactive card.
    event.preventDefault();
    event.stopPropagation();
  }
});

deck.addEventListener("pointermove", (e) => {
  if (!e.isPrimary || multipleTouches === true) return;
  // if (e.pointerType === "mouse") deck.classList.remove("no-cursor");
  const card = deck.querySelector(".card-view.active .card");
  const cardNext = card?.querySelector(".card-next");
  const target = document.elementFromPoint(e.clientX, e.clientY);
  if (target === card || target === cardNext) {
    if (
      e.pointerType === "mouse" &&
      activated === false &&
      pointerDown === false
    ) {
      card.classList.add("hover");
    }
  } else if (card) {
    card.classList.remove("touch");
    card.classList.remove("hover");
    activated = false;
  }
});

deck.addEventListener("pointerup", () => {
  const card = deck.querySelector(".card-view.active .card");
  card?.classList.remove("touch");
  pointerDown = false;
});

deck.addEventListener("touchstart", (e) => {
  if (e.touches.length > 1) {
    multipleTouches = true;
    const card = deck.querySelector(".card-view.active .card");

    clearTimeout(deactivateTimeout);
    deactivateTimeout = setTimeout(() => {
      activated = false;
      card.classList.remove("touch");
    }, 250);
  }
});

deck.addEventListener("touchend", (e) => {
  if (e.touches.length === 0) {
    multipleTouches = false;
    const card = deck.querySelector(".card-view.active .card");
    card?.classList.remove("touch");
  }
});

deck.addEventListener("pointerdown", (e) => {
  if (!e.isPrimary) return;
  pointerDown = true;
  const card = deck.querySelector(".card-view.active .card");
  const cardNext = card.querySelector(".card-next");
  const target = document.elementFromPoint(e.clientX, e.clientY);
  if (
    (target === card || target === cardNext) &&
    card.classList.contains("animating") === false &&
    card.classList.contains("touch") === false
  ) {
    card.classList.add("touch");
    card.classList.remove("hover");
    activated = true;
    clearTimeout(deactivateTimeout);
    deactivateTimeout = setTimeout(() => {
      activated = false;
      card.classList.remove("touch", "hover");
    }, 500);
  }
});

// 2️⃣ Switching from one card to another, animating them, and triggering the
// media for each.
deck.addEventListener("pointerup", (e) => {
  if (!e.isPrimary) return;

  const activeView = deck.querySelector(".card-view.active");

  if (!activeView) return; // Cancel if there are no 'active' cards.

  // Cancel if we didn't click on the actual card.
  const activeCard = activeView.querySelector(".card");
  const activeCardNext = activeCard.querySelector(".card-next");
  const target = document.elementFromPoint(e.clientX, e.clientY);
  if (target !== activeCard && target !== activeCardNext) return;

  // Make sure the card is still active based on the pointer events.
  if (activated === false) return;
  activated = false;

  // Make sure we are not in the middle of a transition.
  if (activeView.classList.contains("pressed")) {
    return;
  }

  // 1. Collect all card elements via layerOrder.
  const layers = {};
  layerOrder.forEach((layer) => {
    layers[layer] = document.querySelector(`.card-view[data-type=${layer}]`);
  });

  // Play the push down animation...
  activeView.classList.add("pressed");
  activeCard.classList.add("running");
  activeCard.classList.add("animating");

  if (e.pointerType === "mouse") deck.classList.add("no-cursor");

  activeView.addEventListener(
    "animationend",
    () => {
      activeView.classList.remove("pressed");
      activeCard.classList.remove("animating");
    },
    { once: true }
  );

  // Unmute the first video if it hasn't woken up yet...
  const video = activeCard.querySelector("video");
  if (video && video.paused && activeCard.dataset.type === "video") {
    // First click.
    activeView.style.transform = "none";
    document.querySelector("#card-play").classList.add("played");
    video.play();

    signal("whistlegraph:started");

    video.addEventListener("ended", function end() {
      if (activeView.classList.contains("active")) {
        video.play();
      } else {
        video.removeEventListener("ended", end);
      }
    });
    return;
  } else if (video && cards.length > 1) {
    // Or fade it out if it's already playing and not the only card.
    // console.log("Fading out volume on:", video);
    if (iOS) {
      video.muted = true;
    } else {
      clearInterval(volumeIntervals[video.src]);
      video.volume = 1;
      volumeIntervals[video.src] = setInterval(() => {
        video.volume *= 0.96;
        if (video.volume < 0.001) {
          video.volume = 0;
          clearInterval(volumeIntervals[video.src]);
        }
      }, 8);
    }
  }

  // Fade in audio if it's necessary for the next layer, if it exists.
  const nextLayer = layers[layerOrder[1]];
  const nextVideo = nextLayer?.querySelector(".card video");
  if (nextVideo) {
    const nextCard = nextVideo.closest(".card");
    if (nextVideo.paused) {
      nextVideo.play();
      nextCard.classList.add("running");
      nextVideo.muted = false;
      nextVideo.volume = 1;

      nextVideo.addEventListener("ended", function end() {
        if (nextVideo.closest(".card-view").classList.contains("active")) {
          nextVideo.play();
        } else {
          nextVideo.removeEventListener("ended", end);
        }
      });
    } else {
      // Bring volume back.
      nextCard.classList.add("running");
      if (iOS) {
        nextVideo.muted = false;
      } else {
        nextVideo.volume = 0.0;
        clearInterval(volumeIntervals[nextVideo.src]);
        nextVideo.volume = 0;
        volumeIntervals[nextVideo.src] = setInterval(() => {
          nextVideo.volume = min(1, nextVideo.volume + 0.01);
          if (nextVideo.volume >= 1) {
            nextVideo.volume = 1;
            clearInterval(volumeIntervals[nextVideo.src]);
          }
        }, 8);
      }
    }
  }

  // 2. Animate the top one off the screen, after the press animation ends.
  // Note: For some reason adding a delay here helps prevent the animation from
  //       triggering too early.

  setTimeout(function () {
    activeView.addEventListener(
      "animationend",
      () => {
        if (cards.length === 1) return; // Cancel all behaviors if there is only one card.
        const cardView = layers[layerOrder[0]];
        const card = cardView.querySelector(".card");
        layerOrder.push(layerOrder.shift()); // Move the 1st element to the end...

        // By calculating the proper distance it can move to based on what else
        // is left in the deck and its own size.
        let rX = 0,
          rY = 0;
        let maxTranslateHeight = 0,
          maxTranslateWidth = 0;
        const cardRect = card.getBoundingClientRect();

        [...deck.querySelectorAll(".card-view:not(.active) .card")]
          .map((card) => {
            return card.getBoundingClientRect();
          })
          .forEach((rect) => {
            if (rect.width > maxTranslateWidth) maxTranslateWidth = rect.width;
            if (rect.height > maxTranslateHeight)
              maxTranslateHeight = rect.height;
          });

        maxTranslateWidth -= (maxTranslateWidth - cardRect.width) / 2;
        maxTranslateHeight -= (maxTranslateHeight - cardRect.height) / 2;

        // Pad each by a bit.
        maxTranslateWidth *= 1.025;
        maxTranslateHeight *= 1.025;

        if (random() > 0.5) {
          rX += maxTranslateWidth;
          rY += random() * maxTranslateHeight;
        } else {
          rY += maxTranslateHeight;
          rX += random() * maxTranslateWidth;
        }

        if (random() > 0.5) rX *= -1;
        if (random() > 0.5) rY *= -1;

        // 3. Trigger the first transition, where the card moves off the top.

        const rotation = randIntRange(...rotRange) * lastTiltDir;
        lastTiltDir *= -1;

        rX = floor(rX);
        rY = floor(rY);
        card.dataset.rotation = floor(rotation);

        card.style.transition = "0.25s ease-out transform";
        card.style.transform = `rotate(${rotation}deg) translate(${rX}px, ${rY}px)`;

        // Remove the transform from the next cardView.
        const nextCardView = layers[layerOrder[0]];
        const nextCard = nextCardView?.querySelector(".card");

        if (nextCard) {
          nextCard.style.transition = "0.3s ease-out transform";
          nextCard.style.transform = "none";

          nextCard.addEventListener(
            "transitionend",
            () => {
              nextCardView.classList.add("active");
              nextCard.style.transition = "";
            },
            { once: true }
          );
        }

        cardView.classList.remove("active");
        card.classList.remove("running");
        card.classList.remove("hover");
        card.classList.add("animating");

        card.addEventListener(
          "transitionend",
          function end() {
            // and re-sort them on the z-axis.
            layerOrder.forEach((layer, index) => {
              const zIndex = layerOrder.length - 1 - index;
              const el = layers[layer];
              el.style.zIndex = zIndex;
              //if (zIndex === 2) el.classList.add("active");
            });

            // 5. Animate the top (now bottom) one back into the stack of cards.
            card.style.transition = "0.5s ease-in transform";
            //card.style.transform = "none";
            card.style.transform = `scale(${cardScale}) rotate(${card.dataset.rotation}deg)`;

            card.addEventListener(
              "transitionend",
              function secondEnd() {
                card.classList.remove("animating");
                card.style.transition = "";
              },
              { once: true }
            );
          },
          { once: true }
        );
      },
      { once: true }
    );
  }, 25);
});

function frame() {
  cardViews.forEach((cardView) => {
    const card = cardView.querySelector(".card");
    // const cardNext = card.querySelector(".card-next");
    const cardContent = card.querySelector(".card-content");

    const cardContentBackgroundImage = cardContent.querySelector(
      ".card-content-background-image"
    );

    const longestSide = min(deck.clientWidth, deck.clientHeight);
    const margin = floor(longestSide * 0.17); // Of the page.
    const borderSetting = cardView.dataset.borderSetting;
    const innerRadiusSetting = cardView.dataset.innerRadius;
    const outerRadiusSetting = cardView.dataset.outerRadius;

    card.style.borderRadius = margin * outerRadiusSetting + "px";
    if (cardContentBackgroundImage) {
      cardContentBackgroundImage.style.borderRadius = card.style.borderRadius;
    }
    cardContent.style.borderRadius = margin * innerRadiusSetting + "px";

    const border = floor(margin * borderSetting);

    const width = deck.clientWidth - margin;
    const height = deck.clientHeight - margin;

    const displayRatio = deck.clientWidth / deck.clientHeight;
    const contentRatioValues = card.dataset.ratio
      .split("x")
      .map((n) => parseFloat(n));
    const contentRatio = contentRatioValues[0] / contentRatioValues[1];

    if (contentRatio < displayRatio) {
      cardContent.style.width = floor(height * contentRatio) + "px";
      cardContent.style.height = height + "px";
    } else {
      cardContent.style.height = floor(width / contentRatio) + "px";
      cardContent.style.width = width + "px";
    }

    card.style.width = parseFloat(cardContent.style.width) + border + "px";
    card.style.height = parseFloat(cardContent.style.height) + border + "px";

    cardContent.style.left = border / 2 + "px";
    cardContent.style.top = border / 2 + "px";

    card.style.top = (deck.clientHeight - card.clientHeight) / 2 + "px";
    card.style.left = (deck.clientWidth - card.clientWidth) / 2 + "px";

    const cardCover = card.querySelector(".card-cover");

    if (cardCover) {
      cardCover.style.top = cardContent.style.top;
      cardCover.style.left = cardContent.style.left;
      cardCover.style.width = cardContent.style.width;
      cardCover.style.height = cardContent.style.height;
      cardCover.style.borderRadius = cardContent.style.borderRadius;
    }

    const cardOutline = card.querySelector(".card-outline");
    if (cardOutline) cardOutline.style.borderRadius = card.style.borderRadius;
  });
}

let lastTiltDir; // Used to offset every subsequent card.
const rotRange = [6, 14];

// 🎲 Randomly rotate the back two cards on initialization.
{
  // ⚠️ Assumes that there are TWO rotated card views behind a top card.
  const tiltBag = [randIntRange(...rotRange), randIntRange(...rotRange) * -1];
  shuffleInPlace(tiltBag);

  cards.forEach((card, i) => {
    if (i === cardViews.length - 1) return;
    if (i === 0) {
      // Remember last rotation dir. of the bottom card, the first in this loop.
      lastTiltDir = Math.sin(tiltBag[tiltBag.length - 1]);
    }
    card.style.transform = `scale(${initialCardScale}) rotate(${floor(
      tiltBag.pop()
    )}deg)`;
  });
}

const resizer = new ResizeObserver((entries) => {
  for (let entry of entries) {
    // Frame the deck upon adding it to the DOM, and on each resize, but not
    // when removing it from the DOM!
    if (
      entry.target === deck &&
      entry.contentRect.width > 0 &&
      entry.contentRect.height > 0
    ) {
      frame();
    }
  }
});

resizer.observe(deck);
