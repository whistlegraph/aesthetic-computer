// The Longest Whistlegraph Ever (so far) (Player)

const body = document.body;
const videoWrapper = document.getElementById("video-wrapper");
const embed = document.getElementById("embed");
const curtain = document.getElementById("curtain");
const playStill = document.getElementById("play-still");
const playAnimated = document.getElementById("play-animated");
const playTriangle = document.getElementById("play-triangle");
const playBuffering = document.getElementById("play-buffering");
const playBufferingGraphic = document.getElementById("play-buffering-graphic");
const scrubBar = document.getElementById("scrub-bar");
const timeCode = document.getElementById("timecode");
const scrubProgress = document.getElementById("scrub-progress");
const aboutLink = document.getElementById("about-link");

// const PeerTubePlayer = window["PeerTubePlayer"];

// Prevent some default touch behavior to make the experence better on mobile.
// window.ontouchstart = (e) => {
//   e.preventDefault();
// }

window.ontouchmove = (e) => {
  e.preventDefault();
};

// window.ontouchend = (e) => {
//   e.preventDefault();
// }

// 1. Vimeo Player Integration
const player = new Vimeo.Player(embed);

player.on("loaded", function () {
  // Once the player is ready for interaction, we can show the interface.
  playStill.classList.add("active");
  aboutLink.classList.add("active");
  curtain.classList.add("interactive");

  // ***First Play, Pause & Resume***
  let firstClick = true;
  let curtainUp = false;

  window.addEventListener("pointerdown", (e) => {
    if (e.target === videoWrapper) {
      videoWrapper.classList.add('pressed');
      curtain.classList.add("active");
    }
    if (e.target === aboutLink) aboutLink.classList.add('pressed');
  });

  window.addEventListener("pointerup", (e) => {
    if (curtain.classList.contains('active')) {
      curtain.classList.remove("active");
      videoWrapper.classList.remove('pressed');
      if (e.target !== videoWrapper) return;
      liftCurtain();
    }
    aboutLink.classList.remove('pressed');
  });

  if (window.localStorage.longestWhistlegraphCurrentTime !== undefined) {
    player.setCurrentTime(window.localStorage.longestWhistlegraphCurrentTime)
  }

  async function liftCurtain(startPaused = false) {

    const paused = await player.getPaused();
    if (firstClick) {
      playStill.classList.remove("active");
      playAnimated.classList.add("active");
      aboutLink.classList.remove("active");
      body.classList.add("buffering");

      player.play();
      player.setVolume(1);

      firstClick = false;
    } else if (curtainUp) {
      if (paused) {
        player.play();
        body.classList.add("unpaused");
        body.classList.remove("paused");
        aboutLink.classList.remove("active");
      } else {
        if (curtain.classList.contains("hidden")) {
          curtain.classList.remove("hidden");
        }
        body.classList.remove("unpaused");
        body.classList.add("paused");
        aboutLink.classList.add("active");
        player.pause();
      }
    }
  }

  // ***Progress Update & Curtain Reveal***
  player.on("timeupdate", async (e) => {
    if (
      curtainUp === false &&
      e.seconds > 0.75 &&
      !curtain.classList.contains("hidden")
    ) {
      curtain.classList.add("hidden");
      curtain.addEventListener(
        "animationend",
        () => {
          playStill.classList.remove("active");
          playAnimated.classList.remove("active");
          playTriangle.classList.add("active");
          curtainUp = true;
          scrubBar.classList.add("active");
          body.classList.remove("buffering");
        },
        { once: true }
      );
    }

    // Update the progress bar.
    const duration = await player.getDuration();
    const currentTime = await player.getCurrentTime();

    // Remember current time in localStorage, in case we revisit the page.
    window.localStorage.longestWhistlegraphCurrentTime = currentTime;

    if (!scrubbing) {
      scrubProgress.style.width =
        Math.ceil((currentTime / duration) * scrubBar.clientWidth) + "px";
    }
  });

  let scrubbing = false;
  let scrubIsdown = false;
  let lastX;

  scrubBar.addEventListener("pointerdown", (e) => {
    if (!e.isPrimary) return;
    scrubIsdown = true;
    lastX = e.x;
  });

  window.addEventListener("pointermove", async (e) => {
    if (!e.isPrimary) return;
    if (!scrubIsdown) return;
    scrubbing = true;
    scrubBar.classList.add("scrubbing");
    player.pause();
    const deltaX = e.x - lastX;
    lastX = e.x;

    const currentWidth = parseFloat(scrubProgress.style.width);
    const newTime = Math.floor(currentWidth + deltaX);
    scrubProgress.style.width = newTime + "px";

    const totalSeconds = await player.getDuration();

    // Format a total number of seconds into NN:NN.
    function formatTime(totalSeconds) {
      const minutes = Math.floor(totalSeconds / 60).toString();
      const seconds = Math.floor(totalSeconds - minutes * 60).toString();
      return minutes.padStart(2, "0") + ":" + seconds.padStart(2, "0");
    }

    const currentSeconds = Math.max(0, Math.min(
      totalSeconds * (newTime / scrubBar.clientWidth),
      totalSeconds
    ));

    timeCode.innerHTML = `${formatTime(currentSeconds)} ∕ ${formatTime(
      totalSeconds
    )}`;
  });

  window.addEventListener("pointerup", async (e) => {
    if (!e.isPrimary) return;

    const duration = await player.getDuration();
    const paused = await player.getPaused();
    //const progress = await player.getCurrentTime();

    scrubIsdown = false;
    if (scrubbing) {
      // Progress Scrubbing
      let newTime =
        (parseFloat(scrubProgress.style.width) / scrubBar.clientWidth) *
        duration;
      if (newTime > duration) {
        newTime = 0;
      } else if (newTime < 0) {
        newTime = 0;
      }

      playBuffering.classList.add("active");
      playBufferingGraphic.classList.add("shade");
      body.classList.add("buffering");

      player.setCurrentTime(newTime).then((e) => {
        playBuffering.classList.remove("active");
        playBufferingGraphic.classList.remove("shade");
        body.classList.remove("buffering");
      });

      scrubbing = false;
      // Play back immediately after finishing a scrub.
      if (paused) {
        player.play();
        body.classList.add("unpaused");
        body.classList.remove("paused");
        scrubBar.classList.remove("scrubbing");
      }
    }
  });
});

// 2. PeerTube Player Integration
/*
const player = new PeerTubePlayer(embed);
await player.ready;

// Once the player is ready for interaction, we can show the interface.
playStill.classList.remove('inactive');
aboutLink.classList.remove('inactive');

// Check to see if we clicked inside the iframe.
let focusedIframe = false;

function checkIframe() {
  if (
    document.activeElement &&
    document.activeElement === embed
  ) {
    if (focusedIframe == false) {
      focusedIframe = true;
      playStill.classList.add('inactive');
      playAnimated.classList.add('active');
      clearInterval(checkIframe);
    }
  } else focusedIframe = false;
}

setInterval(checkIframe, 200);

// Remove the curtain once the video actually starts playing a tenth of a second in.
player.addEventListener("playbackStatusUpdate", function statusUpdate(e) {
  console.log(e.position);
  if (e.position > 0 && !curtain.classList.contains("hidden")) {
    curtain.classList.add("hidden");
    player.removeEventListener("playbackStatusUpdate", statusUpdate);
  }
});
*/

// Inject CSS into the iframe.
// TODO: Must be the same origin. 22.09.05.14.10
/*
window.onload = () => {
  console.log(embed);
  const doc = embed.contentDocument;
  console.log("Content document:", doc);
  doc.body.innerHTML += `
          <style>
            .video-js .vjs-control:focus:before, .video-js .vjs-control:hover:before, .video-js .vjs-control:focus {
              text-shadow: none !important;
            }
          </style>`;
};
*/
