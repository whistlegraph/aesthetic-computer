// 📰 Plaque
const plaque = document.querySelector("#plaque");

// 🃏 Cards
const cardDeck = document.querySelector("#card-deck");

// 🔘 UI
const close = document.querySelector("#close");
const next = document.querySelector("#next");
const prev = document.querySelector("#prev");
const progress = document.querySelector("#progress");
const buffering = document.querySelector("#buffering");
const portrait = document.querySelector("#portrait");

// Change the URL to the YouTube page if we are being referred from Youtube.
if (window.location.hash === "#yt") {
  plaque.querySelector('a').href = "https://www.youtube.com/watch?v=JgAlnu5L5n0";
}

// Clear no-transition after a sec.
setTimeout(() => {
  document
    .querySelectorAll(".no-transition")
    .forEach((el) => el.classList.remove("no-transition"));
}, 500);

// Start off with spinner.
let loadCount = 0;

function checkPreload() {
  loadCount += 1;
  if (loadCount === 3) {
    setTimeout(() => {
      buffering.classList.remove("dark");
      document.body.classList.remove("no-scroll");
    }, 500);
  }
}

// Load images in the active slides.
document.querySelectorAll("#card-deck img.active").forEach((img) => {
  img.onload = checkPreload;
  img.src = img.dataset.src;
});

portrait.onload = checkPreload(); 
portrait.src = portrait.dataset.src;

// Touch "active" state events for the plaque and for the slideshow buttons.
window.addEventListener("pointerdown", (e) => {
  if (e.target === plaque) plaque.classList.add("active");
  if (e.target === close) close.classList.add("active");
  if (e.target === next) next.classList.add("active");
  if (e.target === prev) prev.classList.add("active");
});

window.addEventListener("pointerup", (e) => {
  plaque.classList.remove("active");
  close.classList.remove("active");
  next.classList.remove("active");
  prev.classList.remove("active");
});

// 🖼️ Slideshow Logic
class Slideshow {
  active;

  #images;
  #index;
  #card;
  #caption;
  #captionLength;

  #spinnerTimeout;

  constructor(type) {
    this.#images = Array.from(
      document.querySelectorAll(`#${type}-slideshow img`)
    );

    // ❓ Move the 'active' class in `#type-slideshow img` to set the start.
    for (const [i, b] of this.#images.entries()) {
      if (b.classList.contains("active")) {
        this.#index = i;
        break;
      }
    }

    this.#card = document.querySelector(`#${type}-card`);

    this.#card.addEventListener("pointerdown", (e) => {
      if (this.#card.classList.contains("active")) return; // Cancel if slideshow is open.

      this.#card.classList.add("pressed");
    });

    window.addEventListener("pointerup", (e) => {
      this.#card.classList.remove("pressed");
    });

    this.#caption = document.querySelector(`#${type}-caption`);

    // Set the caption text size (special for Safari, and certain device ratios)
    if (/apple/i.test(navigator.vendor)) {
      if (window.devicePixelRatio === 3) {
        this.#caption.querySelector("text").style.fontSize = "4.95em";
      } else {
        this.#caption.querySelector("text").style.fontSize = "3.95em";
      }
    }

    this.#card.addEventListener("click", () => {
      if (this.#card.classList.contains(".no-transition")) return;
      if (this.#card.classList.contains("active")) return;
      this.#card.addEventListener(
        "transitionend",
        () => {
          document.body.classList.add("fullscreen");
          cardDeck.classList.add("fullscreen");
          delete cardDeck.style.fontSize;

          this.#card.classList.add("active");

          // Exception to hide specific slideshow buttons upon activation.
          if (type === "blackboard") {
            document.querySelector("#papers-card").classList.add("hidden");
          } else if (type === "papers") {
            document.querySelector("#blackboard-card").classList.add("hidden");
          }

          this.active = true;

          this.#captionLength = type === "blackboard" ? 65 : 50;
          this.printCaptionAndSetProgress();
        },
        { once: true }
      );
    });

    this.printCaptionAndSetProgress(); // Set first blackboard caption.
  }

  printCaptionAndSetProgress() {
    // 🅰️ Caption
    let fullText = this.#images[this.#index].dataset.alt;
    let finalText = [fullText];

    // Check to see if we can split the caption using a manual separator "$".
    if (fullText.indexOf("$") > -1) {
      finalText = fullText.split("$");
      this.#images[this.#index].alt = this.#images[
        this.#index
      ].dataset.alt.replace("$", "");
    } else {
      this.#images[this.#index].alt = this.#images[this.#index].dataset.alt;

      // Loop through of all " " characters in the caption to find breaks.
      // Breaking on the first " " after index 50.
      const breaks = [];
      for (let i = 0; i < fullText.length; i += 1) {
        if (fullText[i] === " " && i > this.#captionLength) {
          finalText = [fullText.slice(0, i), fullText.slice(i)];
          break;
        }
      }
    }

    let svg = "";

    finalText.forEach((half, i) => {
      svg += `<tspan x="50%" dy="${
        i === 0 ? "0" : "1.4em"
      }">${half.trim()}</tspan>`;
    });

    this.#caption.querySelector("text").innerHTML = svg;

    // 🅱️ Progress
    progress.style.width = `${
      (this.#index / (this.#images.length - 1)) * 100
    }%`;
  }

  close(x) {
    x.addEventListener(
      "transitionend",
      () => {
        document.body.classList.remove("fullscreen");
        cardDeck.classList.remove("fullscreen");
        setCardDeckSize();
        this.#card.classList.remove("active");
        this.#card.classList.add("no-transition");

        setTimeout(() => {
          this.#card.classList.remove("no-transition");
        }, 250);

        // Remove `.hidden` from blackboardCard and papersCard.
        cardDeck.querySelectorAll(".hidden").forEach((e) => {
          e.classList.remove("hidden");
        });
      },
      { once: true }
    );
    this.active = false;
  }

  // Determine if the image needs to load, or show a spinner.
  #checkImageLoad(img) {
    if (img.src === "") {
      img.addEventListener(
        "load",
        (e) => {
          img.classList.add("active");
          this.#caption.classList.remove("hidden");
          clearTimeout(this.#spinnerTimeout);
          buffering.classList.remove("active");
        },
        { once: true }
      );
      img.src = img.dataset.src;

      this.#spinnerTimeout = setTimeout(() => {
        buffering.classList.add("active");
      }, 50);
    } else {
      img.classList.add("active");
      this.#caption.classList.remove("hidden");
    }
  }

  next() {
    // Don't allow next if caption is hidden (image loading).
    if (this.#caption.classList.contains("hidden")) return;

    // Advance to next slide.
    this.#images[this.#index].classList.remove("active");
    this.#caption.classList.add("hidden");
    this.#index = (this.#index + 1) % this.#images.length;

    this.#checkImageLoad(this.#images[this.#index]);
    this.printCaptionAndSetProgress();
  }

  prev() {
    // Don't allow prev if caption is hidden (image loading).
    if (this.#caption.classList.contains("hidden")) return;

    // Precede to previous slide.
    this.#images[this.#index].classList.remove("active");
    this.#caption.classList.add("hidden");
    this.#index = (this.#index - 1 + this.#images.length) % this.#images.length;

    this.#checkImageLoad(this.#images[this.#index]);
    this.printCaptionAndSetProgress();
  }
}

// 🖱️ Input Events
const slideshows = [new Slideshow("blackboard"), new Slideshow("papers")];

close.addEventListener("click", () => {
  slideshows.forEach((s) => {
    if (s.active) s.close(close);
  });
});

next.addEventListener("click", () => {
  slideshows.forEach((s) => {
    if (s.active) s.next();
  });
});

prev.addEventListener("click", () => {
  slideshows.forEach((s) => {
    if (s.active) s.prev();
  });
});

function setCardDeckSize() {
  if (!cardDeck.classList.contains("fullscreen")) {
    cardDeck.style.fontSize = `${(cardDeck.clientWidth / 100) * 3.75}px`;
  }
}

function setPlaqueSize() {
  plaque.style.fontSize = `${(plaque.clientWidth / 100) * 3.5}px`;
}

window.addEventListener("resize", () => {
  setPlaqueSize();
  setCardDeckSize();
});

setTimeout(() => {
  setPlaqueSize();
  setCardDeckSize();
}, 10);

// Use arrow keys to progress through slides.
window.addEventListener("keydown", (e) => {
  if (e.key === "ArrowRight") {
    next.click();
  } else if (e.key === "ArrowLeft") {
    prev.click();
  }
});
