// The Longest Whistlegraph Ever (so far) — self-hosted player
// The film was first embedded from Vimeo, and Vimeo stopped serving it in
// October 2026. This answers the part of the Vimeo Player API that
// script-index.js uses with a plain <video>, so the original player code runs
// unchanged against a copy hosted by whistlegraph.org.

class Player {
  constructor(video) {
    this.video = video;
  }

  on(name, callback) {
    const video = this.video;
    if (name === "loaded") {
      // Vimeo fires "loaded" once the video can be interacted with.
      if (video.readyState >= 1) setTimeout(() => callback({}), 0);
      else video.addEventListener("loadedmetadata", () => callback({}), { once: true });
    } else if (name === "timeupdate") {
      video.addEventListener("timeupdate", () => {
        const duration = video.duration || 0;
        callback({
          seconds: video.currentTime,
          duration,
          percent: duration ? video.currentTime / duration : 0,
        });
      });
    } else {
      video.addEventListener(name, callback);
    }
  }

  play() {
    return this.video.play().catch((err) => console.warn("🎞️ play:", err));
  }

  pause() {
    this.video.pause();
    return Promise.resolve();
  }

  getPaused() {
    return Promise.resolve(this.video.paused);
  }

  getDuration() {
    return Promise.resolve(this.video.duration || 0);
  }

  getCurrentTime() {
    return Promise.resolve(this.video.currentTime);
  }

  setCurrentTime(seconds) {
    const video = this.video;
    return new Promise((resolve) => {
      video.addEventListener("seeked", () => resolve(video.currentTime), { once: true });
      video.currentTime = parseFloat(seconds) || 0;
    });
  }

  setVolume(volume) {
    this.video.muted = false;
    this.video.volume = volume;
    return Promise.resolve(volume);
  }
}

window.Vimeo = { Player };
