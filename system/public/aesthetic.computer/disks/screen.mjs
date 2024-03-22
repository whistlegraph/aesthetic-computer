// Screen, 2024.3.08.18.23.42.453
// Mirror another screen from your system display.

/* #region 📚 README 
#endregion */

/* #region 🏁 TODO 
#endregion */

// 🥾 Boot
function boot({ dom: { html } }) {
  html` <style>
      #screen {
        width: 100%;
        height: 100%;
      }
    </style>
    <video autoplay id="screen"></video>
    <script>
      async function start(e) {
        const video = document.querySelector("#screen");
        video.srcObject = await navigator.mediaDevices.getDisplayMedia({
          video: {
            displaySurface: "window",
          },
          audio: false,
        });
      }

      start();
    </script>`;
}

// 🎨 Paint
function paint({ wipe, ink }) {
  wipe("black");
  return false;
}

export { boot, paint };
