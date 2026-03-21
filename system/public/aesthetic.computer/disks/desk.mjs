// Desk, 2025.9.30
// Pick and use a webcam, with support for Elmo cameras.

/* #region 📚 README 
  Allows you to select and display a webcam feed.
  Automatically searches for and prioritizes Elmo cameras.
#endregion */

/* #region 🏁 TODO 
#endregion */

// 🥾 Boot
function boot({ dom: { html } }) {
  html` <style>
      #desk {
        width: 100%;
        height: 100%;
        object-fit: cover;
      }
      #camera-list {
        position: absolute;
        bottom: 20px;
        left: 20px;
        background: rgba(0, 0, 0, 0.8);
        color: white;
        padding: 10px;
        font-family: monospace;
        font-size: 14px;
        z-index: 1000;
        max-width: 400px;
      }
      #camera-list button {
        display: block;
        margin: 5px 0;
        padding: 8px;
        background: rgba(255, 255, 255, 0.1);
        color: white;
        border: 1px solid rgba(255, 255, 255, 0.3);
        cursor: pointer;
        font-family: monospace;
        width: 100%;
        text-align: left;
      }
      #camera-list button:hover {
        background: rgba(255, 255, 255, 0.2);
      }
      #camera-list button.elmo {
        background: rgba(0, 255, 0, 0.2);
        border-color: rgba(0, 255, 0, 0.5);
      }
      #camera-list button.active {
        background: rgba(100, 150, 255, 0.4);
        border-color: rgba(100, 150, 255, 0.8);
      }
      #camera-list .status {
        margin-bottom: 10px;
        padding: 5px;
        background: rgba(255, 255, 255, 0.05);
      }
      #resolution-controls {
        position: absolute;
        bottom: 20px;
        right: 20px;
        background: rgba(0, 0, 0, 0.8);
        color: white;
        padding: 10px;
        font-family: monospace;
        font-size: 14px;
        z-index: 1000;
      }
      #resolution-controls select,
      #resolution-controls button {
        display: block;
        margin: 5px 0;
        padding: 5px;
        background: rgba(255, 255, 255, 0.1);
        color: white;
        border: 1px solid rgba(255, 255, 255, 0.3);
        font-family: monospace;
        width: 100%;
      }
      #resolution-controls button {
        cursor: pointer;
      }
      #resolution-controls button:hover {
        background: rgba(255, 255, 255, 0.2);
      }
    </style>
    <div id="camera-list">
      <div class="status">Loading cameras...</div>
    </div>
    <div id="resolution-controls">
      <div>Resolution:</div>
      <select id="resolution-select">
        <option value="default">Default</option>
        <option value="1280x720">1280x720</option>
        <option value="1920x1080">1920x1080</option>
        <option value="3840x2160">3840x2160</option>
      </select>
      <div>Frame Rate:</div>
      <select id="fps-select">
        <option value="default">Default</option>
        <option value="30">30 fps</option>
        <option value="60">60 fps</option>
      </select>
      <div>Latency:</div>
      <select id="latency-select">
        <option value="default">Default</option>
        <option value="low">Low Latency</option>
      </select>
      <div>Fit:</div>
      <select id="fit-select">
        <option value="cover">Cover (Fill)</option>
        <option value="contain">Contain (Fit)</option>
        <option value="fill">Fill (Stretch)</option>
      </select>
      <button id="apply-settings">Apply</button>
    </div>
    <video autoplay playsinline muted id="desk"></video>
    <script>
      const video = document.querySelector("#desk");
      const cameraList = document.querySelector("#camera-list");
      const resolutionSelect = document.querySelector("#resolution-select");
      const fpsSelect = document.querySelector("#fps-select");
      const latencySelect = document.querySelector("#latency-select");
      const fitSelect = document.querySelector("#fit-select");
      const applyButton = document.querySelector("#apply-settings");
      let currentStream = null;
      let currentDeviceId = null;
      let currentButton = null;

      async function listCameras() {
        try {
          const devices = await navigator.mediaDevices.enumerateDevices();
          const videoDevices = devices.filter(device => device.kind === 'videoinput');

          cameraList.innerHTML = '<div class="status">Available Cameras (Click to select):</div>';

          if (videoDevices.length === 0) {
            cameraList.innerHTML += '<div class="status">No cameras found</div>';
            return;
          }

          // Check for Elmo cameras and prioritize them
          let elmoCamera = null;
          videoDevices.forEach((device, index) => {
            console.log('Camera found:', device);
            const isElmo = device.label.toLowerCase().includes('elmo');
            const button = document.createElement('button');
            button.textContent = device.label || 'Camera ' + (index + 1);
            button.dataset.deviceId = device.deviceId;
            
            if (isElmo) {
              button.classList.add('elmo');
              button.textContent = '📹 ' + button.textContent;
              if (!elmoCamera) elmoCamera = device;
            }

            button.onclick = () => selectCamera(device.deviceId, button);
            cameraList.appendChild(button);
          });

          // Auto-select Elmo camera if found
          if (elmoCamera) {
            const elmoButton = Array.from(cameraList.querySelectorAll('button'))
              .find(btn => btn.dataset.deviceId === elmoCamera.deviceId);
            selectCamera(elmoCamera.deviceId, elmoButton);
          } else {
            // Otherwise select the first camera
            const firstButton = cameraList.querySelector('button');
            if (firstButton) {
              selectCamera(videoDevices[0].deviceId, firstButton);
            }
          }
        } catch (err) {
          console.error('Error listing cameras:', err);
          cameraList.innerHTML = '<div class="status">Error: ' + err.message + '</div>';
        }
      }

      async function selectCamera(deviceId, button) {
        try {
          // Stop current stream if exists
          if (currentStream) {
            currentStream.getTracks().forEach(track => track.stop());
          }

          // Store current selection
          currentDeviceId = deviceId;
          currentButton = button;

          // Start with basic constraints - just select the device
          const constraints = {
            video: {
              deviceId: { exact: deviceId }
            },
            audio: false
          };

          // Apply resolution if selected
          const resolution = resolutionSelect.value;
          if (resolution !== 'default') {
            const [width, height] = resolution.split('x').map(Number);
            constraints.video.width = { ideal: width };
            constraints.video.height = { ideal: height };
          }

          // Apply frame rate if selected
          const fps = fpsSelect.value;
          if (fps !== 'default') {
            constraints.video.frameRate = { ideal: Number(fps) };
          }

          // Apply latency hint if selected
          const latency = latencySelect.value;
          if (latency === 'low') {
            constraints.video.latency = 0;
          }

          console.log('Requesting camera with constraints:', constraints);
          currentStream = await navigator.mediaDevices.getUserMedia(constraints);

          // Log the actual settings we got
          const videoTrack = currentStream.getVideoTracks()[0];
          const settings = videoTrack.getSettings();
          const capabilities = videoTrack.getCapabilities();
          
          console.log('Camera settings:', settings);
          console.log('Camera capabilities:', capabilities);
          
          // List available modes
          if (capabilities.width && capabilities.height && capabilities.frameRate) {
            console.log('Available resolutions:');
            console.log('  Width:', capabilities.width);
            console.log('  Height:', capabilities.height);
            console.log('  Frame rates:', capabilities.frameRate);
          }

          video.srcObject = currentStream;

          // Update button states
          cameraList.querySelectorAll('button').forEach(btn => {
            btn.classList.remove('active');
          });
          button.classList.add('active');

          // Apply fit mode
          video.style.objectFit = fitSelect.value;

        } catch (err) {
          console.error('Error selecting camera:', err);
          alert('Failed to access camera: ' + err.message);
        }
      }

      // Apply button handler
      applyButton.onclick = () => {
        if (currentDeviceId && currentButton) {
          selectCamera(currentDeviceId, currentButton);
        }
      };

      listCameras();

      // Cleanup on page unload
      window.addEventListener('beforeunload', () => {
        if (currentStream) {
          currentStream.getTracks().forEach(track => track.stop());
        }
      });
    </script>`;
}

// 🎨 Paint
function paint({ wipe }) {
  wipe("black");
  return false;
}

export { boot, paint };
