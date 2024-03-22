// Forked from: https://github.com/mrdoob/three.js/blob/master/examples/jsm/webxr/VRButton.js

// TODO: Get start and stop events from navigator.xr.requestSesion.

class VRButton {

	static createButton(renderer, started, ended) {

		const button = document.createElement('button');
		button.textContent = 'spatial';

		function showEnterVR( /*device*/) {

			let currentSession = null;

			async function onSessionStarted(session) {
				session.addEventListener('end', onSessionEnded);
				await renderer.xr.setSession(session);
				currentSession = session;
				started(session);
			}

			function onSessionEnded( /*event*/) {
				currentSession.removeEventListener('end', onSessionEnded);
				currentSession = null;
				ended();
			}

			button.tabIndex = -1;
			button.style.display = '';

			button.onclick = function () {
				if (currentSession === null) {
					// WebXR's requestReferenceSpace only works if the corresponding feature
					// was requested at session creation time. For simplicity, just ask for
					// the interesting ones as optional features, but be aware that the
					// requestReferenceSpace call will fail if it turns out to be unavailable.
					// ('local' is always available for immersive sessions and doesn't need to
					// be requested separately.)
					const sessionInit = { optionalFeatures: ['local-floor', 'bounded-floor', 'hand-tracking', 'layers'] };
					navigator.xr.requestSession('immersive-vr', sessionInit).then(onSessionStarted);
				} else {
					currentSession.end();
				}
			};
		}

		function disableButton() {
			button.style.display = 'none';
			button.onclick = null;
		}

		function showWebXRNotFound() {
			disableButton();
			button.style.display = 'none';
		}

		function showVRNotAllowed(exception) {
			disableButton();
			if (window.acDEBUG) console.warn('Exception when trying to call xr.isSessionSupported', exception);
		}

		if ('xr' in navigator) {
			button.id = 'VRButton';
			button.style.display = 'none';
			navigator.xr.isSessionSupported('immersive-vr').then(function (supported) {
				supported ? showEnterVR() : showWebXRNotFound();
				if (supported && VRButton.xrSessionIsGranted) {
					button.click();
				}
			}).catch(showVRNotAllowed);

			return button;
		} else {
			if (window.isSecureContext === false) {
				console.warn("🕶️ WebXR requires HTTPS.")
			} else {
				console.warn("🕶️ WebXR is unsupported in this browser.")
			}
		}
	}

	static xrSessionIsGranted = false;

	static registerSessionGrantedListener() {
		if ('xr' in navigator) {
			// WebXRViewer (based on Firefox) has a bug where addEventListener
			// throws a silent exception and aborts execution entirely.
			if (/WebXRViewer\//i.test(navigator.userAgent)) return;
			navigator.xr.addEventListener('sessiongranted', () => {
				VRButton.xrSessionIsGranted = true;
			});
		}
	}
}

VRButton.registerSessionGrantedListener();

export { VRButton };