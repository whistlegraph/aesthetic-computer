// usb, 25.06.16.22.52
// Implements a WebUSB interface for use in bios.

let usbDevice = null;

const connectToUsb = async () => {
  try {
    usbDevice = await navigator.usb.requestDevice({ filters: [] });
    await usbDevice.open();
    console.log("🔌 Connected to USB device:", usbDevice);

    usbDevice.ondisconnect = (event) => {
      console.log("🔌 USB device disconnected:", event.device);
      usbDevice = null;
    };
  } catch (error) {
    if (error.name !== 'SecurityError' && error.name !== 'NotAllowedError') {
      console.error("🔌 USB connection failed:", error);
    }
  }
};

const listUsbDevices = async () => {
  try {
    const devices = await navigator.usb.getDevices();
    // console.log("🔌 Connected USB devices:", devices);
  } catch (error) {
    if (error.name !== 'SecurityError' && error.name !== 'NotAllowedError') {
      console.error("🔌 Failed to list USB devices:", error);
    }
  }
};

function initialize() {
  // Check if WebUSB is supported and available in this context
  if (typeof navigator !== 'undefined' && navigator.usb && 
      navigator.usb.getDevices && typeof navigator.usb.getDevices === 'function') {
    // Check if we have permissions to use USB
    try {
      listUsbDevices();
      connectToUsb();
    } catch (error) {
      // Silently handle permission errors
      if (error.name !== 'SecurityError' && error.name !== 'NotAllowedError') {
        console.warn("🔌 USB initialization failed:", error.message);
      }
    }
  } else {
    // Only log if WebUSB should be available but isn't (not in iframe/restricted context)
    if (typeof navigator !== 'undefined' && window.location.protocol === 'https:') {
      console.warn("🔌 WebUSB not supported in this context.");
    }
  }
}

export { initialize };

