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
    if (error.name !== 'SecurityError') {
      console.error("🔌 USB connection failed:", error);
    }
  }
};

const listUsbDevices = async () => {
  try {
    const devices = await navigator.usb.getDevices();
    // console.log("🔌 Connected USB devices:", devices);
  } catch (error) {
    if (error.name !== 'SecurityError') {
      console.error("🔌 Failed to list USB devices:", error);
    }
  }
};

function initialize() {
  if (navigator.usb) {
    listUsbDevices();
    connectToUsb();
  } else {
    console.warn("🔌 WebUSB not supported.");
  }
}

export { initialize };

