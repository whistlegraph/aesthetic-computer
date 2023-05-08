import { networkInterfaces } from "os";
import qrcode from "qrcode-terminal";

async function constructUrl() {
  const ifaces = networkInterfaces();
  let ipAddress;

  // iterate over network interfaces to find the first non-internal IPv4 address
  Object.keys(ifaces).forEach((ifname) => {
    ifaces[ifname].forEach((iface) => {
      if (iface.family === "IPv4" && !iface.internal) {
        ipAddress = iface.address;
        return;
      }
    });
  });

  const url = `https://${ipAddress}:8888`;
  console.log(`📱 URL: ${url}`);

  // generate QR code in the terminal
  qrcode.generate(url, { small: true });
}

constructUrl();