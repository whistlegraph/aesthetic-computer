import { networkInterfaces } from "os";
import qrcode from "qrcode-terminal";
import got from "got";

async function constructUrl() {
  const ifaces = networkInterfaces();
  let ipAddress;

  // Iterate over network interfaces to find the 1st non-internal IPv4 address
  Object.keys(ifaces).forEach((ifname) => {
    ifaces[ifname].forEach((iface) => {
      if (iface.family === "IPv4" && !iface.internal) {
        ipAddress = iface.address;
        return;
      }
    });
  });

  const url = `https://${ipAddress}:8888`;

  // Generate QR code in the terminal once a 200 is received from `url`.
  while (true) {
    try {
      const response = await got.get("https://localhost:8888", {
        https: { rejectUnauthorized: false },
      });
      if (response.statusCode === 200) {
        qrcode.generate(url, { small: true });
        console.log(`😃 Welcome to aesthetic.computer!`);
        console.log(`Local 💻️ https://localhost:8888`);
        console.log(`  LAN 🫂 ${url} (QR code above)`);
        console.log(`World 🌎 https://prompt.ac`);
        break;
      }
    } catch (error) {
      // console.error("❌ An error occurred:", error);
    }
    await new Promise((res) => setTimeout(res, 500)); // Hang out half a sec.
  }
}

constructUrl();