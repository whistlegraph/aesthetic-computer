// import { networkInterfaces } from "os";
import { exec } from "child_process";
import qrcode from "qrcode-terminal";
import got from "got";

let bootUps = 0;
const bootUpTimer = setInterval(() => {
  process.stdout.write("\x1Bc"); // Clear terminal.

  const command = `echo '' && echo "${
    bootUps % 2 === 0 ? "Booting up..." : "Booting up. . ."
  }" | toilet -f smblock`;

  exec(command, (error, stdout, stderr) => {
    if (error) {
      console.error(`Error executing command: ${error}`);
      return;
    }
    process.stdout.write(stdout);
    if (stderr) {
      console.error(`stderr: ${stderr}`);
    }
  });

  bootUps += 1;
}, 250);

async function constructUrl() {
  const url = `https://${process.env.HOST_IP}:8888`;

  // Generate QR code in the terminal once a 200 is received from `url`.
  try {
    while (true) {
      try {
        const response = await got.get("https://localhost:8888", {
          https: { rejectUnauthorized: false },
        });
        if (response.statusCode === 200) {
          clearInterval(bootUpTimer);
          setTimeout(() => {
            process.stdout.write("\x1Bc"); // Clear terminal.
            console.log(''); // Blank line.
            qrcode.generate(url, { small: true });
            console.log(`Local 💻️ https://localhost:8888`);
            console.log(`  LAN 🤗 ${url} (QR code above)`);
            console.log(`World 🌎 https://prompt.ac\n`);
          }, 100);
          break;
        }
      } catch (error) {
        // Keep trying indefinitely
      }
      await new Promise((res) => setTimeout(res, 500)); // Hang out half a sec.
    }
  } finally {
    console.log("🟪 Tunneling...");
  }
}

constructUrl();
