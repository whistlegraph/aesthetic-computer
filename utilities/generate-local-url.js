// import { networkInterfaces } from "os";
import { spawn } from "child_process";
import qrcode from "qrcode-terminal";
import got from "got";

// Detect if we're running in GitHub Codespaces
const isCodespaces = process.env.CODESPACES === "true";
const codespaceName = process.env.CODESPACE_NAME;
const codespacesDomain = process.env.GITHUB_CODESPACES_PORT_FORWARDING_DOMAIN;

let bootUps = 0;
const bootUpTimer = setInterval(() => {
  process.stdout.write("\x1Bc"); // Clear terminal.

  const message = bootUps % 2 === 0 ? "Starting..." : "Starting. . .";
  
  const toilet = spawn("toilet", ["-f", "future"], {
    stdio: ['pipe', 'pipe', 'inherit']
  });
  
  const lolcat = spawn("lolcat", ["-x", "-r"], {
    stdio: ['pipe', 'inherit', 'inherit']
  });

  // Handle process cleanup to prevent blocking
  toilet.on('error', () => {});
  lolcat.on('error', () => {});
  
  toilet.stdout.pipe(lolcat.stdin);
  toilet.stdin.write(message);
  toilet.stdin.end();
  
  // Ensure processes exit properly
  toilet.on('close', () => {
    lolcat.stdin.end();
  });

  bootUps += 1;
}, 250);

async function constructUrl() {
  // Determine the URL based on environment
  let checkUrl, displayUrl, localUrl;
  
  if (isCodespaces && codespaceName && codespacesDomain) {
    // In Codespaces: netlify dev runs on HTTP, but Codespaces proxies it to HTTPS
    checkUrl = "http://localhost:8888";
    displayUrl = `https://${codespaceName}-8888.${codespacesDomain}`;
    localUrl = displayUrl; // In Codespaces, "local" URL is the forwarded HTTPS URL
  } else {
    // On local laptop: netlify dev runs with SSL certificates
    checkUrl = "https://localhost:8888";
    displayUrl = `https://${process.env.HOST_IP}:8888`;
    localUrl = "https://localhost:8888";
  }

  // Generate QR code in the terminal once a 200 is received from `checkUrl`.
  let attempts = 0;
  const maxAttempts = 120; // 60 seconds max wait time
  
  try {
    while (attempts < maxAttempts) {
      try {
        const response = await got.get(checkUrl, {
          https: { rejectUnauthorized: false },
          timeout: {
            request: 2000 // 2 second timeout per request
          }
        });
        if (response.statusCode === 200) {
          clearInterval(bootUpTimer);
          setTimeout(() => {
            process.stdout.write("\x1Bc"); // Clear terminal.
            console.log(''); // Blank line.
            
            if (isCodespaces) {
              console.log(`🌐 Codespaces URL: ${displayUrl}`);
              console.log(`🍃 Local Check:    ${checkUrl}`);
              console.log(`📡 World:          https://prompt.ac\n`);
            } else {
              qrcode.generate(displayUrl, { small: true });
              console.log(`💻 LAN   ${displayUrl}`);
              console.log(`🍃 Local ${localUrl}`);
              console.log(`📡 World https://prompt.ac\n`);
            }
          }, 100);
          break;
        }
      } catch (error) {
        // Debug: uncomment to see what's happening
        // console.error(`Attempt ${attempts + 1}: ${error.message}`);
      }
      attempts++;
      await new Promise((res) => setTimeout(res, 500)); // Hang out half a sec.
    }
    
    if (attempts >= maxAttempts) {
      clearInterval(bootUpTimer);
      process.stdout.write("\x1Bc"); // Clear terminal.
      console.log("❌ Server failed to start after 60 seconds");
      console.log(`🔧 Checked: ${checkUrl}`);
      console.log("� Try running 'npm run site' manually to debug");
      process.exit(1);
    }
  } finally {
    if (!isCodespaces) {
      console.log("🟪 Tunneling...");
    }
  }
}

constructUrl();
