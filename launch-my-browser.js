import puppeteer from 'puppeteer-core';

(async () => {
  const url = 'https://localhost:8888/prompt?icon=128x128';
  console.log(`Attempting to launch Puppeteer to: ${url}`);

  try {
    const browser = await puppeteer.launch({
      executablePath: '/usr/bin/chromium-browser', // As used in your icon.js
      headless: false,                             // Set to false to see the browser
      args: [                                      // Arguments similar to your icon.js
        '--autoplay-policy=no-user-gesture-required',
        '--ignore-gpu-blocklist',
        '--enable-gpu-rasterization',
        '--enable-oop-rasterization',
        // If localhost:8888 uses a self-signed SSL certificate and you encounter SSL errors,
        // you might need to add: '--ignore-certificate-errors'
      ],
    });
    const page = await browser.newPage();
    
    // Optional: Log console messages from the page to your terminal
    page.on('console', msg => {
      const args = msg.args();
      const logArgs = [];
      for (let i = 0; i < args.length; ++i) {
        logArgs.push(args[i].jsonValue());
      }
      Promise.all(logArgs).then(resolvedArgs => {
        console.log(`PAGE LOG (${msg.type()}):`, ...resolvedArgs);
      });
    });

    console.log(`Navigating to ${url}...`);
    await page.goto(url, { waitUntil: 'networkidle2' });
    console.log(`Successfully navigated to ${url}. The browser will remain open.`);
    // The browser will stay open because the script doesn't call browser.close()
    // and the Node.js process won't exit until the browser is manually closed by you.
  } catch (error) {
    console.error('Failed to launch or navigate with Puppeteer:', error);
  }
})();
