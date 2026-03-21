#!/usr/bin/env node
// 🎨 KidLisp.com Development Server
// Opens an external Chrome window with extensions (Temple, etc.) for testing
// Run: node webmeister/sites/kidlisp.com/dev.mjs

import { ExternalChrome } from '../../lib/external-chrome.mjs';

const DEV_URL = 'https://localhost:8888/kidlisp.com';

async function main() {
  const args = process.argv.slice(2);
  const command = args[0] || 'watch';
  
  const chrome = new ExternalChrome({
    verbose: true,
    sshHost: 'jas@host.docker.internal', // From vault/machines.json
  });
  
  switch (command) {
    case 'open':
      // Just open the page
      console.log('🎨 Opening KidLisp.com in external Chrome...');
      await chrome.openUrl(DEV_URL);
      console.log('✅ Page opened');
      break;
      
    case 'watch':
      // Open and watch console
      console.log('🎨 KidLisp.com Dev Mode');
      console.log('────────────────────────────────────────');
      await chrome.openUrl(DEV_URL);
      await chrome.watchConsole('kidlisp.com');
      break;
      
    case 'reload':
      // Reload the page
      await chrome.reloadPage('kidlisp.com');
      console.log('🔄 Page reloaded');
      break;
      
    case 'eval':
      // Evaluate JS in page context
      const code = args.slice(1).join(' ');
      if (!code) {
        console.log('Usage: node dev.mjs eval <javascript>');
        process.exit(1);
      }
      const result = await chrome.eval('kidlisp.com', code);
      console.log(result);
      break;
      
    case 'wallet':
      // Check wallet state
      const walletInfo = await chrome.eval('kidlisp.com', `
        JSON.stringify({
          beacon: !!window.beacon,
          taquito: !!window.taquito,
          keepsWallet: !!window.keepsWallet,
          keepsWalletAddress: window.keepsWalletAddress || null,
        })
      `);
      console.log('🔐 Wallet state:', JSON.parse(walletInfo));
      break;
      
    default:
      console.log(`
🎨 KidLisp.com Dev Commands:

  node dev.mjs open     - Open page in external Chrome
  node dev.mjs watch    - Open + watch console logs (default)
  node dev.mjs reload   - Reload the page
  node dev.mjs eval <js> - Evaluate JS in page
  node dev.mjs wallet   - Check wallet/beacon state
`);
  }
}

main().catch(e => {
  console.error('❌', e.message);
  process.exit(1);
});
