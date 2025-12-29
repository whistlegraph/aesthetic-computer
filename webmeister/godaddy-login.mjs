#!/usr/bin/env node
// godaddy-login.mjs - Automate GoDaddy login via CDP

import Web from './web.mjs';

async function login(domain) {
  const web = new Web({ verbose: true, port: 9222 });
  
  try {
    const creds = await Web.loadCredentials(domain);
    if (!creds.godaddy) throw new Error('No GoDaddy credentials for ' + domain);
    
    const { username, password } = creds.godaddy;
    console.log('Logging into GoDaddy as', username);
    
    await web.connect('godaddy');
    
    const pageInfo = await web.getPageInfo();
    console.log('Current page:', pageInfo.url);
    
    // Fill username
    console.log('Filling username...');
    await web.type('input#username', username);
    await web.sleep(300);
    
    // Fill password (both fields on same page)
    console.log('Filling password...');
    await web.type('input#password', password);
    await web.sleep(300);
    
    // Click Sign In button (its a button type=button with text Sign In)
    console.log('Clicking Sign In...');
    await web.eval('Array.from(document.querySelectorAll("button")).find(b => b.textContent.includes("Sign In")).click()');
    
    await web.sleep(4000);
    
    const afterLogin = await web.getPageInfo();
    console.log('After login:', afterLogin.url);
    
    if (afterLogin.url.includes('account.godaddy.com') || afterLogin.url.includes('myproducts')) {
      console.log('SUCCESS! Logged in.');
    } else if (afterLogin.url.includes('mfa') || afterLogin.url.includes('verify')) {
      console.log('2FA required - check browser');
    }
    
  } catch (err) {
    console.error('Error:', err.message);
  } finally {
    web.close();
  }
}

const domain = process.argv[2] || 'thomaslawson.com';
login(domain);