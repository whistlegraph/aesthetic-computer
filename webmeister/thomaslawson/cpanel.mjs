#!/usr/bin/env node
/**
 * cPanel automation helpers for thomaslawson.com
 */

import { PageController, listPages } from "../browser.mjs";

const CPANEL_HOST = "p3plzcpnl502855.prod.phx3.secureserver.net:2083";

export async function getCPanelPage() {
  const pages = await listPages();
  const cpanelPage = pages.find(p => p.url.includes(CPANEL_HOST));
  if (!cpanelPage) throw new Error("No cPanel page found - open it in Chrome first");
  return PageController.connect(CPANEL_HOST);
}

export async function gotoSSLStatus(ctrl) {
  await ctrl.goto(`https://${CPANEL_HOST}/frontend/jupiter/ssl/ssl_tls_status.html`);
  return ctrl;
}

export async function gotoSSLInstall(ctrl) {
  await ctrl.goto(`https://${CPANEL_HOST}/frontend/jupiter/ssl/install.html`);
  return ctrl;
}

export async function installSSL(certData, keyData, cabundleData) {
  const ctrl = await getCPanelPage();
  await gotoSSLInstall(ctrl);
  await ctrl.wait(2000);
  
  // Fill in the certificate
  await ctrl.fill("textarea[name=\"crt\"]", certData);
  await ctrl.fill("textarea[name=\"key\"]", keyData);
  if (cabundleData) {
    await ctrl.fill("textarea[name=\"cabundle\"]", cabundleData);
  }
  
  console.log("SSL cert data filled - review and submit manually");
  return ctrl;
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const cmd = process.argv[2];
  
  if (cmd === "status") {
    const ctrl = await getCPanelPage();
    await gotoSSLStatus(ctrl);
    console.log("Navigated to SSL status page");
    await ctrl.close();
  } else if (cmd === "install") {
    const ctrl = await getCPanelPage();
    await gotoSSLInstall(ctrl);
    console.log("Navigated to SSL install page");
    await ctrl.close();
  } else if (cmd === "content") {
    const ctrl = await getCPanelPage();
    console.log(await ctrl.getContent());
    await ctrl.close();
  } else {
    console.log("Usage: node cpanel.mjs status|install|content");
  }
}
