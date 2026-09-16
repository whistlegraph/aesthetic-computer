// Binary updates are owned by the desktop host, never by the CLI tar updater.
function createUpdater({app, notify, requestRestart, prepareRelaunch = () => {}, canUpdateBinary = app.isPackaged, load = () => require('electron-updater').autoUpdater}) {
  let updater, downloaded = false, applying = false;
  function configure() {
    if (updater || !canUpdateBinary) return updater;
    updater = load();
    updater.autoDownload = true;
    updater.autoInstallOnAppQuit = false; // Session checkpoint must succeed first.
    updater.on('error', error => notify(`Update failed: ${error.message}`));
    updater.on('update-not-available', () => notify('Easel is up to date.'));
    updater.on('update-available', info => notify(`Downloading Easel ${info.version}…`));
    updater.on('update-downloaded', () => { downloaded = true; if (!applying) requestRestart('update'); });
    return updater;
  }
  return {
    async check() {
      if (!canUpdateBinary) { notify('Reloading this development build…'); requestRestart('restart'); return; }
      try { await configure().checkForUpdates(); } catch (error) { notify(`Update failed: ${error.message}`); }
    },
    async afterCheckpoint(action) {
      if (applying) return;
      applying = true;
      if (action === 'update' && canUpdateBinary && !downloaded) {
        // /update has exited its PTY after saving. Check before deciding to restart.
        try { const result = await configure().checkForUpdates(); if (result?.downloadPromise) await result.downloadPromise; } catch (error) { notify(`Update failed: ${error.message}`); }
      }
      prepareRelaunch();
      if (downloaded) { updater.quitAndInstall(false, true); return; }
      app.relaunch();
      app.exit(0);
    },
  };
}
module.exports = {createUpdater};
