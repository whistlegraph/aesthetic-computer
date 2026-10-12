// Account progress and optional handle palettes for the native entry screen.
// An old verification must never reopen the workspace after sign-out/retry.
export class AccountConnection {
  constructor({verify, changed = () => {}, paletteChanged = null, loadColors = null, initialColors = () => []} = {}) {
    this.verify = verify;
    this.changed = changed;
    this.paletteChanged = paletteChanged;
    this.loadColors = loadColors;
    this.initialColors = initialColors;
    this.revision = 0;
  }
  async connect(token, notice = '') {
    const revision = ++this.revision;
    if (!token) {
      this.changed({status: notice ? 'failed' : 'signedOut', notice});
      return null;
    }
    this.changed({status: 'checking', notice: ''});
    try {
      const account = await this.verify(token);
      if (revision !== this.revision) return null;
      this.changed({status: account.handle ? 'ready' : 'needsHandle', notice: ''});
      const result = {...account, colors: account.handle ? this.initialColors('@' + account.handle) : []};
      if (account.handle && this.paletteChanged && this.loadColors) {
        void this.loadColors('@' + account.handle).then(colors => {
          if (revision !== this.revision) return;
          result.colors = colors;
          this.paletteChanged({handle: account.handle, colors});
        }).catch(() => {});
      }
      return result;
    } catch (error) {
      if (revision !== this.revision) return null;
      this.changed({status: 'failed', notice: error.code === 'offline'
        ? 'Could not reach Aesthetic Computer. Check your connection and retry.'
        : 'Could not verify your account. Retry or log in again.'});
      return null;
    }
  }
}
