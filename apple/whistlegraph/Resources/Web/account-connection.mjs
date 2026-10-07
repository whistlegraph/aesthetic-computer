// Both wares report account progress to the visible native entry screen.
// An old verification must never reopen the workspace after sign-out/retry.
export class AccountConnection {
  constructor({verify, changed = () => {}} = {}) {
    this.verify = verify;
    this.changed = changed;
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
      return account;
    } catch (error) {
      if (revision !== this.revision) return null;
      this.changed({status: 'failed', notice: error.code === 'offline'
        ? 'Could not reach Aesthetic Computer. Check your connection and retry.'
        : 'Could not verify your account. Retry or log in again.'});
      return null;
    }
  }
}
