// Manual edits use the selected immutable version, never the inference loop.
export class SourceEditor {
  constructor({state, checks, hash, begin, render, inspect, commit, restore, finish,
    now = () => performance.now(), wait = ms => new Promise(resolve => setTimeout(resolve, ms)),
    timeout = 8000, settle = 750}) {
    Object.assign(this, {state, checks, hash, begin, render, inspect, commit, restore, finish, now, wait, timeout, settle});
    this.applying = false;
  }

  async read() {
    const base = this.state();
    const sourceHash = await this.hash(base.source);
    this.assertCurrent(base);
    return {piece: base.piece, code: base.code || '', version: base.version, source: base.source, sourceHash};
  }

  assertCurrent(base) {
    const current = this.state();
    if (current.piece !== base.piece || current.version !== base.version || current.source !== base.source) {
      throw Error('The selected version changed. Reopen Source to edit that version; your draft is saved.');
    }
    return current;
  }

  async apply({piece, version, sourceHash, source} = {}) {
    if (this.applying) throw Error('A source edit is already being checked.');
    this.applying = true;
    let started = false, committed = false;
    try {
      const base = this.state();
      if (base.busy || base.recording) throw Error('Finish the current request or recording before editing source.');
      if (typeof source !== 'string' || !source.trim()) throw Error('Enter a complete JavaScript piece.');
      if (new TextEncoder().encode(source).length >= 500000) throw Error('Source edits must be smaller than 500 KB.');
      if (piece !== base.piece || version !== base.version || sourceHash !== await this.hash(base.source)) {
        throw Error('The selected version changed. Reopen Source to edit that version; your draft is saved.');
      }
      const findings = this.checks(source);
      if (findings.length) throw Error(findings.map(finding => finding.message).join('\n'));
      const candidateHash = await this.hash(source);
      const current = this.assertCurrent(base);
      if (current.busy || current.recording) throw Error('Finish the current request or recording before editing source.');
      if (source === base.source) return {...base, sourceHash: candidateHash, changed: false};
      started = true;
      this.begin();
      const requestID = this.render(source);
      const deadline = this.now() + this.timeout;
      let paintedAt = null;
      while (true) {
        this.assertCurrent(base);
        const proof = this.inspect();
        if (proof.cancelled) throw Error('Source edit stopped. The previous version was restored.');
        if (proof.requestID !== requestID) throw Error('The preview changed while checking this edit.');
        if (proof.sourceHash && proof.sourceHash !== candidateHash) throw Error('The preview does not match this source.');
        const runtimeError = proof.logs?.find(entry => entry.level === 'error');
        if (proof.runtimeFailed || runtimeError) throw Error(runtimeError?.text || 'The edited piece could not run.');
        if (proof.sourceHash === candidateHash && proof.rendered) {
          paintedAt ??= this.now();
          if (this.now() - paintedAt >= this.settle) break;
        } else paintedAt = null;
        if (this.now() >= deadline) throw Error('The edited piece did not render. The previous version was restored.');
        await this.wait(25);
      }
      this.assertCurrent(base);
      const saved = this.commit({source, parent: base.version, request: 'Manual source edit', layers: 1});
      committed = true;
      return {piece: base.piece, code: base.code || '', version: saved.id, source, sourceHash: candidateHash, changed: true};
    } catch (error) {
      if (started && !committed) this.restore();
      throw error;
    } finally {
      try { if (started) this.finish(committed); }
      finally { this.applying = false; }
    }
  }
}
