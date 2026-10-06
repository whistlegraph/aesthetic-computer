// Account deletion stays in the web view; only explicit confirmation submits it.
const endpoint = 'https://aesthetic.computer/api/delete-erase-and-forget-me';

export function mountAccountDeletion({ account, onDeleted }) {
  const button = document.createElement('button');
  button.type = 'button'; button.textContent = 'delete account';
  button.id = 'account-delete'; button.hidden = !account.signedIn;
  document.querySelector('#account').append(button);
  const dialog = document.createElement('dialog');
  dialog.setAttribute('aria-labelledby', 'account-deletion-title');
  dialog.style.cssText = 'box-sizing:border-box;width:calc(100% - 32px);max-width:26rem;max-height:90vh;overflow:auto;padding:24px;background:#f5f5f0;color:#111;border:1px solid #555;font:18px/1.5 sans-serif';
  dialog.innerHTML = `<form>
    <h2 id="account-deletion-title" style="margin:0 0 16px;font-size:26px">Delete your AC account</h2>
    <p id="account-deletion-note" role="status">Loading your account…</p>
    <label>Type DELETE to confirm
      <input aria-label="Type DELETE to confirm" autocomplete="off" autocapitalize="characters" spellcheck="false" style="box-sizing:border-box;display:block;width:100%;padding:12px;margin:8px 0 16px;font:inherit">
    </label>
    <button type="submit" disabled style="font:inherit;padding:10px">Delete my account</button>
    <button type="button" style="font:inherit;padding:10px">Cancel</button>
  </form>`;
  document.body.append(dialog);
  const form = dialog.querySelector('form'), note = dialog.querySelector('p');
  const field = dialog.querySelector('input'), label = dialog.querySelector('label');
  const submit = dialog.querySelector('[type="submit"]'), cancel = dialog.querySelector('[type="button"]');
  let revision = 0, ready = false, busy = false, completed = false;
  const canSubmit = () => ready && !busy && !completed && account.signedIn && field.value === 'DELETE';
  const refresh = () => { submit.disabled = !canSubmit(); };
  function close() {
    if (busy) return;
    revision++; ready = false; dialog.close();
  }
  dialog.addEventListener('close', () => {
    revision++; ready = false;
    globalThis.__oskiewarAccountOpen = false;
    if (!button.hidden) button.focus();
  });
  dialog.addEventListener('cancel', event => { if (busy) event.preventDefault(); });
  cancel.addEventListener('click', close);
  field.addEventListener('input', refresh);
  addEventListener('oskiewar:account-change', () => {
    button.hidden = !account.signedIn;
    if (!account.signedIn && !completed) close();
  });
  async function request(method, token) {
    const response = await fetch(endpoint + (method === 'GET' ? '?preview' : ''), {
      method, headers: { authorization: 'Bearer ' + token },
      signal: AbortSignal.timeout(20000),
    });
    const body = await response.json();
    if (!response.ok) throw Error(body.message || 'The account service did not answer.');
    return body;
  }
  button.addEventListener('click', async () => {
    if (!account.signedIn) return;
    const current = ++revision;
    ready = false; completed = false; field.value = ''; refresh();
    label.hidden = false; submit.hidden = false; cancel.textContent = 'Cancel';
    note.textContent = 'Loading your account…';
    globalThis.__oskiewarAccountOpen = true; dialog.showModal();
    try {
      const token = await account.bearer();
      if (!token) throw Error('Sign in again before deleting your account.');
      const preview = await request('GET', token);
      if (current !== revision || !account.signedIn || !dialog.open) return;
      const days = Number(preview.graceDays);
      if (!Number.isFinite(days) || days < 0) throw Error('Could not read the deletion schedule.');
      note.textContent = `This deletes your entire Aesthetic Computer account, including its saved characters and content. Access stops immediately; deletion follows in ${days} days. Your email will receive a link to cancel before then.` +
        (preview.braincells > 0 ? ` You will lose ${preview.braincells} braincells.` : '') +
        ' Public blockchain records remain.';
      ready = true; refresh(); field.focus();
    } catch (error) { if (current === revision) note.textContent = error.message; }
  });
  form.addEventListener('submit', async event => {
    event.preventDefault();
    if (!canSubmit()) return;
    busy = true; cancel.disabled = true; field.readOnly = true; refresh();
    try {
      const token = await account.bearer();
      if (!token) throw Error('Sign in again before deleting your account.');
      const result = await request('POST', token);
      completed = true; ready = false;
      onDeleted();
      const date = result.purgeAfter && new Date(result.purgeAfter);
      note.textContent = `Your account is locked${date && Number.isFinite(date.getTime()) ? ' and will be deleted on ' + date.toLocaleDateString() : ' for deletion'}.` +
        (result.mailed ? ' Check your email for the link to keep it.' : '');
      label.hidden = true; submit.hidden = true; cancel.textContent = 'Done';
    } catch (error) {
      ready = false;
      note.textContent = error.message + ' Close this panel and check your account before trying again.';
    } finally {
      busy = false; cancel.disabled = false; field.readOnly = false; refresh();
    }
  });
}
