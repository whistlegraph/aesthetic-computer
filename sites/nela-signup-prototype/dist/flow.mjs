export const steps = ['Identity', 'Signal', 'Agreement', 'Contact', 'Membership', 'Review'];
export function fresh(origin = 'web') {
  return { origin, step: 0, reached: 0, name: origin === 'signal' ? 'River' : '', handle: '', signalName: origin === 'signal' ? 'river.42' : '', signalStatus: 'idle', signalCode: '', signalExpires: 0, agreed: false, email: '', emailRequestedFor: '', emailVerified: false, emailEvents: false, signalEvents: false, support: 'later', amount: 10, payment: 'none', complete: false };
}
export function handleError(handle) {
  if (!/^[a-z][a-z0-9-]{1,22}[a-z0-9]$/.test(handle)) return 'Use 3–24 lowercase letters, numbers or hyphens. Start with a letter; end with a letter or number.';
  if (['admin', 'auth', 'wiki', 'www', 'mail', 'signal', 'support', 'root', 'nela'].includes(handle)) return 'That name is reserved in this demo. Try another.';
  return '';
}
export function emailNeeded(s) { return s.signalStatus !== 'verified' || s.emailEvents; }
export function validate(s, step) {
  if (step === 0) {
    if (!s.name.trim()) return 'Enter the name you would like people to use.';
    return handleError(s.handle);
  }
  if (step === 2 && !s.agreed) return 'Read the code of conduct and confirm your agreement to continue.';
  if (step === 3) {
    if (s.email || emailNeeded(s)) {
      if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(s.email)) return 'Enter an email address, such as river@example.com.';
      if (!s.emailVerified || s.emailRequestedFor !== s.email) return 'Complete the demo email verification to continue.';
    }
    if (s.signalEvents && s.signalStatus !== 'verified') return 'Link Signal before choosing Signal announcements.';
  }
  if (step === 4 && s.support === 'monthly') {
    if (!Number.isInteger(s.amount) || s.amount < 1 || s.amount > 500) return 'Choose a whole-dollar amount from $1 to $500 for this demo.';
    if (s.payment !== 'success') return 'Try the demo checkout, or choose “Not now”.';
  }
  return '';
}
export function firstError(s) {
  for (const step of [0, 2, 3, 4]) { const error = validate(s, step); if (error) return { step, error }; }
  return null;
}
export function issueSignal(s, code, now = Date.now()) {
  if (!/^[a-zA-Z0-9_.-]{3,64}$/.test(s.signalName.trim())) throw new Error('Enter a Signal username for this demo, such as river.42.');
  s.signalStatus = 'pending'; s.signalCode = code; s.signalExpires = now + 90000; s.signalEvents = false;
}
export function confirmSignal(s, code, now = Date.now()) {
  if (s.signalStatus !== 'pending') throw new Error('Start a new Signal request first.');
  if (now >= s.signalExpires) { s.signalStatus = 'expired'; throw new Error('This request expired. Start a new request.'); }
  if (code !== s.signalCode) throw new Error('This code belongs to a different request.');
  s.signalStatus = 'verified'; s.signalCode = ''; s.signalExpires = 0;
}
export function setEmail(s, email) {
  if (email !== s.email) { s.emailVerified = false; s.emailRequestedFor = ''; }
  s.email = email;
}
export function summary(s) {
  return { origin: s.origin, name: s.name, address: `${s.handle}.nela.computer`, signal: s.signalStatus === 'verified' ? s.signalName : null, email: s.email || null, announcements: { email: s.emailEvents, signal: s.signalEvents }, support: s.support === 'monthly' ? { amount: s.amount, currency: 'USD', frequency: 'monthly', simulated: true } : null, agreement: s.agreed, complete: s.complete };
}
