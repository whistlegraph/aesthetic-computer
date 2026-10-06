// Shared relay errors can name another client's checkout. This app's purchase
// entry is Brain settings, including when the user's free allowance runs out.
export function inferenceError(error) {
  const message = typeof error?.message === 'string' ? error.message : '';
  if (/^Out of braincells\b/i.test(message)) {
    return 'Out of braincells. Open Brain settings to add braincells, or wait for the free allowance to reset at midnight UTC.';
  }
  return message;
}
