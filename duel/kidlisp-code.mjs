// Every finished object is a KidLisp program, so each version gets a $code
// from AC's KidLisp store (identical source returns the same code).
const STORE = 'https://aesthetic.computer/api/store-kidlisp';

export function kidlispPublisher({ url = STORE, fetch = globalThis.fetch } = {}) {
  return async (source) => {
    const response = await fetch(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ source }),
      signal: AbortSignal.timeout(8000),
    });
    if (!response.ok) throw Error(`KidLisp store returned ${response.status}`);
    const { code } = await response.json();
    if (typeof code !== 'string' || !/^[A-Za-z0-9]{2,16}$/.test(code))
      throw Error('KidLisp store returned no code');
    return code;
  };
}
