// Retry only idempotent transfers. Inference uses deadlines without replaying a paid request.
export function isTransientNetworkError(error) {
  return error?.name === 'TimeoutError' || error?.status === 408 || error?.status === 429 || error?.status >= 500 ||
    /^(Load failed|Failed to fetch|fetch failed|NetworkError|Network request failed)/i.test(error?.message || '');
}

export function httpError(message, status) {
  return Object.assign(new Error(message), {status});
}

export async function withNetworkDeadline(operation, {controller = new AbortController(), timeoutMs = 20000} = {}) {
  let timer;
  const timeout = new Promise((_, reject) => {
    timer = setTimeout(() => {
      const error = Object.assign(new Error('The connection timed out.'), {name: 'TimeoutError'});
      reject(error);
      controller.abort(error);
    }, timeoutMs);
  });
  try { return await Promise.race([operation(controller.signal), timeout]); }
  finally { clearTimeout(timer); }
}

export async function retryNetwork(operation, {delays = [1000, 3000], sleep = ms => new Promise(resolve => setTimeout(resolve, ms)), onRetry = () => {}, timeoutMs = 20000} = {}) {
  for (let attempt = 0; ; attempt++) {
    try { return await withNetworkDeadline(operation, {timeoutMs}); }
    catch (error) {
      if (!isTransientNetworkError(error) || attempt >= delays.length) throw error;
      onRetry({attempt: attempt + 1, delay: delays[attempt]});
      await sleep(delays[attempt]);
    }
  }
}
