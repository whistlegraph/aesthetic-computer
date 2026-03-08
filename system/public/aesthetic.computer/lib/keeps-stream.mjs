// Shared keep-mint stream helpers for keep UIs.
// Keeps rendering and UX local while unifying SSE parsing and request behavior.

function parseEventBlock(block) {
  if (!block || !block.trim()) return null;

  let eventType = "message";
  const dataLines = [];

  for (const rawLine of block.split("\n")) {
    const line = rawLine.replace(/\r$/, "");
    if (!line || line.startsWith(":")) continue;

    if (line.startsWith("event:")) {
      eventType = line.slice(6).trim() || "message";
    } else if (line.startsWith("data:")) {
      dataLines.push(line.slice(5).trimStart());
    }
  }

  if (dataLines.length === 0 && eventType === "message") {
    return null;
  }

  const rawData = dataLines.join("\n");
  let data = rawData;
  if (rawData) {
    try {
      data = JSON.parse(rawData);
    } catch {
      // Keep string payload when data is not JSON.
    }
  }

  return { type: eventType, data, rawData };
}

async function dispatchEvent(event, onEvent, summary) {
  summary.eventsCount += 1;
  summary.lastEventType = event.type;
  summary.lastEventData = event.data;

  if (!onEvent) return true;
  const shouldContinue = await onEvent(event);
  return shouldContinue !== false;
}

async function processEventBlocks(blocks, onEvent, summary) {
  for (const block of blocks) {
    const event = parseEventBlock(block);
    if (!event) continue;

    const shouldContinue = await dispatchEvent(event, onEvent, summary);
    if (!shouldContinue) {
      summary.stoppedByHandler = true;
      return false;
    }
  }
  return true;
}

export async function streamSseResponse(response, options = {}) {
  const { onEvent } = options;
  const summary = {
    eventsCount: 0,
    lastEventType: null,
    lastEventData: null,
    stoppedByHandler: false,
  };

  if (!response?.body?.getReader) {
    const text = await response.text();
    const blocks = text.replace(/\r/g, "").split("\n\n").filter(Boolean);
    await processEventBlocks(blocks, onEvent, summary);
    return summary;
  }

  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  let buffer = "";

  try {
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;

      buffer += decoder.decode(value, { stream: true }).replace(/\r/g, "");
      const blocks = buffer.split("\n\n");
      buffer = blocks.pop() || "";

      const shouldContinue = await processEventBlocks(blocks, onEvent, summary);
      if (!shouldContinue) {
        try {
          await reader.cancel();
        } catch {
          // Ignore cancellation errors from already-closed streams.
        }
        break;
      }
    }

    buffer += decoder.decode().replace(/\r/g, "");
    if (buffer.trim() && !summary.stoppedByHandler) {
      await processEventBlocks([buffer], onEvent, summary);
    }
  } finally {
    reader.releaseLock();
  }

  return summary;
}

function trimTrailingSlash(input = "") {
  return input.endsWith("/") ? input.slice(0, -1) : input;
}

function buildServerError(status, bodyText) {
  let message = `Server error ${status}`;
  if (!bodyText) return new Error(message);

  try {
    const parsed = JSON.parse(bodyText);
    if (parsed?.error) message = parsed.error;
  } catch {
    message = bodyText.trim() || message;
  }

  const error = new Error(message);
  error.status = status;
  error.responseText = bodyText;
  return error;
}

export async function postKeepMintStream(options = {}) {
  const {
    apiBase = "",
    endpoint = "/api/keep-mint",
    payload = {},
    token = null,
    signal = undefined,
    headers = {},
    onEvent = null,
    fetchImpl = fetch,
  } = options;

  const url = `${trimTrailingSlash(apiBase)}${endpoint}`;
  const response = await fetchImpl(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      ...(token ? { Authorization: `Bearer ${token}` } : {}),
      ...headers,
    },
    body: JSON.stringify(payload),
    signal,
  });

  if (!response.ok) {
    const bodyText = await response.text().catch(() => "");
    throw buildServerError(response.status, bodyText);
  }

  return streamSseResponse(response, { onEvent });
}
