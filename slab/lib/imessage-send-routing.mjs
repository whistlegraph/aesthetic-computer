export function chooseMessagesRoute(handles, latest = null) {
  const configured = (handles || []).map(String).filter(Boolean);
  if (!configured.length) throw new Error("recipient has no Messages handles");

  const observedService = String(latest?.service || "");
  const observedHandle = String(latest?.handle || "");
  const appleService = observedService === "RCS" || observedService === "SMS"
    ? "SMS"
    : "iMessage";

  return {
    handle: observedHandle || configured[0],
    appleService,
    observedService: observedService || null,
  };
}

export function classifyMessagesDelivery(row) {
  if (!row) return { status: "pending", service: null, error: 0 };

  const error = Number(row.error) || 0;
  const service = String(row.service || "") || null;
  if (error !== 0) return { status: "failed", service, error };
  if (Number(row.is_delivered)) return { status: "delivered", service, error: 0 };
  if (Number(row.is_sent)) return { status: "sent", service, error: 0 };
  return { status: "pending", service, error: 0 };
}

export function classifyMessagesAttachment(row) {
  const message = classifyMessagesDelivery(row);
  if (message.status === "failed") return message;
  if (!row) return message;

  const transferState = Number(row.transfer_state) || 0;
  if (transferState === 6) {
    return { status: "failed", service: message.service, error: message.error, transferState };
  }
  if (transferState !== 5) {
    return { status: "pending", service: message.service, error: message.error, transferState };
  }
  return { ...message, transferState };
}

export function conversationTitleMatches(title, expected) {
  const clean = (value) => String(value || "")
    .toLowerCase()
    .replace(/[^a-z0-9@+]+/g, " ")
    .trim();
  const actual = clean(title);
  const wanted = clean(expected);
  const actualWords = ` ${actual} `;
  const wantedWords = ` ${wanted} `;
  return actual.length >= 3 && wanted.length >= 3 && (
    actual === wanted || actualWords.includes(wantedWords) || wantedWords.includes(actualWords)
  );
}

export function isMessagesComposerEmpty(value) {
  const text = String(value ?? "").trim();
  return text === "" || text === "Message" || text === "iMessage" || text === "Text Message";
}

export function shouldRetryViaSms(route, delivery) {
  return delivery?.status === "failed" &&
    route?.appleService === "iMessage" &&
    /^\+[0-9]{6,}$/.test(String(route?.handle || ""));
}
