// Letters may appear in provider/database errors. Never log their free text.
const ERROR_CODES = new Set([
  "EAUTH", "ECONNECTION", "ECONNREFUSED", "ECONNRESET", "EDNS",
  "EENVELOPE", "EMESSAGE", "ENOTFOUND", "ESOCKET", "ETIMEDOUT", "ETLS",
]);

export function mailErrorCode(error) {
  if (error?.code === 11000) return "DUPLICATE";
  return ERROR_CODES.has(error?.code) ? error.code : "UNKNOWN";
}

// A letter from a handle names who sent it, never what it says: "@amelia" /
// "sent you a message" on a lock screen tells you whether to open it while
// the words stay on the wall. Letters from outside email stay anonymous — a
// sender's email address or display name is theirs, not the lock screen's.
const HANDLE = /^@[A-Za-z0-9._-]{1,40}$|^ac\d\d[a-z]{5}$/;

export function letterNotification(id, fromHandle) {
  const from = typeof fromHandle === "string" && HANDLE.test(fromHandle) ? fromHandle : null;
  return {
    title: from || "Letters",
    body: from ? "sent you a message" : "You have a new letter",
    data: { kind: "tell", tellId: id.toString(), piece: "mail" },
    // One notification stack per conversation on the lock screen.
    ...(from ? { thread: `mail:${from}` } : {}),
  };
}

// Shared push diagnostics include raw provider responses and device labels.
// Mail callers report aggregate results instead.
export const quietMailPush = () => {};
