// Letters may appear in provider/database errors. Never log their free text.
const ERROR_CODES = new Set([
  "EAUTH", "ECONNECTION", "ECONNREFUSED", "ECONNRESET", "EDNS",
  "EENVELOPE", "EMESSAGE", "ENOTFOUND", "ESOCKET", "ETIMEDOUT", "ETLS",
]);

export function mailErrorCode(error) {
  if (error?.code === 11000) return "DUPLICATE";
  return ERROR_CODES.has(error?.code) ? error.code : "UNKNOWN";
}

export function letterNotification(id) {
  return {
    title: "Letters",
    body: "You have a new letter",
    data: { kind: "tell", tellId: id.toString(), piece: "mail" },
  };
}

// Shared push diagnostics include raw provider responses and device labels.
// Mail callers report aggregate results instead.
export const quietMailPush = () => {};
