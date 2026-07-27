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

export function shouldRetryViaSms(route, delivery) {
  return delivery?.status === "failed" &&
    route?.appleService === "iMessage" &&
    /^\+[0-9]{6,}$/.test(String(route?.handle || ""));
}
