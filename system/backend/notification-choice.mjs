const CHOICES = new Set([
  "denied",
  "dismissed",
  "disabled",
  "enabled",
  "error",
]);

const SOURCES = new Set(["auto", "bell", "observed", "register"]);

const COUNTERS = {
  denied: "deniedCount",
  dismissed: "dismissedCount",
  disabled: "disabledCount",
  enabled: "enabledCount",
  error: "errorCount",
};

export function notificationChoiceUpdate(userSub, body = {}, at = new Date()) {
  const choice = typeof body.choice === "string" ? body.choice : "";
  const source = typeof body.source === "string" ? body.source : "";
  const deviceId = typeof body.deviceId === "string" ? body.deviceId : "";

  if (!CHOICES.has(choice)) throw new Error("Invalid notification choice");
  if (!SOURCES.has(source)) throw new Error("Invalid notification source");
  if (deviceId.length < 8 || deviceId.length > 128) {
    throw new Error("Invalid notification device ID");
  }

  const label =
    typeof body.label === "string" ? body.label.slice(0, 64) : "";
  const platform = body.platform === "ios" ? "ios" : "web";
  const user = userSub.startsWith("sotce-")
    ? userSub
    : "sotce-" + userSub;

  const update = {
    $set: { choice, source, label, platform, updatedAt: at },
    $setOnInsert: { createdAt: at },
  };

  // An observed denial records the browser's current state without counting
  // every subsequent page load as another rejection.
  if (source !== "observed") {
    update.$inc = { [COUNTERS[choice]]: 1 };
  }

  return {
    filter: { user, deviceId },
    update,
    options: { upsert: true },
  };
}
