import test from "node:test";
import assert from "node:assert/strict";
import { notificationChoiceUpdate } from "../backend/notification-choice.mjs";

test("records a notification rejection without content or endpoint data", () => {
  const at = new Date("2026-09-13T00:00:00.000Z");
  const operation = notificationChoiceUpdate(
    "auth0|reader",
    {
      choice: "denied",
      source: "bell",
      deviceId: "device-123",
      label: "Chrome on macOS",
      platform: "web",
      endpoint: "must-not-be-stored",
      text: "must-not-be-stored",
    },
    at,
  );

  assert.deepEqual(operation, {
    filter: { user: "sotce-auth0|reader", deviceId: "device-123" },
    update: {
      $set: {
        choice: "denied",
        source: "bell",
        label: "Chrome on macOS",
        platform: "web",
        updatedAt: at,
      },
      $setOnInsert: { createdAt: at },
      $inc: { deniedCount: 1 },
    },
    options: { upsert: true },
  });
});

test("observing an existing denial does not inflate the rejection count", () => {
  const operation = notificationChoiceUpdate("sotce-auth0|reader", {
    choice: "denied",
    source: "observed",
    deviceId: "device-123",
  });

  assert.equal(operation.filter.user, "sotce-auth0|reader");
  assert.equal(operation.update.$inc, undefined);
});

test("rejects unknown choices, sources, and malformed device IDs", () => {
  assert.throws(
    () =>
      notificationChoiceUpdate("auth0|reader", {
        choice: "maybe",
        source: "bell",
        deviceId: "device-123",
      }),
    /Invalid notification choice/,
  );
  assert.throws(
    () =>
      notificationChoiceUpdate("auth0|reader", {
        choice: "denied",
        source: "unknown",
        deviceId: "device-123",
      }),
    /Invalid notification source/,
  );
  assert.throws(
    () =>
      notificationChoiceUpdate("auth0|reader", {
        choice: "denied",
        source: "bell",
        deviceId: "short",
      }),
    /Invalid notification device ID/,
  );
});
