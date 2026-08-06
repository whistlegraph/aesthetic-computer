import assert from "node:assert/strict";
import test from "node:test";

import {
  classifyMessagesAttachment,
  conversationTitleMatches,
  isMessagesComposerEmpty,
  chooseMessagesRoute,
  classifyMessagesDelivery,
  shouldRetryViaSms,
} from "../lib/imessage-send-routing.mjs";

test("routes an RCS conversation through the Messages RCS account", () => {
  assert.deepEqual(
    chooseMessagesRoute(["+15551234567"], {
      handle: "+15551234567",
      service: "RCS",
    }),
    {
      handle: "+15551234567",
      appleService: "RCS",
      observedService: "RCS",
    },
  );
});

test("keeps an iMessage conversation on iMessage", () => {
  assert.deepEqual(
    chooseMessagesRoute(["person@example.com"], {
      handle: "person@example.com",
      service: "iMessage",
    }),
    {
      handle: "person@example.com",
      appleService: "iMessage",
      observedService: "iMessage",
    },
  );
});

test("keeps an SMS conversation on the Messages SMS account", () => {
  assert.deepEqual(
    chooseMessagesRoute(["+15551234567"], {
      handle: "+15551234567",
      service: "SMS",
    }),
    {
      handle: "+15551234567",
      appleService: "SMS",
      observedService: "SMS",
    },
  );
});

test("defaults an unknown route to iMessage", () => {
  assert.deepEqual(
    chooseMessagesRoute(["+15551234567"]),
    {
      handle: "+15551234567",
      appleService: "iMessage",
      observedService: null,
    },
  );
});

test("classifies asynchronous Messages delivery state", () => {
  assert.deepEqual(classifyMessagesDelivery(null), {
    status: "pending",
    service: null,
    error: 0,
  });
  assert.deepEqual(classifyMessagesDelivery({ service: "iMessage", error: 22 }), {
    status: "failed",
    service: "iMessage",
    error: 22,
  });
  assert.deepEqual(
    classifyMessagesDelivery({ service: "RCS", error: 0, is_sent: 1, is_delivered: 1 }),
    { status: "delivered", service: "RCS", error: 0 },
  );
  assert.deepEqual(
    classifyMessagesDelivery({ service: "SMS", error: 0, is_sent: 1, is_delivered: 0 }),
    { status: "sent", service: "SMS", error: 0 },
  );
});

test("requires an attachment transfer before confirming media delivery", () => {
  assert.deepEqual(classifyMessagesAttachment(null), {
    status: "pending", service: null, error: 0,
  });
  assert.deepEqual(
    classifyMessagesAttachment({ service: "iMessage", error: 0, is_sent: 1, transfer_state: 1 }),
    { status: "pending", service: "iMessage", error: 0, transferState: 1 },
  );
  assert.deepEqual(
    classifyMessagesAttachment({ service: "iMessage", error: 0, is_sent: 0, transfer_state: 6 }),
    { status: "failed", service: "iMessage", error: 0, transferState: 6 },
  );
  assert.deepEqual(
    classifyMessagesAttachment({ service: "iMessage", error: 0, is_sent: 1, is_delivered: 1, transfer_state: 5 }),
    { status: "delivered", service: "iMessage", error: 0, transferState: 5 },
  );
});

test("refuses a focused Messages conversation that is not the recipient", () => {
  assert.equal(conversationTitleMatches("Alex Freundlich", "Alex"), true);
  assert.equal(conversationTitleMatches("Alexis", "Alex"), false);
  assert.equal(conversationTitleMatches("Amy Lynn", "Alex"), false);
  assert.equal(conversationTitleMatches("Maybe: jeffrey", "me@jas.life"), false);
  assert.equal(conversationTitleMatches("", "Maybe: jeffrey"), false);
});

test("preserves an existing Messages draft", () => {
  assert.equal(isMessagesComposerEmpty("Message"), true);
  assert.equal(isMessagesComposerEmpty("iMessage"), true);
  assert.equal(isMessagesComposerEmpty(" design"), false);
});

test("retries only an explicitly failed phone-number iMessage via SMS", () => {
  assert.equal(
    shouldRetryViaSms(
      { handle: "+15551234567", appleService: "iMessage" },
      { status: "failed", error: 22 },
    ),
    true,
  );
  assert.equal(
    shouldRetryViaSms(
      { handle: "+15551234567", appleService: "iMessage" },
      { status: "pending", error: 0 },
    ),
    false,
  );
  assert.equal(
    shouldRetryViaSms(
      { handle: "person@example.com", appleService: "iMessage" },
      { status: "failed", error: 22 },
    ),
    false,
  );
});
