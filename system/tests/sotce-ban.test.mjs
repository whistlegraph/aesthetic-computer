import assert from "node:assert/strict";
import test from "node:test";
import { sotceBanned } from "../../shared/sotce-ban.mjs";
import { ChatManager } from "../../session-server/chat-manager.mjs";

const user = { sub: "auth0|test", email: "reader@example.com" };
function database(keys = []) {
  return { collection(name) {
    assert.equal(name, "sotce-bans");
    return { async findOne(query) {
      return query._id.$in.some(key => keys.includes(key)) ? { _id: "ban" } : null;
    } };
  } };
}

test("ban matches old tokens by sub and alternate accounts by normalized email", async () => {
  assert.equal(await sotceBanned(database(["sub:" + user.sub]), { sub: user.sub }), true);
  assert.equal(await sotceBanned(database(["email:reader@example.com"]), {
    sub: "different-sub", email: " Reader@Example.COM ",
  }), true);
  assert.equal(await sotceBanned(database(), user), false);
});

test("missing identity and unavailable ban store fail closed", async () => {
  assert.equal(await sotceBanned(database(), {}), true);
  assert.equal(await sotceBanned(undefined, user), true);
  assert.equal(await sotceBanned({ collection() { throw new Error("offline"); } }, user), true);
});

for (const type of ["chat:message", "chat:edit", "chat:delete", "chat:heart"]) {
  test(`${type} rejects cached authorization after ban, before mutation`, async () => {
    const manager = Object.create(ChatManager.prototype);
    manager.db = database(["sub:" + user.sub]);
    let mutations = 0;
    for (const method of ["handleChatMessage", "handleEditMessage", "handleDeleteMessage", "handleChatHeart"])
      manager[method] = async () => mutations++;
    const instance = { config: { name: "chat-sotce" },
      authorizedConnections: { socket: { token: "old-token", user } },
      subsToSubscribers: { [user.sub]: true } };
    const messages = [];
    await manager.handleMessage(instance, { send: value => messages.push(value) }, "socket",
      Buffer.from(JSON.stringify({ type, content: { token: "old-token", sub: user.sub } })));
    assert.equal(mutations, 0);
    assert.equal(messages.length, 1);
    assert.equal(instance.authorizedConnections.socket, undefined);
    assert.equal(instance.subsToSubscribers[user.sub], undefined);
  });
}

test("unbanned reader can mutate, but cannot claim another sub", async () => {
  const manager = Object.create(ChatManager.prototype);
  manager.db = database();
  manager.authorize = async () => user;
  manager.accountLocked = async () => false;
  let mutations = 0;
  manager.handleChatMessage = async () => mutations++;
  const instance = { config: { name: "chat-sotce" }, authorizedConnections: {}, subsToSubscribers: {} };
  const ws = { send() {} };
  const message = sub => Buffer.from(JSON.stringify({ type: "chat:message", content: { token: "token", sub } }));
  await manager.handleMessage(instance, ws, "socket", message(user.sub));
  assert.equal(mutations, 1);
  await manager.handleMessage(instance, ws, "socket", message("other-sub"));
  assert.equal(mutations, 1);
});
