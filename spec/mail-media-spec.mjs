import * as mailEvents from "../system/backend/mail-events.mjs";
const eventMocks = {
  ...mailEvents,
  recordMailEvent: (_database, fields) => logs.push(mailEvents.mailEvent(fields)),
  observeMail: async (transport, options, _database, operation) => {
    const trace = options.trace || mailEvents.mailTrace();
    const event = (event, fields = {}) => logs.push(mailEvents.mailEvent({ ...fields, event, transport, trace }));
    event("started");
    try { return await operation({ ...options, trace }, event); }
    catch (error) { event("failed", { error: privacy.mailErrorCode(error) }); throw error; }
  },
};
// node --experimental-vm-modules spec/mail-media-spec.mjs
// Real MIME/SMTP and production handlers; isolated in-memory mailbox/auth.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { Readable } from 'node:stream';
import vm from 'node:vm';
import { ObjectId, BSON } from 'mongodb';
import nodemailer from 'nodemailer';
import sharp from 'sharp';
import { simpleParser } from '../lith/node_modules/mailparser/index.js';
import { createInbound, letterText } from '../lith/mail-inbound.mjs';
import * as media from '../system/backend/mail-media.mjs';
import * as privacy from '../shared/mail-privacy.mjs';
import { respond } from '../system/backend/http.mjs';

const logs = [], rows = [], deliveries = [];
const match = (row, query) => Object.entries(query).every(([key, expected]) => {
  if (key === '$or') return expected.some((q) => match(row, q));
  if (expected && typeof expected === 'object' && !(expected instanceof ObjectId)) {
    if ('$ne' in expected) return row[key] !== expected.$ne;
    if ('$in' in expected) return expected.$in.some((v) => v == null ? row[key] == null : row[key] === v);
  }
  return String(row[key]) === String(expected);
});
const noBytes = (row) => ({ ...row, attachments: row.attachments?.map(({ data, ...file }) => file) });
const tells = {
  createIndex: async () => {}, dropIndex: async () => {},
  insertOne: async (row) => {
    if (row.messageId && rows.some((r) => r.to === row.to && r.messageId === row.messageId)) throw Object.assign(new Error('duplicate'), { code: 11000 });
    const _id = new ObjectId(); rows.push({ ...row, _id }); return { insertedId: _id };
  },
  findOne: async (query) => rows.find((row) => match(row, query)),
  find: (query, options) => {
    assert.equal(options?.projection?.['attachments.data'], 0, 'listing must exclude bytes at the database');
    const cursor = { sort: () => cursor, limit: () => cursor, toArray: async () => rows.filter((row) => match(row, query)).map(noBytes) };
    return cursor;
  },
  countDocuments: async (query) => rows.filter((row) => match(row, query)).length,
};
const publicRows = {
  paintings: [{ code: 'art' }, { code: 'secret', private: true }, { code: 'gone', nuked: true }, { code: 'draft', draft: true }, { code: 'hide', hidden: true }, { code: 'delete', deleted: true }, { code: 'unlisted', visibility: 'unlisted' }],
  tapes: [{ code: 'mov' }], kidlisp: [{ code: 'abc' }],
};
const database = { db: { collection: (name) => {
  if (name === 'tells') return tells;
  if (name === 'users') return { findOne: async () => ({ code: 'ac25abcde' }) };
  assert.ok(publicRows[name], `unexpected collection ${name}`);
  return { findOne: async (q) => publicRows[name].find((row) => match(row, q)) };
} }, disconnect: async () => {} };
let identity = { sub: 'recipient' };
const context = vm.createContext({ console: { error: (...args) => logs.push(args) }, Buffer });
async function load(path, mocks) {
  const mod = new vm.SourceTextModule(await readFile(new URL(path, import.meta.url), 'utf8'), {
    context, importModuleDynamically: (name) => synthetic(name),
  });
  async function synthetic(name) {
    assert.ok(mocks[name], `mock ${name}`);
    const module = new vm.SyntheticModule(Object.keys(mocks[name]), function () {
      for (const [key, value] of Object.entries(mocks[name])) this.setExport(key, value);
    }, { context });
    await module.link(() => {}); await module.evaluate(); return module;
  }
  await mod.link(synthetic); await mod.evaluate(); return mod.namespace;
}
context.process = { env: {} };
const backend = await load('../system/backend/mail.mjs', {
  "./mail-events.mjs": eventMocks,
  './authorization.mjs': { handleFor: async (sub) => sub, userIDFromHandleOrEmail: async () => 'recipient' },
  './filter.mjs': { filter: (s) => s }, './shell.mjs': { shell: { log: (...args) => logs.push(args) } },
  './mail-media.mjs': media, '../../shared/mail-privacy.mjs': privacy,
  '../../shared/push.mjs': { sendToUser: async () => ({ attempted: 0, failed: 0 }) },
  nodemailer: { default: { createTransport: () => ({ sendMail: async (letter) => {
    const sent = await nodemailer.createTransport({ streamTransport: true, buffer: true }).sendMail(letter);
    deliveries.push(sent.message); return sent;
  } }) } },
});
const api = await load('../system/netlify/functions/mail.mjs', {
      "../../backend/mail-events.mjs": eventMocks,
  '../../backend/authorization.mjs': { authorize: async () => identity },
  '../../backend/database.mjs': { connect: async () => database },
  '../../backend/http.mjs': { respond }, '../../backend/mail.mjs': backend,
  '../../backend/mail-media.mjs': media, '../../../shared/mail-privacy.mjs': privacy,
  mongodb: { ObjectId },
});
const get = (query = {}) => api.handler({ httpMethod: 'GET', headers: {}, queryStringParameters: query });
const json = (res) => JSON.parse(res.body);

assert.deepEqual(media.mediaCodes('see #art, !mov and $abc. #art https://aesthetic.computer/#art').map((r) => r.label), ['#art', '!mov', '$abc']);
assert.deepEqual(media.mediaCodes('https://other.invalid/#art user#art@test.invalid abc#art #x'), []);
const refs = await media.resolveMailMedia('#art !mov $abc #secret #gone #draft #hide #delete #unlisted #missing', database);
assert.deepEqual(refs.map((r) => r.label), ['#art', '!mov', '$abc']);
assert.deepEqual(refs.map((r) => r.path), ['painting#art', 'video~!mov', '$abc']);
await backend.sendOutside({ from: 'sender', toEmail: 'outside@example.invalid', subject: 'Media', text: '<script>PRIVATE</script> #art !mov $abc' }, database);
const outgoing = await simpleParser(deliveries[0]);
assert.match(outgoing.text, /https:\/\/aesthetic.computer\/#art/);
assert.match(outgoing.html, /<img src="https:\/\/aesthetic.computer\/media\/paintings\/art.png"/);
assert.ok(!outgoing.html.includes('<script>'));
assert.match(outgoing.html, /&lt;script&gt;/);
assert.equal(outgoing.attachments.length, 0, 'outgoing mail references existing media, without copying files');

const png = await sharp({ create: { width: 400, height: 200, channels: 4, background: '#308fc0' } }).png().toBuffer();
const pdf = Buffer.from('%PDF-1.4\nPRIVATE_LETTER_CANARY\n\x00\xff', 'latin1');
const server = createInbound({ domains: ['example.invalid'], open: true,
  lookup: async () => 'recipient', log: (...args) => logs.push(args),
  file: ({ parsed, attachments, sub }) => backend.deliverFromOutside({
    to: sub, fromEmail: 'outside@example.invalid', text: letterText(parsed) || '(an empty letter)',
    messageId: parsed.messageId, attachments, quiet: true,
  }, database),
});
await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
const transport = nodemailer.createTransport({ host: '127.0.0.1', port: server.server.address().port, secure: false, ignoreTLS: true });
try {
  const letter = { from: 'outside@example.invalid', to: 'recipient@example.invalid', messageId: '<private-fixture@example.invalid>',
    html: '<p>Two files</p><img src="cid:photo">',
    attachments: [{ filename: 'photo.png', content: png, cid: 'photo' }, { filename: 'notes.pdf', content: pdf }],
  };
  await transport.sendMail(letter);
  await transport.sendMail(letter);
  const received = rows.filter((row) => row.via === 'smtp');
  assert.equal(received.length, 1, 'relay retries do not duplicate letters/files');
  const row = received[0];
  assert.equal(row.attachments.length, 2);
  assert.deepEqual(Buffer.from(row.attachments[0].data, 'base64'), png);
  assert.deepEqual(Buffer.from(row.attachments[1].data, 'base64'), pdf);
  const list = await get();
  assert.equal(list.statusCode, 200);
  assert.equal(json(list).inbox[0].attachments[0].image, true);
  assert.ok(!list.body.includes(row.attachments[0].data));
  assert.ok(!list.body.includes(row.attachments[1].data));
  const query = { id: String(row._id), attachment: '1' };
  let download = await get(query);
  assert.equal(download.statusCode, 200);
  assert.deepEqual(Buffer.from(download.body, 'base64'), pdf);
  assert.equal(download.headers['Content-Type'], 'application/octet-stream');
  assert.match(download.headers['Cache-Control'], /no-store/);
  assert.match(download.headers['Content-Disposition'], /^attachment;/);
  assert.equal((await get({ ...query, attachment: '99' })).statusCode, 404);
  assert.equal((await get({ ...query, id: 'bad' })).statusCode, 400);
  const preview = await get({ ...query, attachment: '0', preview: '1' });
  assert.equal(preview.statusCode, 200);
  const image = await sharp(Buffer.from(json(preview).data, 'base64')).metadata();
  assert.equal(image.width, 320); assert.equal(image.height, 160);
  assert.equal((await get({ ...query, preview: '1' })).statusCode, 404);
  identity = { sub: 'intruder' };
  for (const options of [{}, { json: '1' }, { preview: '1' }]) assert.equal((await get({ ...query, ...options })).statusCode, 404);
  identity = null;
  assert.equal((await get(query)).statusCode, 401);
  identity = { sub: 'recipient' };
  await transport.sendMail({ from: 'outside@example.invalid', to: 'recipient@example.invalid', attachments: [{ filename: 'image.png', content: png }] });
  assert.equal(rows.filter((r) => r.via === 'smtp').length, 2, 'image-only email is retained');
  const before = rows.length;
  await assert.rejects(transport.sendMail({ from: 'outside@example.invalid', to: 'recipient@example.invalid', attachments: [{ filename: 'large.bin', content: Buffer.alloc(media.MAX_MAIL_FILE_BYTES + 1) }] }), (err) => err.responseCode === 552);
  await assert.rejects(transport.sendMail({ from: 'outside@example.invalid', to: 'recipient@example.invalid', attachments: Array.from({ length: 11 }, () => ({ filename: 'x.txt', content: 'x' })) }), (err) => err.responseCode === 552);
  assert.equal(rows.length, before, 'rejected files do not leave partial letters');
  let rejected;
  await server.options.onData(Readable.from([Buffer.alloc(media.MAX_MAIL_WIRE_BYTES), Buffer.from('x')]), {}, (err) => { rejected = err; });
  assert.equal(rejected.responseCode, 552);
} finally { transport.close(); await new Promise((resolve) => server.close(resolve)); }
const max = media.incomingAttachments([{ filename: '../../unsafe\r\n.bin', contentType: 'bad\r\ntype', content: Buffer.alloc(media.MAX_MAIL_FILE_BYTES) }]);
assert.equal(max[0].name, 'unsafe.bin'); assert.equal(max[0].type, 'application/octet-stream');
assert.ok(BSON.calculateObjectSize({ attachments: max, text: 'x'.repeat(2000) }) < 16 * 1024 * 1024);
assert.equal(await media.attachmentThumbnail({ type: 'image/png', data: 'YmFk' }), null);
assert.ok(!JSON.stringify(logs).includes('PRIVATE_LETTER_CANARY'));
console.log('mail media spec passed: code references, outbound MIME, SMTP inline images/files, retries, byte limits, mailbox authorization, downloads, previews and privacy');
