import { createHash, randomUUID } from 'node:crypto';
import { getPkhfromPk, verifySignature, validateAddress } from '@taquito/utils';
import { chainJSON } from './tezos-credits.mjs';

export const HEN_MINTER = 'KT1Hkg5qeNhfwpKW4fXvq7HGZB9z2EnmCCA9';
export const HEN_OBJKTS = 'KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton';
export const hash = text => createHash('sha256').update(text).digest('hex');
export const mintError = (status, message) => Object.assign(new Error(message), { status });
const publicState = i => ({ id:i._id, code:i.code, version:i.version, sourceHash:i.sourceHash, density:i.density, aspect:i.aspect,
  title:i.title, description:i.description, editions:i.editions, royalties:i.royalties, handle:i.handle,
  status:i.status, sender:i.sender, artifactUri:i.artifactUri, coverUri:i.coverUri, metadataUri:i.metadataUri,
  artifactHash:i.artifactHash, bytes:i.bytes, operationHash:i.operationHash, tokenId:i.tokenId,
  error:i.error, network:'mainnet', contract:HEN_OBJKTS });

export function mintPayload(i) {
  const text = `Tezos Signed Message: Mint Whistlegraph artwork\nhttps://aesthetic.computer\nMint: ${i._id}\nAccount: @${i.handle}\nPiece: ${i.code} v${i.version}\nSource: ${i.sourceHash}\nNonce: ${i.nonce}`;
  const bytes = Buffer.from(text);
  return '0501' + bytes.length.toString(16).padStart(8, '0') + bytes.toString('hex');
}
export function verifyMintWallet(i, proof) {
  try {
    return /^tz[123]/.test(proof.address) && validateAddress(proof.address) === 3 &&
      getPkhfromPk(proof.publicKey) === proof.address && verifySignature(mintPayload(i), proof.publicKey, proof.signature);
  } catch { return false; }
}
export function mintMetadata(i) {
  return { name:i.title, description:i.description, tags:['whistlegraph', 'aesthetic-computer', 'interactive', 'generative'],
    symbol:'OBJKT', artifactUri:i.artifactUri, displayUri:i.coverUri, thumbnailUri:i.coverUri,
    creators:[i.sender], formats:[{ uri:i.artifactUri, mimeType:'text/html' }], decimals:0,
    isBooleanAmount:false, shouldPreferSymbol:false, date:new Date(i.createdAt).toISOString(),
    whistlegraph:{ code:i.code, version:i.version, sourceHash:i.sourceHash, artifactHash:i.artifactHash,
      pixelSize:i.density, previewAspect:i.aspect, mintId:i._id, packVersion:i.packVersion } };
}
export function mintOperation(i) {
  return { kind:'transaction', destination:HEN_MINTER, amount:'0', parameters:{ entrypoint:'mint_OBJKT',
    value:{ prim:'Pair', args:[{ prim:'Pair', args:[{ string:i.sender }, { int:String(i.editions) }] },
      { prim:'Pair', args:[{ bytes:Buffer.from(i.metadataUri).toString('hex') }, { int:String(i.royalties) }] }] } } };
}
export function matchingMint(i, tx) {
  const p = tx.parameter?.value;
  return tx.status === 'applied' && tx.target?.address === HEN_MINTER && tx.sender?.address === i.sender &&
    tx.nonce == null && (!tx.initiator || tx.initiator.address === i.sender) && String(tx.amount) === '0' &&
    tx.parameter?.entrypoint === 'mint_OBJKT' && p?.address === i.sender && String(p.amount) === String(i.editions) &&
    p.metadata === Buffer.from(i.metadataUri).toString('hex') && String(p.royalties) === String(i.royalties) &&
    Number.isSafeInteger(tx.id) && Number.isSafeInteger(tx.level) && Number.isSafeInteger(tx.counter) && Date.parse(tx.timestamp) >= +new Date(i.createdAt);
}

// Persist each step before opening a wallet. A lost browser callback recovers by
// the unique metadata URI, never by an approximate amount or the latest token.
export function whistlegraphMints({ intents, threads, pack, pin, cover, chain = chainJSON, verify = verifyMintWallet, now = () => new Date() }) {
  async function lookup(secret) {
    if (typeof secret !== 'string' || !/^[a-f0-9]{64}$/.test(secret)) throw mintError(401, 'Invalid mint link');
    const i = await intents.findOne({ _id:hash(secret) });
    if (!i) throw mintError(404, 'Mint not found');
    return i;
  }
  async function alive(i) {
    if (+now() - +new Date(i.createdAt) > 24 * 3600_000) throw mintError(409, 'Mint preview expired. Open a new preview from Whistlegraph.');
  }
  return {
    async create(user, handle, input) {
      if (!/^[a-f0-9]{64}$/.test(input.secret || '')) throw mintError(400, 'Invalid request key');
      const existing = await intents.findOne({ _id:hash(input.secret) });
      if (existing) {
        if (existing.user !== user) throw mintError(403, 'Mint belongs to another account');
        return publicState(existing);
      }
      if (!/^(?:wg|ww)[a-z]{5,12}$/i.test(input.code || '') || !Number.isSafeInteger(input.version) ||
          !Number.isInteger(input.density) || input.density < 1 || input.density > 4 ||
          !['2:3','9:16','1:1','4:3','16:9'].includes(input.aspect ?? '2:3') ||
          typeof input.title !== 'string' || !input.title.trim() || input.title.length > 120 ||
          typeof input.description !== 'string' || input.description.length > 2000 ||
          !Number.isInteger(input.editions) || input.editions < 1 || input.editions > 10000 ||
          !Number.isInteger(input.royalties) || input.royalties < 0 || input.royalties > 250) throw mintError(400, 'Invalid mint settings');
      if (await intents.countDocuments({ user, createdAt:{ $gt:new Date(+now() - 3600_000) } }) >= 5) throw mintError(429, 'Too many previews. Reopen an existing mint.');
      const row = await threads.findOne({ owner:user, codeKey:input.code.toLowerCase() });
      const version = row?.ledger?.versions.find(v => v.id === input.version);
      if (!version) throw mintError(404, 'Save this version to your AC account first');
      if (typeof version.source !== 'string' || !version.source.trim() || Buffer.byteLength(version.source) > 500_000 || hash(version.source) !== input.sourceHash) throw mintError(409, 'The saved version differs from the preview. Reopen it before minting.');
      const png = await cover(input.cover);
      const i = { _id:hash(input.secret), user, handle:handle.replace(/^@/, ''), code:row.code, version:version.id,
        source:version.source, sourceHash:input.sourceHash, density:input.density, aspect:input.aspect ?? '2:3', title:input.title.trim(),
        description:input.description.trim(), editions:input.editions, royalties:input.royalties,
        cover:png.toString('base64'), nonce:randomUUID(), createdAt:now(), status:'packing' };
      try { await intents.insertOne(i); } catch (e) {
        if (e.code !== 11000) throw e;
        return this.create(user, handle, input);
      }
      return publicState(i);
    },
    async status(secret) {
      const i = await lookup(secret);
      return { ...publicState(i), payload:mintPayload(i) };
    },
    async prepare(secret) {
      const i = await lookup(secret);
      if (i.status !== 'packing') return publicState(i);
      await alive(i);
      const claimed = await intents.updateOne({ _id:i._id, status:'packing', $or:[{ lease:{ $exists:false } }, { lease:{ $lt:now() } }] },
        { $set:{ lease:new Date(+now() + 10 * 60_000) } });
      if (!claimed.modifiedCount) return publicState(i);
      try {
        const packed = await pack(i);
        const artifactUri = await pin('index.html', 'text/html', Buffer.from(packed.html));
        const coverUri = await pin('cover.png', 'image/png', Buffer.from(i.cover, 'base64'));
        await intents.updateOne({ _id:i._id, status:'packing' }, { $set:{ status:'packed', artifactUri, coverUri,
          artifactHash:hash(packed.html), bytes:Buffer.byteLength(packed.html), packVersion:packed.version }, $unset:{ source:'', cover:'', lease:'' } });
      } catch (error) {
        await intents.updateOne({ _id:i._id, status:'packing' }, { $set:{ status:'failed', error:'Packing failed. Open a new mint preview.' }, $unset:{ source:'', cover:'', lease:'' } });
        throw error;
      }
      return publicState(await lookup(secret));
    },
    async bind(secret, proof) {
      const i = await lookup(secret);
      await alive(i);
      if (i.sender && i.sender !== proof.address) throw mintError(409, 'This mint belongs to a different wallet');
      if (i.metadataUri) return publicState(i);
      if (i.status !== 'packed' && i.status !== 'binding') throw mintError(409, 'Wait for the HTML pack');
      if (!verify(i, proof)) throw mintError(403, 'Wallet signature did not verify');
      await intents.updateOne({ _id:i._id, status:'packed' }, { $set:{ status:'binding', sender:proof.address } });
      const bound = await lookup(secret);
      if (bound.sender !== proof.address) throw mintError(409, 'Another wallet claimed this mint');
      const metadataUri = await pin('metadata.json', 'application/json', Buffer.from(JSON.stringify(mintMetadata(bound))));
      await intents.updateOne({ _id:i._id, status:'binding', sender:proof.address }, { $set:{ status:'ready', metadataUri } });
      return publicState(await lookup(secret));
    },
    async begin(secret) {
      const i = await lookup(secret); await alive(i);
      if (i.status !== 'ready') throw mintError(409, 'A wallet request is already pending. Check the mint before trying again.');
      const changed = await intents.updateOne({ _id:i._id, status:'ready' }, { $set:{ status:'requested', requestedAt:now() } });
      if (!changed.modifiedCount) throw mintError(409, 'A wallet request is already pending');
      return { ...publicState({ ...i, status:'requested' }), operation:mintOperation(i) };
    },
    async cancel(secret) {
      const i = await lookup(secret);
      if (['requested','confirming','minted'].includes(i.status)) throw mintError(409, 'A wallet request exists. Check that mint before making another.');
      const changed = await intents.updateOne({ _id:i._id, status:i.status },
        { $set:{ status:'cancelled' }, $unset:{ source:'', cover:'' } });
      if (!changed.modifiedCount) throw mintError(409, 'Mint state changed. Check it again.');
      return publicState(await lookup(secret));
    },
    async confirm(secret, operationHash) {
      const i = await lookup(secret);
      if (i.status === 'minted') return publicState(i);
      if (!['requested','confirming'].includes(i.status)) return publicState(i);
      if (operationHash && (typeof operationHash !== 'string' || !/^o[1-9A-HJ-NP-Za-km-z]{50}$/.test(operationHash))) throw mintError(400, 'Invalid operation hash');
      const head = await chain('/head');
      if (head.chain !== 'mainnet' || head.chainId !== 'NetXdQprcVkpaWU' || !head.synced || !Number.isSafeInteger(head.level)) throw mintError(503, 'Tezos is still syncing');
      const op = i.operationHash || operationHash;
      const query = new URLSearchParams({ sender:i.sender, target:HEN_MINTER, 'parameter.entrypoint':'mint_OBJKT',
        'parameter.metadata':Buffer.from(i.metadataUri).toString('hex'), 'timestamp.ge':new Date(i.createdAt).toISOString(), limit:'100', 'sort.desc':'id' });
      const txs = await chain(op ? `/operations/transactions/${op}` : `/operations/transactions?${query}`);
      const tx = txs.find(tx => matchingMint(i, tx));
      if (!tx) {
        if (op && txs.length) throw mintError(409, 'That operation is not an applied mint of this artwork');
        return publicState(i);
      }
      await intents.updateOne({ _id:i._id, status:{ $ne:'minted' } }, { $set:{ status:'confirming', operationHash:tx.hash } });
      if (head.level - tx.level + 1 < 3) return publicState(await lookup(secret));
      const operations = await chain(`/operations/transactions/${tx.hash}`);
      if (!operations.length || operations.some(t => t.status !== 'applied')) throw mintError(409, 'The mint operation did not fully apply');
      // A wallet may batch several top-level calls under one operation hash.
      // Only transfers caused by this mint's counter identify its token.
      const ids = operations.filter(t => t.counter === tx.counter).map(t => t.id).join(',');
      const transfers = await chain(`/tokens/transfers?transactionId.in=${ids}&token.contract=${HEN_OBJKTS}&limit=100`);
      const minted = transfers.find(t => !t.from && t.to?.address === i.sender && String(t.amount) === String(i.editions) &&
        t.token?.contract?.address === HEN_OBJKTS && /^\d+$/.test(String(t.token.tokenId)));
      if (!minted) return publicState(await lookup(secret));
      await intents.updateOne({ _id:i._id, status:'confirming', operationHash:tx.hash },
        { $set:{ status:'minted', tokenId:String(minted.token.tokenId), mintedAt:now() } });
      return publicState(await lookup(secret));
    },
  };
}
