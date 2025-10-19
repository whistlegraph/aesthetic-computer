#!/usr/bin/env node

/**
 * Verify that MongoDB paintings and ATProto painting records match for a given user.
 * - Confirms every Mongo painting has an ATProto rkey and matching record.
 * - Confirms there are no duplicate ATProto records for the same slug.
 */

import { connect } from '../../../system/backend/database.mjs';
import { userIDFromHandle } from '../../../system/backend/authorization.mjs';
import { AtpAgent } from '@atproto/api';
import { config } from 'dotenv';

config({ path: '../../../system/.env' });

const handleArg = process.argv[2];
if (!handleArg || !handleArg.startsWith('@')) {
  console.error('Usage: node verify-paintings-sync.mjs @handle [--limit N]');
  process.exit(1);
}

const limitIndex = process.argv.indexOf('--limit');
const limit = limitIndex !== -1 ? parseInt(process.argv[limitIndex + 1], 10) : null;

const handle = handleArg.replace('@', '');

async function fetchAllPaintingsFromPds(agent, did) {
  const allRecords = [];
  let cursor = undefined;

  do {
    const response = await agent.com.atproto.repo.listRecords({
      repo: did,
      collection: 'computer.aesthetic.painting',
      limit: 100,
      cursor,
    });

    const { records = [], cursor: nextCursor } = response.data ?? response;
    allRecords.push(...records);
    cursor = nextCursor;
  } while (cursor);

  return allRecords;
}

async function main() {
  const database = await connect();

  try {
    const userSub = await userIDFromHandle(handle, database);
    if (!userSub) {
      console.error(`❌ Could not find user for handle @${handle}`);
      process.exit(1);
    }

    const users = database.db.collection('users');
    const paintings = database.db.collection('paintings');

    const userDoc = await users.findOne({ _id: userSub });
    if (!userDoc?.atproto?.did || !userDoc?.atproto?.password) {
      console.error(`❌ User @${handle} does not have ATProto credentials stored`);
      process.exit(1);
    }

    const mongoQuery = { user: userSub, nuked: { $ne: true } };
    const mongoPaintingsCursor = paintings.find(mongoQuery).sort({ when: 1 });
    const mongoPaintings = limit ? await mongoPaintingsCursor.limit(limit).toArray() : await mongoPaintingsCursor.toArray();

    console.log(`🎨 Mongo paintings for @${handle}: ${mongoPaintings.length}`);

    const missingRkey = mongoPaintings.filter(p => !p.atproto?.rkey);
    if (missingRkey.length > 0) {
      console.log(`⚠️  ${missingRkey.length} Mongo paintings missing atproto.rkey`);
      missingRkey.slice(0, 5).forEach(p => console.log(`   - ${p.slug}`));
    } else {
      console.log('✅ All Mongo paintings have atproto.rkey values');
    }

    const agent = new AtpAgent({ service: process.env.PDS_URL || 'https://at.aesthetic.computer' });
    await agent.login({ identifier: userDoc.atproto.did, password: userDoc.atproto.password });

    const pdsRecords = await fetchAllPaintingsFromPds(agent, userDoc.atproto.did);
    console.log(`🗂️  ATProto painting records for @${handle}: ${pdsRecords.length}`);

    const slugToRecords = new Map();
    const duplicateSlugs = new Set();

    for (const record of pdsRecords) {
      const slug = record.value?.slug;
      if (!slug) continue;
      const list = slugToRecords.get(slug) ?? [];
      list.push(record);
      slugToRecords.set(slug, list);
      if (list.length > 1) duplicateSlugs.add(slug);
    }

    if (duplicateSlugs.size > 0) {
      console.log('❌ Duplicate ATProto records detected for slugs:');
      for (const slug of duplicateSlugs) {
        const entries = slugToRecords.get(slug) ?? [];
        console.log(`   - ${slug}: ${entries.map(r => r.uri.split('/').pop()).join(', ')}`);
      }
    } else {
      console.log('✅ No duplicate ATProto painting records (by slug)');
    }

    let mismatched = 0;
    let missingRecords = 0;
    let fullyMatched = 0;
    const mongoSlugs = new Set(mongoPaintings.map(p => p.slug));

    for (const painting of mongoPaintings) {
      const slug = painting.slug;
      const rkey = painting.atproto?.rkey;
      const recordList = slugToRecords.get(slug);

      if (!recordList || recordList.length === 0) {
        console.log(`❌ Missing ATProto record for slug ${slug}`);
        missingRecords += 1;
        continue;
      }

      const matched = rkey ? recordList.find(r => r.uri.endsWith(rkey)) : null;
      if (!matched) {
        console.log(`⚠️  Slug ${slug} has ATProto records but none match Mongo rkey ${rkey ?? '(none)'}`);
        mismatched += 1;
        continue;
      }

      // Additional sanity checks
      if (matched.value?.ref && matched.value.ref !== painting._id.toString()) {
        console.log(`⚠️  rkey ${rkey} ref mismatch (ATProto ref=${matched.value.ref}, Mongo _id=${painting._id})`);
        mismatched += 1;
        continue;
      }

      fullyMatched += 1;
    }

    const extraRecords = [];
    for (const [slug, recordList] of slugToRecords.entries()) {
      if (!mongoSlugs.has(slug)) {
        extraRecords.push({ slug, rkeys: recordList.map(r => r.uri.split('/').pop()) });
      }
    }

    console.log('\n📊 Summary');
    console.log(`   Mongo paintings checked: ${mongoPaintings.length}`);
    console.log(`   ATProto records fetched: ${pdsRecords.length}`);
    console.log(`   Fully matched records: ${fullyMatched}`);
    console.log(`   Missing ATProto record: ${missingRecords}`);
    console.log(`   Ref / rkey mismatches: ${mismatched}`);
    console.log(`   ATProto-only records (no Mongo slug): ${extraRecords.length}`);

    if (extraRecords.length > 0) {
      console.log('\nℹ️  ATProto records without matching Mongo slug:');
      extraRecords.slice(0, 5).forEach(({ slug, rkeys }) => {
        console.log(`   - ${slug} (${rkeys.join(', ')})`);
      });
      if (extraRecords.length > 5) {
        console.log(`   ...and ${extraRecords.length - 5} more`);
      }
    }

    if (missingRecords === 0 && mismatched === 0) {
      console.log('\n✅ Paintings look consistent between Mongo and ATProto for this user.');
    } else {
      console.log('\n⚠️  Some inconsistencies detected.');
    }
  } finally {
    await database.disconnect();
  }
}

main()
  .then(() => process.exit(0))
  .catch(error => {
    console.error('❌ Verification failed:', error);
    process.exit(1);
  });
