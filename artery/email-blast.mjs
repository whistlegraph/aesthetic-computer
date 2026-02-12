#!/usr/bin/env node
// email-blast.mjs
// Send email blast to all Auth0 users about give.aesthetic.computer
//
// Usage:
//   node artery/email-blast.mjs --list           # List all users (summary)
//   node artery/email-blast.mjs --export         # Export all emails to CSV
//   node artery/email-blast.mjs --test EMAIL     # Send test email
//   node artery/email-blast.mjs --preview        # Preview email content
//   node artery/email-blast.mjs --send           # Send to VERIFIED users (default)
//   node artery/email-blast.mjs --send --all     # Send to ALL users
//   node artery/email-blast.mjs --send --resume  # Resume from last sent
//
// Gmail SMTP limits:
//   ~500 emails/day with app passwords.
//   Verified users (~5.3k) = ~11 days. All users (~18k) = ~36 days.
//   Use --resume to continue across multiple days.

import { config } from 'dotenv';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { createTransport } from 'nodemailer';
import { createInterface } from 'readline';
import { existsSync, readFileSync, writeFileSync, appendFileSync, unlinkSync } from 'fs';
import { createHmac, timingSafeEqual } from 'crypto';

const __dirname = dirname(fileURLToPath(import.meta.url));

// Load env from at/.env (has Auth0 M2M creds)
config({ path: join(__dirname, '../at/.env') });

// Secrets loaded from MongoDB at startup
let SECRETS = null;

async function loadSecrets() {
  if (SECRETS) return SECRETS;
  const { connect } = await import('../system/backend/database.mjs');
  const db = await connect();
  SECRETS = await db.db.collection('secrets').findOne({ _id: 'email-blast' });
  await db.disconnect();
  if (!SECRETS) throw new Error('email-blast secrets not found in database');
  return SECRETS;
}

// HMAC token generation for unsubscribe links
function generateUnsubscribeToken(email) {
  const secret = SECRETS?.unsubscribeSecret;
  if (!secret) {
    console.warn('WARNING: unsubscribeSecret not loaded — unsubscribe links will not work');
    return 'no-secret';
  }
  return createHmac('sha256', secret)
    .update(email.toLowerCase().trim())
    .digest('hex');
}

function getUnsubscribeUrl(email) {
  const token = generateUnsubscribeToken(email);
  return `https://aesthetic.computer/api/unsubscribe?email=${encodeURIComponent(email)}&token=${token}`;
}

// File paths for tracking (user data goes to vault, logs stay in scratch)
const VAULT_DIR = join(__dirname, '../aesthetic-computer-vault/user-reports');
const SENT_LOG_FILE = join(__dirname, '../scratch/email-blast-sent.log');
const FAILED_LOG_FILE = join(__dirname, '../scratch/email-blast-failed.log');
const EXPORT_FILE = join(VAULT_DIR, 'email-blast-users.csv');
const USERS_CACHE_FILE = join(VAULT_DIR, 'email-blast-users.json');
const FETCH_CHECKPOINT_FILE = join(__dirname, '../scratch/email-blast-checkpoint.json');

// Save fetch checkpoint
function saveFetchCheckpoint(from, users) {
  writeFileSync(FETCH_CHECKPOINT_FILE, JSON.stringify({ 
    from, 
    count: users.length,
    timestamp: new Date().toISOString() 
  }));
  // Also save all users fetched so far
  writeFileSync(USERS_CACHE_FILE, JSON.stringify(users, null, 2));
}

// Load fetch checkpoint
function loadFetchCheckpoint() {
  if (!existsSync(FETCH_CHECKPOINT_FILE)) return null;
  try {
    return JSON.parse(readFileSync(FETCH_CHECKPOINT_FILE, 'utf-8'));
  } catch { return null; }
}

// Load cached users
function loadCachedUsers() {
  if (!existsSync(USERS_CACHE_FILE)) return [];
  try {
    return JSON.parse(readFileSync(USERS_CACHE_FILE, 'utf-8'));
  } catch { return []; }
}

// Clear fetch checkpoint
function clearFetchCheckpoint() {
  if (existsSync(FETCH_CHECKPOINT_FILE)) unlinkSync(FETCH_CHECKPOINT_FILE);
  if (existsSync(USERS_CACHE_FILE)) unlinkSync(USERS_CACHE_FILE);
}

// Create an Auth0 export job to get ALL users (for 16k+)
async function createExportJob() {
  const { got } = await import('got');
  const { token, baseURI } = await getAuth0Token();
  
  console.log('📤 Creating Auth0 user export job...\n');
  
  const response = await got.post(`${baseURI}/api/v2/jobs/users-exports`, {
    json: {
      format: 'json',
      fields: [
        { name: 'user_id' },
        { name: 'email' },
        { name: 'email_verified' },
        { name: 'created_at' },
        { name: 'last_login' },
        { name: 'logins_count' },
      ],
    },
    headers: { Authorization: `Bearer ${token}` },
    responseType: 'json',
  });
  
  return response.body;
}

// Check export job status
async function checkExportJob(jobId) {
  const { got } = await import('got');
  const { token, baseURI } = await getAuth0Token();
  
  const response = await got(`${baseURI}/api/v2/jobs/${jobId}`, {
    headers: { Authorization: `Bearer ${token}` },
    responseType: 'json',
  });
  
  return response.body;
}

// Download and parse export
async function downloadExport(url) {
  const { got } = await import('got');
  const { createGunzip } = await import('zlib');
  const { pipeline } = await import('stream/promises');
  const { Readable } = await import('stream');
  
  console.log('📥 Downloading export...');
  
  const response = await got(url, { responseType: 'buffer' });
  
  // Auth0 exports are gzipped NDJSON
  const gunzip = createGunzip();
  const chunks = [];
  
  await new Promise((resolve, reject) => {
    const input = Readable.from(response.body);
    input.pipe(gunzip);
    gunzip.on('data', chunk => chunks.push(chunk));
    gunzip.on('end', resolve);
    gunzip.on('error', reject);
  });
  
  const text = Buffer.concat(chunks).toString('utf-8');
  const users = text.trim().split('\n').map(line => JSON.parse(line));
  
  return users;
}

// Full export flow for 16k+ users
async function exportAllUsers() {
  console.log('\n🚀 AUTH0 BULK EXPORT (for 16k+ users)\n');
  
  // Create export job
  const job = await createExportJob();
  console.log(`   Job ID: ${job.id}`);
  console.log(`   Status: ${job.status}`);
  
  // Poll for completion
  let status = job.status;
  let result = job;
  let dots = 0;
  
  while (status === 'pending' || status === 'processing') {
    await new Promise(r => setTimeout(r, 2000));
    result = await checkExportJob(job.id);
    status = result.status;
    dots++;
    process.stdout.write(`\r   Waiting${'.'.repeat(dots % 4).padEnd(3)} (${status})`);
  }
  
  console.log(`\n   Final status: ${status}`);
  
  if (status !== 'completed') {
    console.log(`\n❌ Export failed: ${result.error || 'Unknown error'}`);
    return [];
  }
  
  // Download the export
  const users = await downloadExport(result.location);
  
  // Save to cache
  saveFetchCheckpoint('export', users);
  
  const verified = users.filter(u => u.email_verified).length;
  console.log(`\n${'═'.repeat(50)}`);
  console.log(`📊 EXPORT COMPLETE`);
  console.log(`   Total users: ${users.length}`);
  console.log(`   Verified: ${verified}`);
  console.log(`   Saved to: aesthetic-computer-vault/user-reports/`);
  console.log(`${'═'.repeat(50)}\n`);
  
  return users;
}

// SMTP config — loaded from DB secrets at runtime
function getSmtpConfig() {
  if (!SECRETS) throw new Error('Secrets not loaded — call loadSecrets() first');
  return {
    host: SECRETS.smtpHost || 'smtp.gmail.com',
    port: SECRETS.smtpPort || 465,
    secure: true,
    auth: {
      user: SECRETS.smtpUser,
      pass: SECRETS.smtpPass,
    },
  };
}

const EMAIL_SUBJECT = 'help aesthetic.computer stay online 💾';

function getEmailText(recipientEmail) {
  const unsubUrl = getUnsubscribeUrl(recipientEmail);
  return `hi!

you signed up for aesthetic.computer and i really appreciate that.

aesthetic computer needs your help to keep running.

if you can, please visit: https://give.aesthetic.computer

you can give with:
  card (USD or DKK)
  Tezos (XTZ)
  Ethereum (ETH)
  Bitcoin (BTC)

you can also add a personal note, or subscribe monthly to become an ongoing supporter.

every bit helps. thank you for being part of this!

— jas

---
Unsubscribe: ${unsubUrl}`;
}

function getEmailHtml(recipientEmail) {
  const unsubUrl = getUnsubscribeUrl(recipientEmail);
  return `<p>hi!</p>

<p>you signed up for aesthetic.computer and i really appreciate that.</p>

<p>aesthetic computer needs your help to keep running.</p>

<p>if you can, please visit: <a href="https://give.aesthetic.computer">give.aesthetic.computer</a></p>

<p>you can give with:<br>
&nbsp;&nbsp;💳 card (USD or DKK)<br>
&nbsp;&nbsp;🔮 Tezos (XTZ)<br>
&nbsp;&nbsp;💎 Ethereum (ETH)<br>
&nbsp;&nbsp;🟠 Bitcoin (BTC)</p>

<p>you can also add a personal note, or subscribe monthly to become an ongoing supporter.</p>

<p>every bit helps. thank you for being part of this!</p>

<p>— jas</p>

<hr style="border: none; border-top: 1px solid #444; margin: 24px 0;">
<p style="font-size: 12px; color: #888;">
  <a href="${unsubUrl}" style="color: #888;">Unsubscribe</a> from aesthetic.computer emails
</p>`;
}

// Get Auth0 access token
async function getAuth0Token() {
  const { got } = await import('got');
  
  const clientId = process.env.AUTH0_M2M_CLIENT_ID;
  const clientSecret = process.env.AUTH0_M2M_SECRET;
  const baseURI = 'https://aesthetic.us.auth0.com';

  if (!clientId || !clientSecret) {
    throw new Error('Missing AUTH0_M2M_CLIENT_ID or AUTH0_M2M_SECRET in env');
  }

  const response = await got.post(`${baseURI}/oauth/token`, {
    json: {
      client_id: clientId,
      client_secret: clientSecret,
      audience: `${baseURI}/api/v2/`,
      grant_type: 'client_credentials',
    },
    responseType: 'json',
  });

  return { token: response.body.access_token, baseURI };
}

// Get ALL Auth0 users - use page-based for first 1000, then checkpoint for rest
async function getAllAuth0Users(resume = false, showUsers = true) {
  const { got } = await import('got');
  const { token, baseURI } = await getAuth0Token();

  let allUsers = [];
  const perPage = 100;
  
  // Track existing user IDs to avoid duplicates
  const existingIds = new Set();

  // Check for resume
  if (resume) {
    const cached = loadCachedUsers();
    if (cached.length > 0) {
      for (const u of cached) {
        if (!existingIds.has(u.user_id)) {
          existingIds.add(u.user_id);
          allUsers.push(u);
        }
      }
      console.log(`\n🔄 RESUMING: Loaded ${allUsers.length} unique users from cache\n`);
    }
  }

  if (allUsers.length === 0) {
    console.log('📥 Fetching ALL Auth0 users...\n');
  }

  // Phase 1: Page-based pagination (works up to ~1000 users reliably)
  let page = 0;
  let pageBasedDone = false;
  
  while (!pageBasedDone && page < 100) { // Max 10,000 via pages
    try {
      const response = await got(`${baseURI}/api/v2/users`, {
        searchParams: {
          per_page: perPage,
          page: page,
          include_totals: true,
          fields: 'user_id,email,email_verified,created_at',
          include_fields: true,
        },
        headers: { Authorization: `Bearer ${token}` },
        responseType: 'json',
      });

      const data = response.body;
      const users = data.users || data;
      const total = data.total || 0;
      
      if (users.length === 0) {
        pageBasedDone = true;
        break;
      }
      
      let newCount = 0;
      for (const user of users) {
        if (existingIds.has(user.user_id)) continue;
        existingIds.add(user.user_id);
        allUsers.push(user);
        newCount++;
        
        if (showUsers) {
          const email = (user.email || '(no email)').slice(0, 35).padEnd(37);
          const date = new Date(user.created_at).toISOString().slice(0, 10);
          const v = user.email_verified ? '✓' : '✗';
          console.log(`${v} ${date}  ${email}  [${allUsers.length}]`);
        }
      }
      
      page++;
      
      // Save checkpoint every 500 users
      if (allUsers.length % 500 < perPage && allUsers.length >= 500) {
        const verifiedCount = allUsers.filter(u => u.email_verified).length;
        saveFetchCheckpoint('page:' + page, allUsers);
        console.log(`\n   💾 Checkpoint: ${allUsers.length} users (${verifiedCount} verified) [page ${page}]\n`);
      }
      
      // Check if we've gotten all users
      if (allUsers.length >= total || users.length < perPage) {
        pageBasedDone = true;
      }

      await new Promise(r => setTimeout(r, 100));
      
    } catch (error) {
      if (error.response?.statusCode === 429) {
        console.log(`\n   ⏳ Rate limited, waiting 5s...`);
        saveFetchCheckpoint('page:' + page, allUsers);
        await new Promise(r => setTimeout(r, 5000));
        continue;
      }
      if (error.response?.statusCode === 400) {
        // Hit the 1000 user limit for page-based
        console.log(`\n   ℹ️  Page-based limit reached at page ${page}`);
        pageBasedDone = true;
        break;
      }
      saveFetchCheckpoint('page:' + page, allUsers);
      throw error;
    }
  }

  // Final save
  const verifiedCount = allUsers.filter(u => u.email_verified).length;
  saveFetchCheckpoint('done', allUsers);
  
  console.log(`\n${'═'.repeat(50)}`);
  console.log(`📊 FETCH COMPLETE`);
  console.log(`   Total users: ${allUsers.length}`);
  console.log(`   Verified: ${verifiedCount}`);
  console.log(`   Saved to: aesthetic-computer-vault/user-reports/`);
  console.log(`${'═'.repeat(50)}\n`);
  
  return allUsers;
}

// Quick y/n prompt
function confirmContinue(question) {
  const rl = createInterface({ input: process.stdin, output: process.stdout });
  return new Promise(resolve => {
    rl.question(question, answer => {
      rl.close();
      resolve(answer.toLowerCase() !== 'n' && answer.toLowerCase() !== 'no');
    });
  });
}

// Get handles from MongoDB
async function getHandles() {
  const { connect } = await import('../system/backend/database.mjs');
  const database = await connect();
  const handles = database.db.collection('@handles');
  
  const allHandles = await handles.find({}).toArray();
  const handleMap = new Map();
  
  for (const h of allHandles) {
    handleMap.set(h._id, h.handle);
  }
  
  await database.disconnect();
  return handleMap;
}

// List all users (summary only for large counts)
async function listUsers(resume = false) {
  console.log('\n📋 FETCHING ALL AUTH0 USERS\n');

  const users = await getAllAuth0Users(resume, true);
  const handles = await getHandles();

  let verifiedCount = 0;
  let withHandleCount = 0;

  for (const user of users) {
    if (user.email_verified) verifiedCount++;
    if (handles.get(user.user_id)) withHandleCount++;
  }

  console.log(`\n📊 FINAL SUMMARY:`);
  console.log(`   Total users: ${users.length}`);
  console.log(`   Email verified: ${verifiedCount}`);
  console.log(`   With handles: ${withHandleCount}`);
  console.log(`   Local cache: vault/user-reports/`);
}

// Just load cached users (no fetch)
async function showCachedUsers() {
  const users = loadCachedUsers();
  if (users.length === 0) {
    console.log('\n❌ No cached users. Run --fetch first.\n');
    return;
  }
  
  const handles = await getHandles();
  let verifiedCount = 0;
  let withHandleCount = 0;

  console.log('\n📋 CACHED USERS (from local file):\n');
  console.log('─'.repeat(70));
  
  for (const user of users) {
    const email = (user.email || '(no email)').slice(0, 35).padEnd(37);
    const date = new Date(user.created_at).toISOString().slice(0, 10);
    const v = user.email_verified ? '✓' : '✗';
    const handle = handles.get(user.user_id);
    
    if (user.email_verified) verifiedCount++;
    if (handle) withHandleCount++;
    
    console.log(`${v} ${date}  ${email}  ${handle ? '@' + handle : ''}`);
  }
  
  console.log('─'.repeat(70));
  console.log(`\n📊 SUMMARY:`);
  console.log(`   Total: ${users.length}`);
  console.log(`   Verified: ${verifiedCount}`);
  console.log(`   With handles: ${withHandleCount}`);
}

// Export all users to CSV
async function exportUsers() {
  console.log('\n📤 EXPORTING ALL USERS TO CSV\n');

  const users = await getAllAuth0Users();
  const handles = await getHandles();

  let verifiedCount = 0;
  const lines = ['email,created_at,verified,handle'];

  for (const user of users) {
    const email = user.email || '';
    const created = user.created_at || '';
    const verified = user.email_verified ? 'yes' : 'no';
    const handle = handles.get(user.user_id) || '';
    
    if (user.email_verified) verifiedCount++;
    
    // Escape commas in email
    const safeEmail = email.includes(',') ? `"${email}"` : email;
    lines.push(`${safeEmail},${created},${verified},${handle}`);
  }

  writeFileSync(EXPORT_FILE, lines.join('\n'));
  
  console.log(`✅ Exported ${users.length} users to:`);
  console.log(`   ${EXPORT_FILE}`);
  console.log(`\n📊 Summary:`);
  console.log(`   Total: ${users.length}`);
  console.log(`   Verified: ${verifiedCount}`);
}

// Preview email content
async function previewEmail() {
  await loadSecrets();
  console.log('\n📧 EMAIL PREVIEW\n');
  console.log('─'.repeat(60));
  console.log(`From: ${SECRETS.smtpUser}`);
  console.log(`Subject: ${EMAIL_SUBJECT}`);
  console.log('─'.repeat(60));
  console.log(getEmailText('preview@example.com'));
  console.log('─'.repeat(60));
}

// Send a single email
async function sendEmail(transporter, to) {
  const unsubUrl = getUnsubscribeUrl(to);
  const result = await transporter.sendMail({
    from: '"Aesthetic Computer" <mail@aesthetic.computer>',
    to,
    subject: EMAIL_SUBJECT,
    text: getEmailText(to),
    html: getEmailHtml(to),
    headers: {
      'List-Unsubscribe': `<${unsubUrl}>`,
      'List-Unsubscribe-Post': 'List-Unsubscribe=One-Click',
    },
  });
  return result;
}

// Send test email
async function sendTestEmail(testEmail) {
  await loadSecrets();
  console.log(`\n📧 Sending test email to: ${testEmail}\n`);

  const transporter = createTransport(getSmtpConfig());

  try {
    const result = await sendEmail(transporter, testEmail);
    console.log(`✅ Test email sent! Message ID: ${result.messageId}`);
  } catch (error) {
    console.error(`❌ Failed to send: ${error.message}`);
  }
}

// Prompt for confirmation
function confirm(question) {
  const rl = createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  return new Promise(resolve => {
    rl.question(question, answer => {
      rl.close();
      resolve(answer.toLowerCase() === 'yes' || answer.toLowerCase() === 'y');
    });
  });
}

// Get already-sent emails from log
function getSentEmails() {
  if (!existsSync(SENT_LOG_FILE)) return new Set();
  const content = readFileSync(SENT_LOG_FILE, 'utf-8');
  return new Set(content.split('\n').filter(Boolean));
}

// Log a sent email
function logSentEmail(email) {
  appendFileSync(SENT_LOG_FILE, email + '\n');
}

// Log a failed email
function logFailedEmail(email, error) {
  appendFileSync(FAILED_LOG_FILE, `${email}|${error}\n`);
}

// Record blast metadata in MongoDB
async function createBlastRecord(subject, totalAttempted) {
  const { connect } = await import('../system/backend/database.mjs');
  const db = await connect();
  const col = db.db.collection('email-blast-history');
  const result = await col.insertOne({
    when: new Date(),
    subject,
    status: 'in-progress',
    totalAttempted,
    sent: 0,
    failed: 0,
    lastUpdated: new Date(),
  });
  await db.disconnect();
  return result.insertedId;
}

async function updateBlastRecord(blastId, update) {
  const { connect } = await import('../system/backend/database.mjs');
  const { ObjectId } = await import('mongodb');
  const db = await connect();
  const col = db.db.collection('email-blast-history');
  await col.updateOne(
    { _id: blastId instanceof ObjectId ? blastId : new ObjectId(blastId) },
    { $set: { ...update, lastUpdated: new Date() } },
  );
  await db.disconnect();
}

// Get muted users from MongoDB
async function getMutedUsers() {
  const { connect } = await import('../system/backend/database.mjs');
  const db = await connect();

  const mutesCollections = ['chat-sotce-mutes', 'chat-clock-mutes', 'chat-system-mutes'];
  const mutedUsers = new Set();

  for (const col of mutesCollections) {
    const mutes = await db.db.collection(col).find({}).toArray();
    mutes.forEach(m => mutedUsers.add(m.user));
  }

  await db.disconnect();
  return mutedUsers;
}

// Get unsubscribed emails from MongoDB
async function getUnsubscribedEmails() {
  const { connect } = await import('../system/backend/database.mjs');
  const db = await connect();

  const docs = await db.db.collection('email-blast-unsubscribes').find({}).toArray();
  const emails = new Set(docs.map(d => d.email.toLowerCase()));

  await db.disconnect();
  return emails;
}

// Send blast to verified users by default, excluding muted + unsubscribed
async function sendBlast(resume = false, verifiedOnly = true) {
  await loadSecrets();
  console.log('\n🚀 EMAIL BLAST MODE\n');

  // Load cached users
  let users = loadCachedUsers();
  if (users.length === 0) {
    console.log('⚠️  No cached users. Fetching...');
    users = await getAllAuth0Users(false, false);
  }

  // Get muted users and unsubscribed emails
  console.log('📋 Checking muted users...');
  const mutedUsers = await getMutedUsers();
  console.log('📋 Checking unsubscribed emails...');
  const unsubscribedEmails = await getUnsubscribedEmails();

  // Filter users
  let eligibleUsers = users.filter(u =>
    u.email &&
    !mutedUsers.has(u.user_id) &&
    !unsubscribedEmails.has(u.email.toLowerCase())
  );

  if (verifiedOnly) {
    eligibleUsers = eligibleUsers.filter(u => u.email_verified);
  }

  const verified = eligibleUsers.filter(u => u.email_verified).length;
  const unverified = eligibleUsers.length - verified;
  
  // Get already sent if resuming
  const alreadySent = resume ? getSentEmails() : new Set();
  const toSend = eligibleUsers.filter(u => !alreadySent.has(u.email));

  console.log(`\n📊 Stats:`);
  console.log(`   Total eligible: ${eligibleUsers.length} (${verified} verified, ${unverified} unverified)`);
  console.log(`   Muted/excluded: ${mutedUsers.size}`);
  console.log(`   Unsubscribed: ${unsubscribedEmails.size}`);
  if (resume) {
    console.log(`   Already sent: ${alreadySent.size}`);
  }
  console.log(`   To send now: ${toSend.length}`);

  if (toSend.length === 0) {
    console.log('\n✅ All emails already sent!');
    return;
  }

  // Estimate time
  const estimatedMinutes = Math.ceil(toSend.length * 1.2 / 60); // 1.2s per email avg
  const estimatedDays = Math.ceil(toSend.length / 450);
  console.log(`   Estimated time: ~${estimatedMinutes} minutes per day`);
  console.log(`   Estimated days: ~${estimatedDays} (Gmail ~500/day limit)`);

  console.log(`\n⚠️  WARNING: This will send ${toSend.length} emails!`);
  console.log(`   Use --resume to continue across multiple days.`);
  
  const confirmed = await confirm('\nType "yes" to confirm: ');
  
  if (!confirmed) {
    console.log('❌ Aborted.');
    return;
  }

  console.log('\n📤 Starting email blast...\n');
  console.log(`   Progress saved to: ${SENT_LOG_FILE}`);
  console.log(`   Use --send --resume to continue if interrupted\n`);

  // Record blast in database
  let blastId;
  try {
    blastId = await createBlastRecord(EMAIL_SUBJECT, toSend.length);
    console.log(`   Blast ID: ${blastId}\n`);
  } catch (err) {
    console.warn(`   ⚠️  Could not record blast in DB: ${err.message}\n`);
  }

  const transporter = createTransport(getSmtpConfig());
  
  let sent = 0;
  let failed = 0;
  const startTime = Date.now();

  for (let i = 0; i < toSend.length; i++) {
    const user = toSend[i];
    const email = user.email;

    try {
      await sendEmail(transporter, email);
      sent++;
      logSentEmail(email);
      
      const elapsed = ((Date.now() - startTime) / 1000 / 60).toFixed(1);
      const rate = (sent / (elapsed || 0.1)).toFixed(1);
      console.log(`✅ [${sent + failed}/${toSend.length}] ${email} (${elapsed}m, ${rate}/min)`);
    } catch (error) {
      failed++;
      logFailedEmail(email, error.message);
      console.log(`❌ [${sent + failed}/${toSend.length}] ${email} - ${error.message}`);
      
      // If too many failures, pause longer
      if (failed > 10 && failed / (sent + failed) > 0.2) {
        console.log(`\n⚠️  High failure rate! Pausing 30s...`);
        await new Promise(r => setTimeout(r, 30000));
      }
    }

    // Rate limit: 1 email per second
    if (i < toSend.length - 1) {
      await new Promise(r => setTimeout(r, 1000));
    }

    // Longer pause every 50 emails (Gmail likes this)
    if ((i + 1) % 50 === 0) {
      console.log(`\n⏸️  Batch ${Math.floor((i + 1) / 50)} complete. Pausing 10s...\n`);
      await new Promise(r => setTimeout(r, 10000));
    }
    
    // Even longer pause every 400 emails (approaching daily limit)
    if ((i + 1) % 400 === 0) {
      console.log(`\n🛑 Approaching Gmail daily limit. Pausing 5 minutes...`);
      console.log(`   (Safe to Ctrl+C and resume later with --resume)\n`);
      await new Promise(r => setTimeout(r, 5 * 60 * 1000));
    }
  }

  const totalTime = ((Date.now() - startTime) / 1000 / 60).toFixed(1);

  // Update blast record in database
  if (blastId) {
    try {
      await updateBlastRecord(blastId, {
        sent,
        failed,
        status: 'completed',
        completedAt: new Date(),
        duration: `${totalTime} minutes`,
      });
    } catch (err) {
      console.warn(`⚠️  Could not update blast record: ${err.message}`);
    }
  }

  console.log('\n' + '═'.repeat(60));
  console.log(`📊 BLAST COMPLETE`);
  console.log(`   Sent: ${sent}`);
  console.log(`   Failed: ${failed}`);
  console.log(`   Time: ${totalTime} minutes`);
  console.log(`   Total sent (all time): ${getSentEmails().size}`);
  console.log('═'.repeat(60));
}

// Main
const args = process.argv.slice(2);
const hasResume = args.includes('--resume') || args.includes('-r');

if (args.includes('--fetch') || args.includes('-f')) {
  // Fetch all users from Auth0, show them flying by, save locally
  await getAllAuth0Users(hasResume, true);
} else if (args.includes('--fetch-all') || args.includes('--bulk')) {
  // Use Auth0 export job for 16k+ users
  await exportAllUsers();
} else if (args.includes('--list') || args.includes('-l')) {
  // Show cached users (or fetch if none)
  const users = loadCachedUsers();
  if (users.length === 0 || hasResume) {
    await listUsers(hasResume);
  } else {
    await showCachedUsers();
  }
} else if (args.includes('--export') || args.includes('-e')) {
  await exportUsers();
} else if (args.includes('--preview') || args.includes('-p')) {
  previewEmail();
} else if (args.includes('--test') || args.includes('-t')) {
  const testIdx = args.indexOf('--test') !== -1 ? args.indexOf('--test') : args.indexOf('-t');
  const testEmail = args[testIdx + 1];
  if (!testEmail) {
    console.log('Usage: node email-blast.mjs --test EMAIL');
    process.exit(1);
  }
  await sendTestEmail(testEmail);
} else if (args.includes('--send') || args.includes('-s')) {
  const includeUnverified = args.includes('--all') || args.includes('--include-unverified');
  await sendBlast(hasResume, !includeUnverified);
} else if (args.includes('--clear')) {
  clearFetchCheckpoint();
  console.log('✅ Cleared all cached data and checkpoints');
} else {
  console.log(`
📧 Email Blast Tool — give.aesthetic.computer

Usage:
  node artery/email-blast.mjs --fetch          Fetch users (page-based, max 1000)
  node artery/email-blast.mjs --fetch-all      Bulk export ALL users (16k+)
  node artery/email-blast.mjs --list           Show cached users
  node artery/email-blast.mjs --export         Export to CSV
  node artery/email-blast.mjs --preview        Preview email content
  node artery/email-blast.mjs --test EMAIL     Send test email
  node artery/email-blast.mjs --send           Send to VERIFIED users only (default)
  node artery/email-blast.mjs --send --all     Send to ALL users (verified + unverified)
  node artery/email-blast.mjs --send --resume  Resume interrupted send
  node artery/email-blast.mjs --clear          Clear all cached data

Files:
  vault/user-reports/email-blast-users.json  - Cached user list
  scratch/email-blast-sent.log               - Emails successfully sent
  scratch/email-blast-failed.log             - Failed emails

Notes:
  - Muted + unsubscribed users automatically excluded
  - Use --fetch-all for 16k+ users (Auth0 bulk export)
  - Gmail limit: ~500 emails/day (~11 days for verified, ~36 days for all)
  - Use --resume to continue across multiple days
  - Each email includes a personalized unsubscribe link
`);
}
