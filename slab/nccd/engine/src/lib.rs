// nccd-engine — NCCd's Signal engine.
//
// The app is a Signal linked device, as Signal Desktop is. This library is
// the part that speaks to Signal: it wraps presage, which is built on
// libsignal, Signal's own library. It runs on one thread of its own. The app
// hands it requests as JSON lines and gets replies and events back the same
// way, in the shape signal-cli's jsonRpc uses.
//
// Four rules hold here, and each has a test:
//
//   1. The store is encrypted. Its key comes from the app (the Keychain) and
//      is never written to a log.
//   2. The engine keeps keys and groups, and no messages. What Signal's
//      servers hand it goes to the app or is dropped, and is then deleted
//      from the store.
//   3. Until the app says which groups are the club's, nothing leaves the
//      engine. After that, only those groups do.
//   4. A file's name on disk comes from the engine, never from the sender.
//   5. When a member's identity key is not the one seen before, the app is
//      told. (Signal Desktop shows this as "safety number changed".)

pub mod wire;

use std::collections::HashSet;
use std::ffi::{c_char, c_void, CStr, CString};
use std::os::unix::fs::{OpenOptionsExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use futures::channel::oneshot;
use futures::{future, pin_mut, StreamExt};
use presage::libsignal_service::configuration::SignalServers;
use presage::libsignal_service::content::{Content, ContentBody, DataMessage, GroupContextV2};
use presage::libsignal_service::proto::sync_message::{Content as SyncContent, Sent};
use presage::libsignal_service::proto::AttachmentPointer;
use presage::libsignal_service::protocol::{IdentityKeyStore, ProtocolAddress};
use presage::manager::Registered;
use presage::model::identity::OnNewIdentity;
use presage::model::messages::Received;
use presage::proto::SyncMessage;
use presage::store::{ContentsStore, Store};
use presage::Manager;
use presage_store_sqlite::{SqliteConnectOptions, SqliteStore};
use serde_json::{json, Value};
use sqlx::ConnectOptions;
use tokio::sync::mpsc;

use wire::{Config, Request};

type Engine = Manager<SqliteStore, Registered>;

// ─── the C face ──────────────────────────────────────────────────────────────

pub type NccdEngineCallback = extern "C" fn(ctx: *mut c_void, json: *const c_char);

pub struct NccdEngine {
    tx: Option<mpsc::UnboundedSender<String>>,
    thread: Option<std::thread::JoinHandle<()>>,
}

// Where the engine's words go. The app's pointer is only ever handed back to the app.
#[derive(Clone, Copy)]
struct Out {
    callback: NccdEngineCallback,
    ctx: usize,
}

impl Out {
    fn emit(&self, v: Value) {
        if let Ok(line) = CString::new(v.to_string()) {
            (self.callback)(self.ctx as *mut c_void, line.as_ptr());
        }
    }
    fn event(&self, method: &str, params: Value) {
        self.emit(json!({ "jsonrpc": "2.0", "method": method, "params": params }));
    }
    fn reply(&self, id: &Value, result: Result<Value, String>) {
        match result {
            Ok(v) => self.emit(json!({ "jsonrpc": "2.0", "id": id, "result": v })),
            Err(message) => self.emit(json!({ "jsonrpc": "2.0", "id": id, "error": { "message": message } })),
        }
    }
}

unsafe fn text(p: *const c_char) -> Option<String> {
    if p.is_null() {
        return None;
    }
    CStr::from_ptr(p).to_str().ok().map(str::to_string)
}

/// # Safety
/// `config` is a C string, or null. `callback` stays valid until `nccd_engine_stop` returns.
#[no_mangle]
pub unsafe extern "C" fn nccd_engine_start(
    config: *const c_char,
    callback: NccdEngineCallback,
    ctx: *mut c_void,
) -> *mut NccdEngine {
    let Some(conf) = text(config).and_then(|t| wire::parse_config(&t).ok()) else {
        return std::ptr::null_mut();
    };
    let out = Out { callback, ctx: ctx as usize };
    let (tx, rx) = mpsc::unbounded_channel();
    let thread = std::thread::Builder::new().name("nccd-engine".into()).spawn(move || {
        // A fault in here ends the engine, with a word to the app. It never crosses into the app.
        let end = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let Ok(rt) = tokio::runtime::Builder::new_current_thread().enable_all().build() else {
                return "the engine could not start".to_string();
            };
            tokio::task::LocalSet::new().block_on(&rt, run(conf, out, rx))
        }))
        .unwrap_or_else(|_| "the engine stopped on a fault".to_string());
        out.event("exit", json!({ "reason": end }));
    });
    match thread {
        Ok(thread) => Box::into_raw(Box::new(NccdEngine { tx: Some(tx), thread: Some(thread) })),
        Err(_) => std::ptr::null_mut(),
    }
}

/// # Safety
/// `engine` came from `nccd_engine_start` and was not stopped. `json` is a C string, or null.
#[no_mangle]
pub unsafe extern "C" fn nccd_engine_request(engine: *mut NccdEngine, json: *const c_char) -> i32 {
    let (Some(engine), Some(line)) = (engine.as_ref(), text(json)) else { return -1 };
    match &engine.tx {
        Some(tx) if line.len() <= wire::MAX_REQUEST && tx.send(line).is_ok() => 0,
        _ => -1,
    }
}

/// # Safety
/// `engine` came from `nccd_engine_start`, or is null. It must not be used after this.
#[no_mangle]
pub unsafe extern "C" fn nccd_engine_stop(engine: *mut NccdEngine) {
    if engine.is_null() {
        return;
    }
    let mut engine = Box::from_raw(engine);
    engine.tx = None; // the engine's thread sees its requests end, and ends
    if let Some(thread) = engine.thread.take() {
        let _ = thread.join();
    }
}

// ─── the store ───────────────────────────────────────────────────────────────

// The store is SQLCipher. Statement logging is off: the statement that opens
// the store holds the key, and a logged statement is a key in a log.
pub async fn open_store(db: &str, key: &str) -> Result<SqliteStore, String> {
    if !wire::valid_key(key) {
        return Err("the key is not valid".into());
    }
    if let Some(dir) = Path::new(db).parent() {
        std::fs::create_dir_all(dir).map_err(|e| e.to_string())?;
        let _ = std::fs::set_permissions(dir, std::fs::Permissions::from_mode(0o700));
    }
    let options = SqliteConnectOptions::new()
        .filename(db)
        .create_if_missing(true)
        .foreign_keys(true)
        .pragma("key", format!("'{key}'"))
        .disable_statement_logging();
    let store = SqliteStore::open_with_options(options, OnNewIdentity::Trust)
        .await
        .map_err(|_| "the store did not open".to_string())?;
    for suffix in ["", "-wal", "-shm"] {
        let _ = std::fs::set_permissions(format!("{db}{suffix}"), std::fs::Permissions::from_mode(0o600));
    }
    Ok(store)
}

// ─── the engine ──────────────────────────────────────────────────────────────

struct State {
    out: Out,
    attachments: PathBuf,
    own: String,                        // this account's id
    allowed: Option<HashSet<String>>,   // the club's groups; None until the app has said
    held: Vec<(String, DataMessage, bool, String)>,   // what came before it said
    tally: Tally,
    key: String,                        // the store's key: it signs the marks below
    marks: PathBuf,                     // where the identity marks are kept
    identities: wire::Identities,
}

// How many things came, and what was done with each. Numbers only: the app can
// show that the door works without the engine saying what was at it.
#[derive(Default)]
struct Tally {
    delivered: u64,   // the club's: given to the app
    held: u64,        // came before the app named the club's groups
    dropped: u64,     // a message that is not the club's
    other: u64,       // not a message: a receipt, a typing mark, a sync
    unreadable: u64,  // Signal could not decrypt it
}

impl State {
    fn count(&self) {
        let t = &self.tally;
        self.out.event("tally", json!({ "delivered": t.delivered, "held": t.held, "dropped": t.dropped, "other": t.other, "unreadable": t.unreadable }));
    }
}

const HELD_MAX: usize = 2000;

fn now() -> u64 {
    SystemTime::now().duration_since(UNIX_EPOCH).map(|d| d.as_millis() as u64).unwrap_or(0)
}

async fn run(conf: Config, out: Out, mut rx: mpsc::UnboundedReceiver<String>) -> String {
    let store = match open_store(&conf.db, &conf.key).await {
        Ok(store) => store,
        Err(why) => return why,
    };
    let _ = std::fs::create_dir_all(&conf.attachments);
    let _ = std::fs::set_permissions(&conf.attachments, std::fs::Permissions::from_mode(0o700));

    let mut manager: Engine = match conf.link {
        Some(name) => {
            let (uri_tx, uri_rx) = oneshot::channel();
            let link = future::join(
                Manager::link_secondary_device(store, SignalServers::Production, name, uri_tx),
                async {
                    if let Ok(uri) = uri_rx.await {
                        out.event("linkUri", json!({ "uri": uri.to_string() }));
                    }
                },
            );
            pin_mut!(link);
            loop {
                tokio::select! {
                    (linked, _) = &mut link => match linked {
                        Ok(manager) => break manager,
                        Err(_) => return "linking did not finish".into(),
                    },
                    line = rx.recv() => match line {
                        None => return "stopped".into(),
                        Some(line) => if let Ok(r) = wire::parse_request(&line) {
                            out.reply(&r.id, Err("Signal is not linked yet".into()));
                        },
                    },
                }
            }
        }
        None => match Manager::load_registered(store).await {
            Ok(manager) => manager,
            Err(_) => return "not linked".into(),
        },
    };

    let data = manager.registration_data();
    let own = data.service_ids.aci.to_string();
    out.event("account", json!({ "uuid": own, "number": data.phone_number.to_string() }));

    let marks = PathBuf::from(format!("{}.identities", conf.db));
    let (identities, whole) = wire::Identities::load(std::fs::read_to_string(&marks).ok().as_deref(), &conf.key);
    if !whole {
        // The file of marks was changed or cut. It is not believed, and the app is told.
        out.event("identitiesReset", json!({}));
    }
    let mut state = State {
        out, attachments: PathBuf::from(conf.attachments), own, allowed: None, held: Vec::new(), tally: Tally::default(),
        key: conf.key.clone(), marks, identities,
    };
    forget(&manager).await;

    loop {
        match manager.receive_messages().await {
            Ok(messages) => {
                pin_mut!(messages);
                loop {
                    tokio::select! {
                        item = messages.next() => match item {
                            None => break,
                            Some(Received::Content(content)) => on_content(&mut manager, &mut state, &content).await,
                            Some(Received::QueueEmpty) => out.event("caughtUp", json!({})),
                            Some(Received::Contacts { .. }) => out.event("contacts", json!({})),
                            Some(_) => { state.tally.unreadable += 1; state.count(); }
                        },
                        line = rx.recv() => match line {
                            None => return "stopped".into(),
                            Some(line) => on_request(&mut manager, &mut state, &line).await,
                        },
                    }
                }
                out.event("offline", json!({}));
            }
            Err(_) => out.event("offline", json!({})),
        }
        // Signal's line dropped. Wait, still answering the app, then take it up again.
        let wait = tokio::time::sleep(Duration::from_secs(8));
        pin_mut!(wait);
        loop {
            tokio::select! {
                _ = &mut wait => break,
                line = rx.recv() => match line {
                    None => return "stopped".into(),
                    Some(line) => on_request(&mut manager, &mut state, &line).await,
                },
            }
        }
    }
}

// Rule 2: the engine keeps no messages. presage saves each one it receives; this takes them out.
async fn forget(manager: &Engine) {
    let mut store = manager.store().clone();
    let _ = store.clear_messages().await;
}

// Rule 5: the sender's identity key, against the one seen before.
async fn check_identity(manager: &Engine, state: &mut State, content: &Content) {
    let who = content.metadata.sender.service_id_string();
    if who == state.own {
        return;
    }
    let address = ProtocolAddress::new(who.clone(), content.metadata.sender_device);
    let Ok(Some(identity)) = manager.store().aci_protocol_store().get_identity(&address).await else { return };
    let seen = state.identities.note(&who, &wire::identity_mark(&identity.serialize()));
    if seen == wire::Seen::Same {
        return;
    }
    let file = state.identities.dump(&state.key);
    let temp = state.marks.with_extension("identities.new");
    let _ = std::fs::remove_file(&temp);
    write_private(&temp, file.as_bytes());
    let _ = std::fs::rename(&temp, &state.marks);
    if seen == wire::Seen::Changed {
        state.out.event("identityChanged", json!({ "uuid": who }));
    }
}

async fn on_content(manager: &mut Engine, state: &mut State, content: &Content) {
    forget(manager).await;
    check_identity(manager, state, content).await;
    let (author, dm, sent) = match &content.body {
        ContentBody::DataMessage(dm) => (content.metadata.sender.raw_uuid().to_string(), dm.clone(), false),
        ContentBody::SynchronizeMessage(SyncMessage {
            content: Some(SyncContent::Sent(Sent { message: Some(dm), .. })), ..
        }) => (state.own.clone(), dm.clone(), true),
        _ => {
            state.tally.other += 1;
            return state.count();
        }
    };
    // Only a group message can be the club's.
    let Some(gid) = dm.group_v2.as_ref().and_then(|g| g.master_key.as_deref()).and_then(wire::group_id) else {
        state.tally.dropped += 1;
        return state.count();
    };
    match &state.allowed {
        // Rule 3: the app has not said yet. Hold it in memory, and no more than a bounded number.
        None => {
            if state.held.len() < HELD_MAX {
                state.held.push((author, dm, sent, gid));
                state.tally.held += 1;
            } else {
                state.tally.dropped += 1;
            }
        }
        Some(allowed) if allowed.contains(&gid) => {
            deliver(manager, state, author, dm, sent, gid).await;
            state.tally.delivered += 1;
        }
        Some(_) => state.tally.dropped += 1,
    }
    state.count();
}

async fn deliver(manager: &mut Engine, state: &State, author: String, dm: DataMessage, sent: bool, gid: String) {
    let mut message = wire::data_message_json(&dm, &gid);
    let mut files = Vec::new();
    for pointer in dm.attachments.iter().take(32) {
        if let Some(file) = fetch(manager, state, pointer).await {
            files.push(file);
        }
    }
    if !files.is_empty() {
        message["attachments"] = Value::Array(files);
    }
    let mut previews = Vec::new();
    for p in dm.preview.iter().take(8) {
        let Some(url) = p.url.clone() else { continue };
        let mut preview = json!({ "url": url, "title": p.title.clone().unwrap_or_default(), "description": p.description.clone().unwrap_or_default() });
        if let Some(pointer) = &p.image {
            if let Some(file) = fetch(manager, state, pointer).await {
                preview["image"] = file;
            }
        }
        previews.push(preview);
    }
    if !previews.is_empty() {
        message["previews"] = Value::Array(previews);
    }
    let mut envelope = json!({ "sourceUuid": author, "timestamp": dm.timestamp.unwrap_or(0) });
    if sent {
        envelope["syncMessage"] = json!({ "sentMessage": message });
    } else {
        envelope["dataMessage"] = message;
    }
    state.out.event("receive", json!({ "envelope": envelope }));
}

// Rule 4: the file is named by the engine, from the attachment's digest.
async fn fetch(manager: &Engine, state: &State, pointer: &AttachmentPointer) -> Option<Value> {
    let kind = pointer.content_type.clone().unwrap_or_default();
    let id = wire::attachment_id(pointer.digest.as_deref().unwrap_or_default(), &kind)?;
    let size = pointer.size.unwrap_or(0) as u64;
    let path = state.attachments.join(&id);
    if !path.exists() && size <= wire::MAX_ATTACHMENT {
        if let Ok(bytes) = manager.get_attachment(pointer).await {
            if bytes.len() as u64 <= wire::MAX_ATTACHMENT {
                write_private(&path, &bytes);
            }
        }
    }
    Some(json!({ "id": id, "filename": pointer.file_name.clone().unwrap_or_default(), "contentType": kind, "size": size }))
}

// A new file that only the owner reads. It is never written through a link.
fn write_private(path: &Path, bytes: &[u8]) {
    use std::io::Write;
    let made = std::fs::OpenOptions::new().write(true).create_new(true).mode(0o600).custom_flags(libc_nofollow()).open(path);
    if let Ok(mut file) = made {
        if file.write_all(bytes).is_err() {
            let _ = std::fs::remove_file(path);
        }
    }
}

const fn libc_nofollow() -> i32 {
    0x0100 // O_NOFOLLOW on macOS
}

async fn on_request(manager: &mut Engine, state: &mut State, line: &str) {
    let r = match wire::parse_request(line) {
        Ok(r) => r,
        Err(why) => return state.out.reply(&Value::Null, Err(why)),
    };
    let result = answer(manager, state, &r).await;
    state.out.reply(&r.id, result);
    forget(manager).await;
}

async fn answer(manager: &mut Engine, state: &mut State, r: &Request) -> Result<Value, String> {
    match r.method.as_str() {
        "version" => Ok(json!({ "engine": env!("CARGO_PKG_VERSION") })),

        // Rule 3: the app names the club's groups. What was held is given out, or dropped.
        "setGroups" => {
            let ids: HashSet<String> = r.params.get("ids").and_then(Value::as_array).ok_or("setGroups needs ids")?
                .iter().filter_map(Value::as_str).map(str::to_string).collect();
            state.allowed = Some(ids.clone());
            for (author, dm, sent, gid) in std::mem::take(&mut state.held) {
                state.tally.held -= 1;
                if ids.contains(&gid) {
                    deliver(manager, state, author, dm, sent, gid).await;
                    state.tally.delivered += 1;
                } else {
                    state.tally.dropped += 1;
                }
            }
            state.count();
            Ok(json!({ "groups": ids.len() }))
        }

        "listGroups" => {
            let mut out = Vec::new();
            for (key, group) in manager.store().groups().await.map_err(|_| "the groups could not be read")?.flatten() {
                let Some(id) = wire::group_id(&key) else { continue };
                out.push(json!({
                    "id": id,
                    "name": group.title,
                    "description": group.description.unwrap_or_default(),
                    "isMember": true,
                    "messageExpirationTime": group.disappearing_messages_timer.map(|t| t.duration).unwrap_or(0),
                    "members": group.members.iter().map(|m| json!({ "uuid": m.aci.service_id_string() })).collect::<Vec<_>>(),
                    "admins": group.members.iter().filter(|m| matches!(m.role, presage::libsignal_service::groups_v2::Role::Administrator))
                        .map(|m| json!({ "uuid": m.aci.service_id_string() })).collect::<Vec<_>>(),
                }));
            }
            Ok(Value::Array(out))
        }

        "listContacts" => {
            let mut out = Vec::new();
            for c in manager.store().contacts().await.map_err(|_| "the contacts could not be read")?.flatten() {
                out.push(json!({ "uuid": c.uuid.to_string(), "name": c.name }));
            }
            Ok(Value::Array(out))
        }

        "send" => {
            let gid = r.params.get("groupId").and_then(Value::as_str).ok_or("send needs a groupId")?;
            let body = r.params.get("message").and_then(Value::as_str).ok_or("send needs a message")?;
            if body.is_empty() || body.len() > 64 * 1024 {
                return Err("the message is empty or too long".into());
            }
            // The app may write only to the club's groups.
            if !state.allowed.as_ref().is_some_and(|a| a.contains(gid)) {
                return Err("that is not one of the club's groups".into());
            }
            let empty = Vec::new();
            let ranges = wire::parse_mentions(r.params.get("mention").and_then(Value::as_array).unwrap_or(&empty), body)?;
            let mut found = None;
            for (key, group) in manager.store().groups().await.map_err(|_| "the groups could not be read")?.flatten() {
                if wire::group_id(&key).as_deref() == Some(gid) {
                    found = Some((key, group));
                    break;
                }
            }
            let (key, group) = found.ok_or("Signal does not know that group")?;
            let ts = now();
            let message = DataMessage {
                body: Some(body.to_string()),
                timestamp: Some(ts),
                body_ranges: ranges,
                // The group's timer goes on the message, so every member's copy disappears on time.
                expire_timer: group.disappearing_messages_timer.map(|t| t.duration),
                group_v2: Some(GroupContextV2 { master_key: Some(key.to_vec()), revision: Some(group.revision), ..Default::default() }),
                ..Default::default()
            };
            manager.send_message_to_group(&key, message, ts).await.map_err(|_| "Signal did not take the message")?;
            Ok(json!({ "timestamp": ts }))
        }

        // Ask the phone for the account's contacts (their names). They come as a sync.
        "requestContacts" => {
            manager.request_contacts().await.map_err(|_| "Signal did not take the request")?;
            Ok(json!({}))
        }

        // The device is taken off the account, and its keys go from this Mac.
        "unlink" => {
            manager.unlink_self().await.map_err(|_| "Signal did not unlink the device")?;
            let mut store = manager.store().clone();
            let _ = store.clear().await;
            Ok(json!({}))
        }

        _ => Err("the engine has no such method".into()),
    }
}
