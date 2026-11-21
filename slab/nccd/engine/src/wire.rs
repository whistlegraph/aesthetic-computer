// wire.rs — the words between the app and the engine.
//
// Everything here is plain: it takes text or bytes and gives text or bytes
// back, with no network and no store. So every rule that guards the engine's
// door (what a key looks like, what a file may be called, what a request may
// hold) has a test at the foot of this file.

use base64::prelude::*;
use presage::libsignal_service::content::DataMessage;
use presage::libsignal_service::zkgroup::groups::{GroupMasterKey, GroupSecretParams};
use presage::proto::body_range::AssociatedValue;
use presage::proto::BodyRange;
use serde_json::{json, Value};
use sha2::{Digest, Sha256};

// The largest picture or file the engine takes from Signal's servers.
pub const MAX_ATTACHMENT: u64 = 100 * 1024 * 1024;
// The longest line the app may hand in.
pub const MAX_REQUEST: usize = 256 * 1024;

pub struct Config {
    pub db: String,
    pub key: String,
    pub attachments: String,
    pub link: Option<String>,
}

// The key is 64 hex characters and nothing else. It goes into a statement
// that opens the store, so a quote mark in it must not be possible.
pub fn valid_key(key: &str) -> bool {
    key.len() == 64 && key.bytes().all(|b| b.is_ascii_hexdigit())
}

pub fn parse_config(text: &str) -> Result<Config, String> {
    let v: Value = serde_json::from_str(text).map_err(|_| "the config is not JSON".to_string())?;
    let field = |name: &str| v.get(name).and_then(Value::as_str).map(str::to_string);
    let db = field("db").filter(|s| s.starts_with('/')).ok_or("db must be a full path")?;
    let attachments = field("attachments").filter(|s| s.starts_with('/')).ok_or("attachments must be a full path")?;
    let key = field("key").filter(|k| valid_key(k)).ok_or("key must be 64 hex characters")?;
    let link = match field("link") {
        Some(name) if name.is_empty() || name.chars().count() > 50 || name.chars().any(char::is_control) => {
            return Err("the device name is not valid".into())
        }
        other => other,
    };
    Ok(Config { db, key, attachments, link })
}

pub struct Request {
    pub id: Value,
    pub method: String,
    pub params: Value,
}

pub fn parse_request(line: &str) -> Result<Request, String> {
    if line.len() > MAX_REQUEST {
        return Err("the request is too long".into());
    }
    let v: Value = serde_json::from_str(line).map_err(|_| "the request is not JSON".to_string())?;
    let method = v.get("method").and_then(Value::as_str).ok_or("the request has no method")?.to_string();
    Ok(Request {
        id: v.get("id").cloned().unwrap_or(Value::Null),
        method,
        params: v.get("params").cloned().unwrap_or_else(|| json!({})),
    })
}

// A group's id as signal-cli writes it: the group identifier, in base64.
// The app's config and its logs already name the club's groups this way.
pub fn group_id(master_key: &[u8]) -> Option<String> {
    let bytes: [u8; 32] = master_key.try_into().ok()?;
    let params = GroupSecretParams::derive_from_master_key(GroupMasterKey::new(bytes));
    Some(BASE64_STANDARD.encode(params.get_group_identifier()))
}

// The file extension for a kind of content. Only kinds the app shows get a
// name of their own; every other kind is "bin". What the sender called the
// file never reaches the disk.
pub fn extension(content_type: &str) -> &'static str {
    match content_type.split(';').next().unwrap_or("").trim().to_ascii_lowercase().as_str() {
        "image/jpeg" | "image/jpg" => "jpg",
        "image/png" => "png",
        "image/gif" => "gif",
        "image/webp" => "webp",
        "image/heic" => "heic",
        "video/mp4" => "mp4",
        "video/quicktime" => "mov",
        "video/webm" => "webm",
        "audio/mpeg" => "mp3",
        "audio/aac" | "audio/mp4" => "m4a",
        "application/pdf" => "pdf",
        "text/plain" => "txt",
        _ => "bin",
    }
}

// The name of an attachment's file: 24 hex characters from its digest, and an
// extension from the list above. No part of it comes from the sender as text.
pub fn attachment_id(digest: &[u8], content_type: &str) -> Option<String> {
    if digest.is_empty() {
        return None;
    }
    let name = hex::encode(&Sha256::digest(digest)[..12]);
    Some(format!("{name}.{}", extension(content_type)))
}

// What the engine itself names is the only thing it writes or removes.
pub fn is_attachment_id(id: &str) -> bool {
    let Some((name, ext)) = id.split_once('.') else { return false };
    name.len() == 24
        && name.bytes().all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
        && !ext.is_empty()
        && ext.len() <= 4
        && ext.bytes().all(|b| b.is_ascii_lowercase() || b.is_ascii_digit())
}

fn uuid_text(bytes: &[u8]) -> Option<String> {
    let b: [u8; 16] = bytes.try_into().ok()?;
    let h = hex::encode(b);
    Some(format!("{}-{}-{}-{}-{}", &h[0..8], &h[8..12], &h[12..16], &h[16..20], &h[20..32]))
}

fn uuid_bytes(text: &str) -> Option<Vec<u8>> {
    let parts: Vec<&str> = text.split('-').collect();
    if parts.len() != 5 || parts.iter().map(|p| p.len()).ne([8usize, 4, 4, 4, 12]) {
        return None;
    }
    hex::decode(parts.concat()).ok()
}

// "start:length:uuid", as signal-cli takes a mention. Counted in UTF-16 units.
pub fn parse_mentions(list: &[Value], body: &str) -> Result<Vec<BodyRange>, String> {
    let units = body.encode_utf16().count() as u64;
    let mut out = Vec::new();
    for item in list.iter().take(64) {
        let text = item.as_str().ok_or("a mention is not text")?;
        let mut parts = text.splitn(3, ':');
        let start: u32 = parts.next().and_then(|s| s.parse().ok()).ok_or("a mention has no start")?;
        let length: u32 = parts.next().and_then(|s| s.parse().ok()).ok_or("a mention has no length")?;
        let who = parts.next().and_then(uuid_bytes).ok_or("a mention has no member")?;
        if length == 0 || start as u64 + length as u64 > units {
            return Err("a mention is outside the message".into());
        }
        out.push(BodyRange {
            start: Some(start),
            length: Some(length),
            associated_value: Some(AssociatedValue::MentionAciBinary(who)),
        });
    }
    Ok(out)
}

// A data message, in the fields signal-cli gives one. Attachments and preview
// pictures are added by the engine, which fetches them.
pub fn data_message_json(dm: &DataMessage, gid: &str) -> Value {
    let mut out = json!({
        "timestamp": dm.timestamp.unwrap_or(0),
        "message": dm.body.clone().unwrap_or_default(),
        "expiresInSeconds": dm.expire_timer.unwrap_or(0),
        "groupInfo": { "groupId": gid },
    });
    let mentions: Vec<Value> = dm
        .body_ranges
        .iter()
        .filter_map(|r| {
            let uuid = match r.associated_value.as_ref()? {
                AssociatedValue::MentionAci(text) => text.to_ascii_lowercase(),
                AssociatedValue::MentionAciBinary(bytes) => uuid_text(bytes)?,
                _ => return None,
            };
            Some(json!({ "start": r.start.unwrap_or(0), "length": r.length.unwrap_or(0), "uuid": uuid }))
        })
        .collect();
    if !mentions.is_empty() {
        out["mentions"] = Value::Array(mentions);
    }
    if let Some(text) = dm.quote.as_ref().and_then(|q| q.text.clone()) {
        out["quote"] = json!({ "text": text });
    }
    if let Some(r) = &dm.reaction {
        out["reaction"] = json!({
            "emoji": r.emoji.clone().unwrap_or_default(),
            "targetSentTimestamp": r.target_sent_timestamp.unwrap_or(0),
            "isRemove": r.remove.unwrap_or(false),
        });
    }
    if let Some(ts) = dm.delete.as_ref().and_then(|d| d.target_sent_timestamp) {
        out["remoteDelete"] = json!({ "timestamp": ts });
    }
    out
}

// ─── safety numbers ─────────────────────────────────────────────────────────
//
// Each member has an identity key. It changes when they install Signal again,
// and it would also change if someone stood between you and them. The engine
// keeps a mark of each key it has seen, and says when one is not the same.
//
// The marks are hashes of public keys, so they are not secret. They must not
// be changed without notice, so the file carries a keyed hash (HMAC-SHA256)
// made with the store's key. A file that does not match is not believed.

#[derive(Debug, PartialEq)]
pub enum Seen {
    First,
    Same,
    Changed,
}

#[derive(Default)]
pub struct Identities {
    marks: std::collections::BTreeMap<String, String>,
}

pub fn hmac_sha256(key: &[u8], message: &[u8]) -> [u8; 32] {
    let mut block = [0u8; 64];
    if key.len() > 64 {
        block[..32].copy_from_slice(&Sha256::digest(key));
    } else {
        block[..key.len()].copy_from_slice(key);
    }
    let inner = Sha256::new().chain_update(block.map(|b| b ^ 0x36)).chain_update(message).finalize();
    Sha256::new().chain_update(block.map(|b| b ^ 0x5c)).chain_update(inner).finalize().into()
}

// The mark of an identity key: the first half of its SHA-256, in hex.
pub fn identity_mark(public_key: &[u8]) -> String {
    hex::encode(&Sha256::digest(public_key)[..16])
}

impl Identities {
    fn body(&self) -> String {
        serde_json::to_string(&self.marks).unwrap_or_default()
    }

    fn mac(body: &str, key: &str) -> String {
        hex::encode(hmac_sha256(format!("nccd identities {key}").as_bytes(), body.as_bytes()))
    }

    pub fn dump(&self, key: &str) -> String {
        let body = self.body();
        json!({ "mac": Self::mac(&body, key), "seen": body }).to_string()
    }

    // The marks, and whether the file was whole. No file at all is whole (a first run).
    pub fn load(text: Option<&str>, key: &str) -> (Self, bool) {
        let Some(text) = text else { return (Self::default(), true) };
        let whole = (|| {
            let v: Value = serde_json::from_str(text).ok()?;
            let body = v.get("seen")?.as_str()?;
            let mac = hex::decode(v.get("mac")?.as_str()?).ok()?;
            let want = hmac_sha256(format!("nccd identities {key}").as_bytes(), body.as_bytes());
            // every byte is compared, whatever the first difference is
            if mac.len() != 32 || mac.iter().zip(want).fold(0u8, |d, (a, b)| d | (a ^ b)) != 0 {
                return None;
            }
            serde_json::from_str(body).ok()
        })();
        match whole {
            Some(marks) => (Self { marks }, true),
            None => (Self::default(), false),
        }
    }

    pub fn note(&mut self, who: &str, mark: &str) -> Seen {
        match self.marks.insert(who.to_string(), mark.to_string()) {
            None => Seen::First,
            Some(old) if old == mark => Seen::Same,
            Some(_) => Seen::Changed,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const KEY: &str = "00112233445566778899aabbccddeeff00112233445566778899aabbccddeeff";

    #[test]
    fn a_key_is_64_hex_characters_and_nothing_else() {
        assert!(valid_key(KEY));
        assert!(!valid_key(""));
        assert!(!valid_key(&KEY[..63]));
        assert!(!valid_key(&format!("{}'", &KEY[..63])));
        assert!(!valid_key(&format!("{}; --", &KEY[..59])));
        assert!(!valid_key(&KEY.replace('0', "g")));
        assert!(!valid_key(&format!("{KEY}00")));
    }

    #[test]
    fn a_config_needs_full_paths_and_a_good_key() {
        let good = json!({"db": "/a/store.db", "key": KEY, "attachments": "/a/att"}).to_string();
        assert!(parse_config(&good).is_ok());
        for bad in [
            json!({"db": "store.db", "key": KEY, "attachments": "/a/att"}),
            json!({"db": "/a/store.db", "key": "x' OR '1", "attachments": "/a/att"}),
            json!({"db": "/a/store.db", "key": KEY, "attachments": "../att"}),
            json!({"db": "/a/store.db", "key": KEY}),
            json!({"db": "/a/store.db", "key": KEY, "attachments": "/a/att", "link": ""}),
            json!({"db": "/a/store.db", "key": KEY, "attachments": "/a/att", "link": "a\nb"}),
            json!([1, 2, 3]),
        ] {
            assert!(parse_config(&bad.to_string()).is_err(), "{bad}");
        }
        assert!(parse_config("not json").is_err());
    }

    #[test]
    fn a_bad_request_is_refused_and_does_not_stop_the_engine() {
        assert!(parse_request("").is_err());
        assert!(parse_request("{").is_err());
        assert!(parse_request("null").is_err());
        assert!(parse_request("[]").is_err());
        assert!(parse_request(r#"{"id":"1"}"#).is_err());
        assert!(parse_request(r#"{"method":7}"#).is_err());
        assert!(parse_request(&format!(r#"{{"method":"send","pad":"{}"}}"#, "x".repeat(MAX_REQUEST))).is_err());
        let deep = format!("{}1{}", "[".repeat(100_000), "]".repeat(100_000));
        assert!(parse_request(&deep).is_err());
        let r = parse_request(r#"{"id":"9","method":"send"}"#).unwrap();
        assert_eq!(r.method, "send");
        assert!(r.params.is_object());
    }

    #[test]
    fn a_file_name_never_comes_from_the_sender() {
        let id = attachment_id(b"digest", "image/jpeg").unwrap();
        assert!(is_attachment_id(&id), "{id}");
        assert!(id.ends_with(".jpg"));
        // a content type cannot walk out of the folder
        for kind in ["../../etc/passwd", "image/../../x", "a/b\0c", "text/html", ""] {
            let id = attachment_id(b"digest", kind).unwrap();
            assert!(is_attachment_id(&id), "{kind} gave {id}");
            assert!(id.ends_with(".bin"), "{kind} gave {id}");
        }
        assert!(attachment_id(b"", "image/png").is_none());
        for bad in ["../x.jpg", "a/b.jpg", "", ".", "..", "0123456789abcdef01234567", "0123456789abcdef01234567.", "0123456789ABCDEF01234567.jpg", "0123456789abcdef01234567.j/g", "0123456789abcdef01234567.jpegx"] {
            assert!(!is_attachment_id(bad), "{bad}");
        }
    }

    #[test]
    fn a_group_id_is_made_from_the_master_key_alone() {
        let a = group_id(&[7u8; 32]).unwrap();
        assert_eq!(a, group_id(&[7u8; 32]).unwrap());
        assert_ne!(a, group_id(&[8u8; 32]).unwrap());
        assert_eq!(BASE64_STANDARD.decode(&a).unwrap().len(), 32);
        assert!(group_id(&[7u8; 31]).is_none());
        assert!(group_id(&[]).is_none());
    }

    #[test]
    fn a_mention_must_be_inside_the_message() {
        let who = "bcfeac83-b650-41e0-9fa0-d4e8d0b7456e";
        let ok = parse_mentions(&[json!(format!("3:1:{who}"))], "hi \u{FFFC}").unwrap();
        assert_eq!(ok.len(), 1);
        assert_eq!(ok[0].start, Some(3));
        for bad in [format!("3:2:{who}"), format!("9:1:{who}"), format!("0:0:{who}"), format!("4294967295:4294967295:{who}"), "3:1:not-a-uuid".to_string(), "3:1".to_string(), "x:1:y".to_string(), format!("-1:1:{who}")] {
            assert!(parse_mentions(&[json!(bad)], "hi \u{FFFC}").is_err(), "{bad}");
        }
        assert!(parse_mentions(&[json!(7)], "hi").is_err());
    }

    #[test]
    fn the_keyed_hash_is_hmac_sha256() {
        // RFC 4231, test case 1 and test case 2
        assert_eq!(hex::encode(hmac_sha256(&[0x0b; 20], b"Hi There")), "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7");
        assert_eq!(hex::encode(hmac_sha256(b"Jefe", b"what do ya want for nothing?")), "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843");
        // RFC 4231, test case 6: a key longer than one block
        assert_eq!(hex::encode(hmac_sha256(&[0xaa; 131], b"Test Using Larger Than Block-Size Key - Hash Key First")), "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54");
    }

    #[test]
    fn a_changed_identity_key_is_seen_and_said_once() {
        let mut ids = Identities::default();
        let (a, b) = (identity_mark(b"key one"), identity_mark(b"key two"));
        assert_ne!(a, b);
        assert_eq!(ids.note("member", &a), Seen::First);
        assert_eq!(ids.note("member", &a), Seen::Same);
        assert_eq!(ids.note("member", &b), Seen::Changed);
        assert_eq!(ids.note("member", &b), Seen::Same, "the new key is the known key after it was said");
        assert_eq!(ids.note("other", &b), Seen::First);
    }

    #[test]
    fn the_marks_survive_a_restart_and_a_changed_file_is_not_believed() {
        let mut ids = Identities::default();
        let mark = identity_mark(b"key one");
        ids.note("member", &mark);
        let file = ids.dump(KEY);

        let (mut back, whole) = Identities::load(Some(&file), KEY);
        assert!(whole);
        assert_eq!(back.note("member", &mark), Seen::Same);
        assert_eq!(back.note("member", &identity_mark(b"key two")), Seen::Changed);

        // No file is a first run. That is whole.
        assert!(Identities::load(None, KEY).1);

        // Someone puts another key's mark in the file, to hide a change: the file is refused.
        let forged = file.replace(&mark, &identity_mark(b"key two"));
        assert_ne!(forged, file);
        let (empty, whole) = Identities::load(Some(&forged), KEY);
        assert!(!whole);
        assert!(empty.marks.is_empty());

        // A file made with another store's key, a cut file and a file of junk are refused too.
        let other = "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff";
        assert!(!Identities::load(Some(&file), other).1);
        assert!(!Identities::load(Some(&file[..file.len() - 3]), KEY).1);
        for junk in ["", "{}", "null", r#"{"mac":"00","seen":"{}"}"#, r#"{"mac":7,"seen":[]}"#] {
            assert!(!Identities::load(Some(junk), KEY).1, "{junk}");
        }
    }

    #[test]
    fn a_uuid_goes_to_bytes_and_back() {
        let text = "bcfeac83-b650-41e0-9fa0-d4e8d0b7456e";
        assert_eq!(uuid_text(&uuid_bytes(text).unwrap()).unwrap(), text);
        assert!(uuid_text(&[1, 2, 3]).is_none());
    }

    #[test]
    fn a_data_message_keeps_its_timer_its_mention_and_its_delete() {
        let dm = DataMessage {
            body: Some("hi \u{FFFC}".into()),
            timestamp: Some(42),
            expire_timer: Some(2_419_200),
            body_ranges: vec![BodyRange {
                start: Some(3),
                length: Some(1),
                associated_value: Some(AssociatedValue::MentionAciBinary(uuid_bytes("bcfeac83-b650-41e0-9fa0-d4e8d0b7456e").unwrap())),
            }],
            ..Default::default()
        };
        let v = data_message_json(&dm, "gid");
        assert_eq!(v["expiresInSeconds"], 2_419_200);
        assert_eq!(v["groupInfo"]["groupId"], "gid");
        assert_eq!(v["mentions"][0]["uuid"], "bcfeac83-b650-41e0-9fa0-d4e8d0b7456e");
        assert!(v.get("remoteDelete").is_none());
    }
}
