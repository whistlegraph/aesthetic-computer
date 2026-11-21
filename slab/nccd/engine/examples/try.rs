// try — run the engine by hand, through its C face, as the app will.
//
//   NCCD_KEY=<64 hex> try <dir> link <device name>   link, then listen
//   NCCD_KEY=<64 hex> try <dir> run [seconds]        listen (default 60)
//
// It names the club's groups to the engine (those whose name has "ncc" or
// "computer club"), and then prints one line for each thing that arrives. It
// prints no words of any message: only what kind of thing it was.

use std::ffi::{c_char, c_void, CStr, CString};
use std::sync::mpsc::{channel, Sender};
use std::time::{Duration, Instant};

use nccd_engine::{nccd_engine_request, nccd_engine_start, nccd_engine_stop};
use serde_json::{json, Value};

extern "C" fn forward(ctx: *mut c_void, json: *const c_char) {
    let tx = unsafe { &*(ctx as *const Sender<String>) };
    let _ = tx.send(unsafe { CStr::from_ptr(json) }.to_string_lossy().to_string());
}

fn main() {
    // NCCD_TRACE=1 shows what the libraries under the engine say. The database's own
    // lines stay off: the engine turns its statements off, and this keeps it so.
    if std::env::var("NCCD_TRACE").is_ok() {
        let filter = tracing_subscriber::EnvFilter::new("debug,sqlx=off,libsignal_protocol=info,h2=off,hyper=off,rustls=off");
        tracing_subscriber::fmt().with_env_filter(filter).with_ansi(false).with_writer(std::io::stderr).init();
    }
    let args: Vec<String> = std::env::args().collect();
    let (dir, mode) = (args.get(1).expect("a folder"), args.get(2).map(String::as_str).unwrap_or("run"));
    let key = std::env::var("NCCD_KEY").expect("NCCD_KEY");
    let mut config = json!({ "db": format!("{dir}/store.db"), "key": key, "attachments": format!("{dir}/attachments") });
    let mut seconds = 60;
    if mode == "link" {
        config["link"] = json!(args.get(3).cloned().unwrap_or_else(|| "nccd-engine-trial".into()));
        seconds = 240;
    } else if let Some(n) = args.get(3).and_then(|s| s.parse().ok()) {
        seconds = n;
    }
    let (tx, rx) = channel::<String>();
    let ctx = Box::into_raw(Box::new(tx)) as *mut c_void;
    let engine = unsafe { nccd_engine_start(CString::new(config.to_string()).unwrap().as_ptr(), forward, ctx) };
    assert!(!engine.is_null(), "the config was refused");
    let ask = |v: Value| unsafe { nccd_engine_request(engine, CString::new(v.to_string()).unwrap().as_ptr()) };

    let until = Instant::now() + Duration::from_secs(seconds);
    while let Ok(line) = rx.recv_timeout(until.saturating_duration_since(Instant::now())) {
        let v: Value = serde_json::from_str(&line).unwrap_or(Value::Null);
        match v["method"].as_str() {
            Some("linkUri") => {
                std::fs::write(format!("{dir}/link.uri"), v["params"]["uri"].as_str().unwrap_or("")).unwrap();
                println!("linkUri written to {dir}/link.uri");
            }
            Some("account") => {
                println!("account: linked");
                ask(json!({ "id": "groups", "method": "listGroups" }));
            }
            Some("receive") => {
                let e = &v["params"]["envelope"];
                let (way, m) = if e["dataMessage"].is_object() { ("in", &e["dataMessage"]) } else { ("sent", &e["syncMessage"]["sentMessage"]) };
                println!(
                    "receive {way}: group={} words={} timer={} mentions={} files={} reaction={} delete={} quote={}",
                    &m["groupInfo"]["groupId"].as_str().unwrap_or("")[..6],
                    m["message"].as_str().map(|s| s.chars().count()).unwrap_or(0),
                    m["expiresInSeconds"],
                    m["mentions"].as_array().map(Vec::len).unwrap_or(0),
                    m["attachments"].as_array().map(|a| a.iter().map(|f| format!("{}:{}", f["id"].as_str().unwrap_or(""), f["size"])).collect::<Vec<_>>().join(",")).unwrap_or_default(),
                    m["reaction"].is_object(),
                    m["remoteDelete"].is_object(),
                    m["quote"].is_object(),
                );
            }
            Some("exit") => {
                println!("exit: {}", v["params"]["reason"]);
                break;
            }
            Some("tally") => println!("tally {}", v["params"]),
            Some(other) => println!("{other}"),
            None if v["id"] == "groups" => {
                let all = v["result"].as_array().cloned().unwrap_or_default();
                let club: Vec<&Value> = all.iter().filter(|g| {
                    let n = g["name"].as_str().unwrap_or("").to_lowercase();
                    n.contains("ncc") || n.contains("computer club")
                }).collect();
                println!("groups: {} on the account, {} are the club's", all.len(), club.len());
                for g in &club {
                    println!("  club: {} members={} timer={}", g["name"].as_str().unwrap_or(""), g["members"].as_array().map(Vec::len).unwrap_or(0), g["messageExpirationTime"]);
                }
                let ids: Vec<&Value> = club.iter().map(|g| &g["id"]).collect();
                ask(json!({ "id": "set", "method": "setGroups", "params": { "ids": ids } }));
                ask(json!({ "id": "contacts", "method": "requestContacts" }));
                // a group that is not the club's must be refused
                if let Some(other) = all.iter().find(|g| !club.iter().any(|c| c["id"] == g["id"])) {
                    ask(json!({ "id": "refuse", "method": "send", "params": { "groupId": other["id"], "message": "this must not be sent" } }));
                }
            }
            None => println!("reply {}: {}", v["id"], if v["error"].is_object() { format!("error: {}", v["error"]["message"]) } else { v["result"].to_string() }),
        }
    }
    unsafe { nccd_engine_stop(engine) };
    println!("stopped");
}
