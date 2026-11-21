// The C face, used as the app uses it: bad input is refused and nothing
// faults, an engine with no link says so and ends, and no word comes after stop.

use std::ffi::{c_char, c_void, CStr, CString};
use std::sync::Mutex;
use std::time::{Duration, Instant};

use nccd_engine::{nccd_engine_request, nccd_engine_start, nccd_engine_stop};

const KEY: &str = "5f1d0c7a9b3e2468ace013579bdf02468ace13579bdf0a1b2c3d4e5f60718293";

extern "C" fn collect(ctx: *mut c_void, json: *const c_char) {
    let lines = unsafe { &*(ctx as *const Mutex<Vec<String>>) };
    lines.lock().unwrap().push(unsafe { CStr::from_ptr(json) }.to_string_lossy().to_string());
}

fn c(text: &str) -> CString {
    CString::new(text).unwrap()
}

#[test]
fn a_bad_config_starts_no_engine() {
    let lines = Mutex::new(Vec::<String>::new());
    let ctx = &lines as *const _ as *mut c_void;
    unsafe {
        assert!(nccd_engine_start(std::ptr::null(), collect, ctx).is_null());
        for bad in [
            "".to_string(),
            "{".to_string(),
            "null".to_string(),
            r#"{"db":"/tmp/x.db"}"#.to_string(),
            r#"{"db":"relative.db","key":"00","attachments":"/tmp/a"}"#.to_string(),
            format!(r#"{{"db":"/tmp/nccd-x.db","key":"{}'","attachments":"/tmp/a"}}"#, &KEY[..63]),
        ] {
            assert!(nccd_engine_start(c(&bad).as_ptr(), collect, ctx).is_null(), "{bad}");
        }
        // Calls on no engine do nothing.
        assert_eq!(nccd_engine_request(std::ptr::null_mut(), c("{}").as_ptr()), -1);
        nccd_engine_stop(std::ptr::null_mut());
    }
    assert!(lines.lock().unwrap().is_empty());
}

#[test]
fn an_engine_with_no_link_says_so_and_ends_and_is_silent_after_stop() {
    let dir = tempfile::tempdir().unwrap();
    let lines = Box::leak(Box::new(Mutex::new(Vec::<String>::new())));
    let ctx = lines as *const _ as *mut c_void;
    let config = serde_json::json!({
        "db": dir.path().join("store.db"), "key": KEY, "attachments": dir.path().join("attachments"),
    })
    .to_string();
    unsafe {
        let engine = nccd_engine_start(c(&config).as_ptr(), collect, ctx);
        assert!(!engine.is_null());
        // Requests of every bad shape, while it starts and after it has ended.
        assert_eq!(nccd_engine_request(engine, std::ptr::null()), -1);
        let long = format!(r#"{{"method":"send","pad":"{}"}}"#, "x".repeat(300 * 1024));
        assert_eq!(nccd_engine_request(engine, c(&long).as_ptr()), -1);
        for junk in ["", "{", "[]", r#"{"method":"send"}"#, r#"{"id":1,"method":"unlink"}"#, "\u{FFFC}\u{202E}"] {
            let _ = nccd_engine_request(engine, c(junk).as_ptr());
        }
        let until = Instant::now() + Duration::from_secs(20);
        while Instant::now() < until && !lines.lock().unwrap().iter().any(|l| l.contains("\"exit\"")) {
            std::thread::sleep(Duration::from_millis(50));
        }
        nccd_engine_stop(engine);
    }
    let seen = lines.lock().unwrap().clone();
    let exit = seen.iter().find(|l| l.contains("\"exit\"")).expect("the engine said that it ended");
    assert!(exit.contains("not linked"), "{exit}");
    // It took no request for an account that is not linked: nothing was sent, nothing unlinked.
    assert!(!seen.iter().any(|l| l.contains("\"result\"")), "{seen:?}");
    assert!(!seen.iter().any(|l| l.contains(KEY)), "the key is in the engine's words");
    let count = seen.len();
    std::thread::sleep(Duration::from_millis(300));
    assert_eq!(lines.lock().unwrap().len(), count, "a word came after stop");
    // The store it made is for the owner only.
    use std::os::unix::fs::PermissionsExt;
    let mode = std::fs::metadata(dir.path().join("store.db")).unwrap().permissions().mode() & 0o777;
    assert_eq!(mode, 0o600);
}
