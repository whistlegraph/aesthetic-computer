// The store's rules, tested on a real store: it is encrypted, a wrong key does
// not open it, and only the owner reads it. (tests/log.rs: the key is in no log.)

use std::os::unix::fs::PermissionsExt;

use nccd_engine::open_store;
use presage::store::{ContentsStore, StateStore};

const KEY: &str = "5f1d0c7a9b3e2468ace013579bdf02468ace13579bdf0a1b2c3d4e5f60718293";
const WRONG: &str = "0000000000000000000000000000000000000000000000000000000000000000";

fn runtime() -> tokio::runtime::Runtime {
    tokio::runtime::Builder::new_current_thread().enable_all().build().unwrap()
}

#[test]
fn the_store_is_encrypted_and_a_wrong_key_does_not_open_it() {
    let dir = tempfile::tempdir().unwrap();
    let db = dir.path().join("deep/store.db");
    let db = db.to_str().unwrap();
    runtime().block_on(async {
        let mut store = open_store(db, KEY).await.expect("the right key opens the store");
        assert!(!store.is_registered().await);
        store.clear_messages().await.unwrap();
        drop(store);

        // On disk it is not a SQLite file that any tool can read.
        let head = std::fs::read(db).unwrap();
        assert!(head.len() >= 16);
        assert_ne!(&head[..15], b"SQLite format 3");
        // No run of the file is the key.
        assert!(!head.windows(KEY.len()).any(|w| w == KEY.as_bytes()));

        assert!(open_store(db, WRONG).await.is_err(), "a wrong key must not open the store");
        assert!(open_store(db, KEY).await.is_ok(), "the right key opens it again");
    });
    // Only the owner reads the store and its folder.
    let mode = |p: &str| std::fs::metadata(p).unwrap().permissions().mode() & 0o777;
    assert_eq!(mode(db), 0o600);
    assert_eq!(mode(dir.path().join("deep").to_str().unwrap()), 0o700);
}

#[test]
fn a_key_that_is_not_hex_is_refused_before_it_reaches_the_store() {
    let dir = tempfile::tempdir().unwrap();
    let db = dir.path().join("store.db");
    runtime().block_on(async {
        for bad in ["", "short", "x'; ATTACH DATABASE '/tmp/x' AS y; --", &format!("{}'", &KEY[..63])] {
            assert!(open_store(db.to_str().unwrap(), bad).await.is_err(), "{bad}");
        }
    });
    assert!(!db.exists(), "a refused key makes no file");
}
