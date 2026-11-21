// The key is in no log.
//
// The statement that opens the store holds the key, and the database library
// logs statements. This file has one test, alone in its program, because it
// listens to every log line of the whole process, on every thread.

use std::io::Write;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use nccd_engine::open_store;
use presage::model::identity::OnNewIdentity;
use presage::store::ContentsStore;
use presage_store_sqlite::{SqliteConnectOptions, SqliteStore};
use sqlx::ConnectOptions;

const KEY: &str = "5f1d0c7a9b3e2468ace013579bdf02468ace13579bdf0a1b2c3d4e5f60718293";
const WRONG: &str = "0000000000000000000000000000000000000000000000000000000000000000";
const CONTROL: &str = "c0ffee00c0ffee00c0ffee00c0ffee00c0ffee00c0ffee00c0ffee00c0ffee00";

#[derive(Clone, Default)]
struct Captured(Arc<Mutex<Vec<u8>>>);

impl Captured {
    fn take(&self) -> String {
        String::from_utf8_lossy(&std::mem::take(&mut *self.0.lock().unwrap())).to_string()
    }
}

impl Write for Captured {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.lock().unwrap().extend_from_slice(buf);
        Ok(buf.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl<'a> tracing_subscriber::fmt::MakeWriter<'a> for Captured {
    type Writer = Captured;
    fn make_writer(&'a self) -> Self::Writer {
        self.clone()
    }
}

#[test]
fn the_key_is_in_no_log_at_any_level() {
    let captured = Captured::default();
    tracing_subscriber::fmt().with_max_level(tracing::Level::TRACE).with_ansi(false).with_writer(captured.clone()).init();
    let rt = tokio::runtime::Builder::new_current_thread().enable_all().build().unwrap();
    let dir = tempfile::tempdir().unwrap();

    // The control: a store opened with statement logging on does put its key in the log.
    // This shows that the fault is real, and that this test can see it.
    let control = dir.path().join("control.db");
    rt.block_on(async {
        let options = SqliteConnectOptions::new()
            .filename(&control)
            .create_if_missing(true)
            .pragma("key", format!("'{CONTROL}'"))
            .log_statements(log::LevelFilter::Info)
            .log_slow_statements(log::LevelFilter::Warn, Duration::from_nanos(1));
        SqliteStore::open_with_options(options, OnNewIdentity::Trust).await.unwrap();
    });
    let log = captured.take();
    assert!(log.contains(CONTROL), "the control did not log its key, so this test proves nothing");

    // The engine: the same work and more, and no key is anywhere.
    let db = dir.path().join("store.db");
    rt.block_on(async {
        let mut store = open_store(db.to_str().unwrap(), KEY).await.unwrap();
        store.clear_messages().await.unwrap();
        let _ = store.groups().await.unwrap().count();
        let _ = open_store(db.to_str().unwrap(), WRONG).await;
    });
    let log = captured.take();
    assert!(!log.contains(KEY), "the key is in a log");
    assert!(!log.contains(WRONG), "a key that was tried is in a log");
    assert!(!log.to_lowercase().contains("pragma key"), "the statement that holds the key is in a log");
}
