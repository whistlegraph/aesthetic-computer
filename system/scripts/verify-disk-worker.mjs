import path from "node:path";
import { fileURLToPath } from "node:url";
import { verifyDiskWorker } from "./disk-worker-integrity.mjs";

const scriptsDir = path.dirname(fileURLToPath(import.meta.url));
const systemDir = process.env.AC_DISK_WORKER_SYSTEM_DIR
  ? path.resolve(process.env.AC_DISK_WORKER_SYSTEM_DIR)
  : path.resolve(scriptsDir, "..");

try {
  const result = await verifyDiskWorker(systemDir);
  console.log(
    `${result.filename} verified (${result.bytes} bytes, ${result.sourceCount} sources)`,
  );
} catch (error) {
  console.error(`Disk worker verification failed: ${error.message}`);
  process.exit(1);
}
