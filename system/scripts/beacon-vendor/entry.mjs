import { DAppClient as BeaconDAppClient } from '@airgap/beacon-dapp';
// Beacon 4.8.1 writes to its metrics IndexedDB even with enableMetrics:false.
// Suppress this optional telemetry completely, including the first-run IDB race.
export class DAppClient extends BeaconDAppClient {
  sendMetrics() {}
}
