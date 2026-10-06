import SwiftUI
import WebKit

extension WhistlegraphSession {
    func deletionClient() -> AccountDeletionClient {
        AccountDeletionClient(credential: { [weak self] in
            guard let self else { return nil }
            let generation = self.account.generation
            guard let token = try await self.account.token(), generation == self.account.generation else { return nil }
            return .init(bearer: token, generation: generation)
        }, current: { [weak self] in self?.account.generation == $0.generation }, clear: { [weak self] in
            guard let self else { return }
            try await self.eraseDeletedAccountLocally()
        })
    }
}

struct WhistlegraphDeleteAccountSheet: View {
    @ObservedObject var session: WhistlegraphSession
    @State private var client: AccountDeletionClient?
    @State private var preview: AccountDeletionClient.Preview?
    @State private var schedule: AccountDeletionClient.Schedule?
    @State private var busy = false
    @State private var notice = ""
    @State private var confirming = false
    var body: some View {
        List {
            if let schedule {
                Section {
                    Text("Account deletion scheduled").font(.headline)
                    Text("Your account is locked. Permanent deletion is scheduled after \(schedule.purgeDate?.formatted(date: .long, time: .shortened) ?? schedule.purgeAfter).")
                    if let error = schedule.localErasureError { Text(error) }
                    else { Text("This phone’s app data has been erased.") }
                    if schedule.mailed == true { Text("Check your email for the recovery link during the grace period.") }
                }
            } else if let preview {
                Section {
                    Text("Delete @\(preview.handle) across Aesthetic Computer?").font(.headline)
                    Text("This locks your AC account now and schedules permanent deletion after \(preview.graceDays) days. It includes your private Whistlegraph drafts, source, history, and AC uploads.")
                    LabeledContent("Purchased braincells to lose", value: preview.braincells.formatted(.number.precision(.fractionLength(0))))
                    if let count = preview.counts["whistlegraphs"] { LabeledContent("Private Whistlegraphs", value: count.formatted()) }
                    Text("It also erases all saved Whistlegraph work, source drafts, recordings and story caches on this phone. Export anything you want to keep first. Files already saved to Photos or shared elsewhere remain there.")
                    Text("Public blockchain records and independently pinned IPFS artwork cannot be erased. Records that must be retained for legal or transaction integrity purposes are handled as described in the privacy policy.")
                    Button("Delete my AC account", role: .destructive) { confirming = true }
                        .disabled(busy).accessibilityIdentifier("account-delete-confirm")
                }
            } else if busy { ProgressView("Reading the account…") }
            if !notice.isEmpty { Text(notice).accessibilityIdentifier("account-delete-notice") }
            if !busy && preview == nil && schedule == nil { Button("Read deletion preview") { Task { await load() } } }
        }
        .navigationTitle("Delete account").navigationBarTitleDisplayMode(.inline)
        .task { if client == nil { client = session.deletionClient(); await load() } }
        .confirmationDialog("Delete this AC account and this phone's saved Whistlegraphs?", isPresented: $confirming, titleVisibility: .visible) {
            Button("Delete account", role: .destructive) { Task { await confirm() } }
            Button("Keep account", role: .cancel) {}
        }
    }
    private func load() async {
        busy = true; notice = ""; defer { busy = false }
        do { preview = try await client?.load() } catch { notice = error.localizedDescription }
    }
    private func confirm() async {
        busy = true; notice = ""; defer { busy = false }
        do { schedule = try await client?.confirm() } catch { notice = error.localizedDescription }
    }
}
