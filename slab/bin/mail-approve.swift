// mail-approve.swift — a small window for approving staged email drafts.
//
// Each draft shows the rendered email on the left and its attached PDF on the
// right, with two buttons: Hold (Esc) and Send (Return). Nothing is sent here.
// When the last draft is decided, or the window is closed, the decisions are
// printed as one JSON line and the tool exits:
//
//   {"send":["<id>", ...],"hold":["<id>", ...]}
//
// The caller (an agent holding the mail tool) reads that line and sends only
// the ids under "send". Closing the window holds every draft not yet decided.
//
// Usage:
//   mail-approve <items.json>
//
// items.json is an array of:
//   {"id": "...", "to": "...", "subject": "...",
//    "preview": "/path/to/rendered-email.png", "attachments": ["/path/a.pdf"]}
//
// Standalone and dependency-free:
//   swiftc -O mail-approve.swift -o mail-approve
import AppKit
import PDFKit

struct Item: Decodable {
  let id: String
  let to: String
  let subject: String
  let preview: String
  let attachments: [String]?
}

final class Approver: NSObject, NSApplicationDelegate, NSWindowDelegate {
  let items: [Item]
  var index = 0
  var send: [String] = []
  var hold: [String] = []

  let window = NSWindow(
    contentRect: NSRect(x: 0, y: 0, width: 1180, height: 820),
    styleMask: [.titled, .closable, .resizable], backing: .buffered, defer: false)
  let heading = NSTextField(labelWithString: "")
  let detail = NSTextField(labelWithString: "")
  let imageView = NSImageView()
  let emailScroll = NSScrollView()
  let pdfView = PDFView()
  let attachLabel = NSTextField(labelWithString: "")

  init(items: [Item]) { self.items = items }

  func applicationDidFinishLaunching(_ note: Notification) {
    let content = NSView()
    window.contentView = content
    window.delegate = self

    heading.font = .boldSystemFont(ofSize: 17)
    detail.font = .systemFont(ofSize: 12)
    detail.textColor = .secondaryLabelColor
    attachLabel.font = .systemFont(ofSize: 11)
    attachLabel.textColor = .secondaryLabelColor
    attachLabel.alignment = .right

    imageView.imageScaling = .scaleNone
    imageView.imageAlignment = .alignTopLeft
    emailScroll.documentView = imageView
    emailScroll.hasVerticalScroller = true
    emailScroll.borderType = .lineBorder
    emailScroll.drawsBackground = true
    emailScroll.backgroundColor = .white

    pdfView.autoScales = true
    pdfView.displayMode = .singlePage
    pdfView.backgroundColor = .windowBackgroundColor

    let holdButton = NSButton(title: "Hold", target: self, action: #selector(holdIt))
    holdButton.keyEquivalent = "\u{1b}"
    let sendButton = NSButton(title: "Send", target: self, action: #selector(sendIt))
    sendButton.keyEquivalent = "\r"
    sendButton.bezelColor = .systemPink

    for view in [heading, detail, attachLabel, emailScroll, pdfView, holdButton, sendButton] as [NSView] {
      view.translatesAutoresizingMaskIntoConstraints = false
      content.addSubview(view)
    }
    NSLayoutConstraint.activate([
      heading.topAnchor.constraint(equalTo: content.topAnchor, constant: 14),
      heading.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 16),
      detail.topAnchor.constraint(equalTo: heading.bottomAnchor, constant: 3),
      detail.leadingAnchor.constraint(equalTo: heading.leadingAnchor),
      attachLabel.centerYAnchor.constraint(equalTo: detail.centerYAnchor),
      attachLabel.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),

      emailScroll.topAnchor.constraint(equalTo: detail.bottomAnchor, constant: 10),
      emailScroll.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 16),
      emailScroll.bottomAnchor.constraint(equalTo: sendButton.topAnchor, constant: -12),
      emailScroll.widthAnchor.constraint(equalTo: content.widthAnchor, multiplier: 0.5, constant: -22),

      pdfView.topAnchor.constraint(equalTo: emailScroll.topAnchor),
      pdfView.bottomAnchor.constraint(equalTo: emailScroll.bottomAnchor),
      pdfView.leadingAnchor.constraint(equalTo: emailScroll.trailingAnchor, constant: 12),
      pdfView.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),

      sendButton.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),
      sendButton.bottomAnchor.constraint(equalTo: content.bottomAnchor, constant: -14),
      sendButton.widthAnchor.constraint(equalToConstant: 110),
      holdButton.trailingAnchor.constraint(equalTo: sendButton.leadingAnchor, constant: -10),
      holdButton.centerYAnchor.constraint(equalTo: sendButton.centerYAnchor),
      holdButton.widthAnchor.constraint(equalToConstant: 110),
    ])

    content.layoutSubtreeIfNeeded()
    show()
    window.center()
    window.makeKeyAndOrderFront(nil)
    NSApp.activate(ignoringOtherApps: true)
  }

  // Put the current draft on screen.
  func show() {
    let item = items[index]
    window.title = "Approve to send — \(index + 1) of \(items.count)"
    heading.stringValue = item.subject
    detail.stringValue = "to \(item.to)"

    if let image = NSImage(contentsOfFile: item.preview) {
      // Fit the rendered email to the pane's width; it scrolls if it is taller.
      let pixels = image.representations.first.map { NSSize(width: $0.pixelsWide, height: $0.pixelsHigh) } ?? image.size
      let width = max(emailScroll.contentSize.width, 200)
      let size = NSSize(width: width, height: width * pixels.height / max(pixels.width, 1))
      image.size = size
      imageView.image = image
      // A short email sits at the top of the pane, not the bottom.
      let frame = NSSize(width: width, height: max(size.height, emailScroll.contentSize.height))
      imageView.frame = NSRect(origin: .zero, size: frame)
      imageView.scroll(NSPoint(x: 0, y: frame.height))
    } else {
      imageView.image = nil
    }

    let pdfs = (item.attachments ?? []).filter { $0.lowercased().hasSuffix(".pdf") }
    if let path = pdfs.first, let doc = PDFDocument(url: URL(fileURLWithPath: path)) {
      pdfView.document = doc
      pdfView.isHidden = false
    } else {
      pdfView.document = nil
      pdfView.isHidden = true
    }
    let names = (item.attachments ?? []).map { ($0 as NSString).lastPathComponent }
    attachLabel.stringValue = names.isEmpty ? "no attachment" : "attached: " + names.joined(separator: ", ")
  }

  @objc func sendIt() { send.append(items[index].id); next() }
  @objc func holdIt() { hold.append(items[index].id); next() }

  func next() {
    index += 1
    if index < items.count { show() } else { finish() }
  }

  // Print the decisions and leave. Anything not decided is held.
  func finish() {
    let decided = Set(send + hold)
    for item in items where !decided.contains(item.id) { hold.append(item.id) }
    let out: [String: [String]] = ["send": send, "hold": hold]
    if let data = try? JSONSerialization.data(withJSONObject: out, options: [.sortedKeys]),
      let line = String(data: data, encoding: .utf8)
    {
      print(line)
    }
    exit(0)
  }

  func windowWillClose(_ note: Notification) { finish() }
}

guard CommandLine.arguments.count == 2,
  let data = FileManager.default.contents(atPath: CommandLine.arguments[1]),
  let items = try? JSONDecoder().decode([Item].self, from: data), !items.isEmpty
else {
  FileHandle.standardError.write("usage: mail-approve <items.json>\n".data(using: .utf8)!)
  exit(2)
}

let app = NSApplication.shared
app.setActivationPolicy(.regular)
let approver = Approver(items: items)
app.delegate = approver
app.run()
