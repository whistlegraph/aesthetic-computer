import AppKit
import ImageIO
import Quartz

struct ArchiveManifest: Decodable {
  struct Counts: Decodable { let posts: Int; let stills: Int }
  let title: String
  let account: String
  let counts: Counts
  let posts: [ArchivePost]
}

struct ArchivePost: Decodable {
  let shortcode: String
  let url: String
  let date: String
  let caption: String
  let stills: [ArchiveStill]
}

struct ArchiveStill: Decodable {
  let file: String?
  let alt: String?
}

final class ThumbnailLoader {
  static let shared = ThumbnailLoader()
  private let cache = NSCache<NSString, NSImage>()
  private let queue = DispatchQueue(label: "computer.aesthetic.dmgify.thumbnails", qos: .userInitiated, attributes: .concurrent)

  private init() {
    cache.countLimit = 96
    cache.totalCostLimit = 192 * 1024 * 1024
  }

  func load(_ url: URL, maximumPixelSize: Int, completion: @escaping (NSImage?) -> Void) {
    let key = "\(url.path)#\(maximumPixelSize)" as NSString
    if let image = cache.object(forKey: key) {
      completion(image)
      return
    }
    queue.async {
      let options: [CFString: Any] = [
        kCGImageSourceShouldCache: false,
        kCGImageSourceCreateThumbnailFromImageAlways: true,
        kCGImageSourceCreateThumbnailWithTransform: true,
        kCGImageSourceThumbnailMaxPixelSize: maximumPixelSize,
        kCGImageSourceShouldCacheImmediately: true,
      ]
      guard let source = CGImageSourceCreateWithURL(url as CFURL, nil),
            let cgImage = CGImageSourceCreateThumbnailAtIndex(source, 0, options as CFDictionary) else {
        DispatchQueue.main.async { completion(nil) }
        return
      }
      let image = NSImage(cgImage: cgImage, size: NSSize(width: cgImage.width, height: cgImage.height))
      let cost = cgImage.bytesPerRow * cgImage.height
      self.cache.setObject(image, forKey: key, cost: cost)
      DispatchQueue.main.async { completion(image) }
    }
  }
}

final class ResponsiveFlowLayout: NSCollectionViewFlowLayout {
  override func shouldInvalidateLayout(forBoundsChange newBounds: NSRect) -> Bool { true }
}

final class ThumbnailTableCell: NSTableCellView {
  private let picture = NSImageView()
  private var representedURL: URL?

  override init(frame frameRect: NSRect) {
    super.init(frame: frameRect)
    picture.translatesAutoresizingMaskIntoConstraints = false
    picture.imageScaling = .scaleProportionallyUpOrDown
    picture.wantsLayer = true
    picture.layer?.cornerRadius = 5
    picture.layer?.masksToBounds = true
    picture.layer?.backgroundColor = NSColor.quaternaryLabelColor.cgColor
    addSubview(picture)
    NSLayoutConstraint.activate([
      picture.centerXAnchor.constraint(equalTo: centerXAnchor),
      picture.centerYAnchor.constraint(equalTo: centerYAnchor),
      picture.widthAnchor.constraint(equalToConstant: 56),
      picture.heightAnchor.constraint(equalToConstant: 56),
    ])
  }

  required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

  func configure(url: URL?) {
    representedURL = url
    picture.image = nil
    guard let url else { return }
    ThumbnailLoader.shared.load(url, maximumPixelSize: 160) { [weak self] image in
      guard let self, self.representedURL == url else { return }
      self.picture.image = image
    }
  }
}

final class ArchiveCardItem: NSCollectionViewItem {
  static let identifier = NSUserInterfaceItemIdentifier("ArchiveCardItem")
  private let picture = NSImageView()
  private let dateLabel = NSTextField(labelWithString: "")
  private let captionLabel = NSTextField(wrappingLabelWithString: "")
  private let countLabel = NSTextField(labelWithString: "")
  private var representedURL: URL?

  override var isSelected: Bool {
    didSet { updateSelection() }
  }

  override func loadView() {
    let card = NSView()
    card.wantsLayer = true
    card.layer?.cornerRadius = 12
    card.layer?.masksToBounds = true
    card.layer?.backgroundColor = NSColor.controlBackgroundColor.cgColor
    card.layer?.borderWidth = 1
    card.layer?.borderColor = NSColor.separatorColor.cgColor

    picture.translatesAutoresizingMaskIntoConstraints = false
    picture.imageScaling = .scaleProportionallyUpOrDown
    picture.imageAlignment = .alignCenter
    picture.wantsLayer = true
    picture.layer?.backgroundColor = NSColor.quaternaryLabelColor.cgColor

    dateLabel.translatesAutoresizingMaskIntoConstraints = false
    dateLabel.font = .monospacedDigitSystemFont(ofSize: 11, weight: .medium)
    dateLabel.textColor = .secondaryLabelColor
    dateLabel.lineBreakMode = .byTruncatingTail

    captionLabel.translatesAutoresizingMaskIntoConstraints = false
    captionLabel.font = .systemFont(ofSize: 12)
    captionLabel.textColor = .labelColor
    captionLabel.maximumNumberOfLines = 2
    captionLabel.lineBreakMode = .byTruncatingTail

    countLabel.translatesAutoresizingMaskIntoConstraints = false
    countLabel.font = .systemFont(ofSize: 11, weight: .semibold)
    countLabel.textColor = .white
    countLabel.alignment = .center
    countLabel.wantsLayer = true
    countLabel.layer?.backgroundColor = NSColor.black.withAlphaComponent(0.68).cgColor
    countLabel.layer?.cornerRadius = 10
    countLabel.isHidden = true

    card.addSubview(picture)
    card.addSubview(dateLabel)
    card.addSubview(captionLabel)
    card.addSubview(countLabel)
    NSLayoutConstraint.activate([
      picture.topAnchor.constraint(equalTo: card.topAnchor),
      picture.leadingAnchor.constraint(equalTo: card.leadingAnchor),
      picture.trailingAnchor.constraint(equalTo: card.trailingAnchor),
      picture.heightAnchor.constraint(equalTo: card.widthAnchor),
      dateLabel.topAnchor.constraint(equalTo: picture.bottomAnchor, constant: 12),
      dateLabel.leadingAnchor.constraint(equalTo: card.leadingAnchor, constant: 13),
      dateLabel.trailingAnchor.constraint(equalTo: card.trailingAnchor, constant: -13),
      captionLabel.topAnchor.constraint(equalTo: dateLabel.bottomAnchor, constant: 7),
      captionLabel.leadingAnchor.constraint(equalTo: card.leadingAnchor, constant: 13),
      captionLabel.trailingAnchor.constraint(equalTo: card.trailingAnchor, constant: -13),
      captionLabel.bottomAnchor.constraint(lessThanOrEqualTo: card.bottomAnchor, constant: -13),
      countLabel.topAnchor.constraint(equalTo: picture.topAnchor, constant: 10),
      countLabel.trailingAnchor.constraint(equalTo: picture.trailingAnchor, constant: -10),
      countLabel.heightAnchor.constraint(equalToConstant: 20),
      countLabel.widthAnchor.constraint(greaterThanOrEqualToConstant: 34),
    ])
    self.view = card
    updateSelection()
  }

  override func prepareForReuse() {
    super.prepareForReuse()
    representedURL = nil
    picture.image = nil
    dateLabel.stringValue = ""
    captionLabel.stringValue = ""
    countLabel.isHidden = true
  }

  func configure(post: ArchivePost, archiveRoot: URL) {
    let day = String(post.date.prefix(10))
    dateLabel.stringValue = "\(day)  ·  \(post.shortcode)"
    captionLabel.stringValue = post.caption.isEmpty ? "No caption" : post.caption
    captionLabel.textColor = post.caption.isEmpty ? .secondaryLabelColor : .labelColor
    if post.stills.count > 1 {
      countLabel.stringValue = " 1 / \(post.stills.count) "
      countLabel.isHidden = false
    } else {
      countLabel.isHidden = true
    }
    guard let file = post.stills.first?.file else { return }
    let url = archiveRoot.appendingPathComponent(file)
    representedURL = url
    ThumbnailLoader.shared.load(url, maximumPixelSize: 480) { [weak self] image in
      guard let self, self.representedURL == url else { return }
      self.picture.image = image
    }
  }

  private func updateSelection() {
    guard isViewLoaded else { return }
    view.layer?.borderWidth = isSelected ? 3 : 1
    view.layer?.borderColor = (isSelected ? NSColor.controlAccentColor : NSColor.separatorColor).cgColor
  }
}

final class GalleryController: NSViewController, NSCollectionViewDataSource,
  NSCollectionViewDelegate, NSCollectionViewDelegateFlowLayout, NSSearchFieldDelegate,
  NSTableViewDataSource, NSTableViewDelegate, NSToolbarDelegate,
  NSSharingServicePickerToolbarItemDelegate, QLPreviewPanelDataSource {

  private static let searchID = NSToolbarItem.Identifier("ArchiveSearch")
  private static let modeID = NSToolbarItem.Identifier("ArchiveViewMode")
  private static let shareID = NSToolbarItem.Identifier("ArchiveShare")
  private let manifest: ArchiveManifest
  private let archiveRoot: URL
  private var filteredPosts: [ArchivePost]
  private let collectionView = NSCollectionView()
  private let tableView = NSTableView()
  private let gridScroll = NSScrollView()
  private let listScroll = NSScrollView()
  private let searchField = NSSearchField()
  private let modeControl = NSSegmentedControl()
  private var sortColumn = "date"
  private var sortAscending = false
  private lazy var shareItem: NSSharingServicePickerToolbarItem = {
    let item = NSSharingServicePickerToolbarItem(itemIdentifier: Self.shareID)
    item.label = "Share"
    item.paletteLabel = "Share selected post"
    item.toolTip = "Share selected post"
    item.delegate = self
    item.isEnabled = false
    return item
  }()
  private var previewURLs: [URL] = []
  private let subtitle = NSTextField(labelWithString: "")

  init(manifest: ArchiveManifest, archiveRoot: URL) {
    self.manifest = manifest
    self.archiveRoot = archiveRoot
    self.filteredPosts = manifest.posts
    super.init(nibName: nil, bundle: nil)
  }

  required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

  override func loadView() {
    let root = NSView()
    root.wantsLayer = true
    root.layer?.backgroundColor = NSColor.windowBackgroundColor.cgColor

    let heading = NSTextField(labelWithString: manifest.title.components(separatedBy: " —").first ?? manifest.title)
    heading.translatesAutoresizingMaskIntoConstraints = false
    heading.font = .systemFont(ofSize: 29, weight: .bold)
    heading.textColor = .labelColor
    heading.lineBreakMode = .byTruncatingTail

    subtitle.translatesAutoresizingMaskIntoConstraints = false
    subtitle.font = .systemFont(ofSize: 13, weight: .regular)
    subtitle.textColor = .secondaryLabelColor
    updateSubtitle()

    let header = NSView()
    header.translatesAutoresizingMaskIntoConstraints = false
    header.addSubview(heading)
    header.addSubview(subtitle)

    let layout = ResponsiveFlowLayout()
    layout.minimumInteritemSpacing = 10
    layout.minimumLineSpacing = 10
    layout.sectionInset = NSEdgeInsets(top: 14, left: 16, bottom: 20, right: 16)
    collectionView.collectionViewLayout = layout
    collectionView.translatesAutoresizingMaskIntoConstraints = false
    collectionView.backgroundColors = [.windowBackgroundColor]
    collectionView.dataSource = self
    collectionView.delegate = self
    collectionView.isSelectable = true
    collectionView.allowsMultipleSelection = false
    collectionView.register(ArchiveCardItem.self, forItemWithIdentifier: ArchiveCardItem.identifier)
    let doubleClick = NSClickGestureRecognizer(target: self, action: #selector(handleDoubleClick(_:)))
    doubleClick.numberOfClicksRequired = 2
    collectionView.addGestureRecognizer(doubleClick)

    gridScroll.translatesAutoresizingMaskIntoConstraints = false
    gridScroll.documentView = collectionView
    gridScroll.hasVerticalScroller = true
    gridScroll.autohidesScrollers = true
    gridScroll.drawsBackground = false

    func makeColumn(_ id: String, _ title: String, _ width: CGFloat, _ minimum: CGFloat) -> NSTableColumn {
      let column = NSTableColumn(identifier: NSUserInterfaceItemIdentifier(id))
      column.title = title
      column.width = width
      column.minWidth = minimum
      column.sortDescriptorPrototype = NSSortDescriptor(key: id, ascending: id != "date")
      return column
    }
    let thumbnailColumn = makeColumn("thumbnail", "", 76, 70)
    thumbnailColumn.maxWidth = 82
    thumbnailColumn.sortDescriptorPrototype = nil
    tableView.addTableColumn(thumbnailColumn)
    tableView.addTableColumn(makeColumn("date", "Date", 106, 96))
    tableView.addTableColumn(makeColumn("caption", "Original caption", 420, 220))
    let imagesColumn = makeColumn("images", "Images", 68, 62)
    imagesColumn.maxWidth = 100
    tableView.addTableColumn(imagesColumn)
    tableView.addTableColumn(makeColumn("shortcode", "Shortcode", 124, 110))
    tableView.delegate = self
    tableView.dataSource = self
    tableView.rowHeight = 68
    tableView.intercellSpacing = NSSize(width: 10, height: 2)
    tableView.usesAlternatingRowBackgroundColors = true
    tableView.allowsMultipleSelection = false
    tableView.allowsEmptySelection = true
    tableView.columnAutoresizingStyle = .lastColumnOnlyAutoresizingStyle
    tableView.target = self
    tableView.doubleAction = #selector(openSelection)
    tableView.sortDescriptors = [NSSortDescriptor(key: "date", ascending: false)]

    listScroll.translatesAutoresizingMaskIntoConstraints = false
    listScroll.documentView = tableView
    listScroll.hasVerticalScroller = true
    listScroll.autohidesScrollers = true
    listScroll.drawsBackground = false
    listScroll.isHidden = true

    root.addSubview(header)
    root.addSubview(gridScroll)
    root.addSubview(listScroll)
    NSLayoutConstraint.activate([
      header.topAnchor.constraint(equalTo: root.topAnchor),
      header.leadingAnchor.constraint(equalTo: root.leadingAnchor),
      header.trailingAnchor.constraint(equalTo: root.trailingAnchor),
      header.heightAnchor.constraint(equalToConstant: 86),
      heading.leadingAnchor.constraint(equalTo: header.leadingAnchor, constant: 22),
      heading.trailingAnchor.constraint(equalTo: header.trailingAnchor, constant: -22),
      heading.topAnchor.constraint(equalTo: header.topAnchor, constant: 15),
      subtitle.leadingAnchor.constraint(equalTo: heading.leadingAnchor),
      subtitle.trailingAnchor.constraint(equalTo: heading.trailingAnchor),
      subtitle.topAnchor.constraint(equalTo: heading.bottomAnchor, constant: 6),
      gridScroll.topAnchor.constraint(equalTo: header.bottomAnchor),
      gridScroll.leadingAnchor.constraint(equalTo: root.leadingAnchor),
      gridScroll.trailingAnchor.constraint(equalTo: root.trailingAnchor),
      gridScroll.bottomAnchor.constraint(equalTo: root.bottomAnchor),
      listScroll.topAnchor.constraint(equalTo: header.bottomAnchor),
      listScroll.leadingAnchor.constraint(equalTo: root.leadingAnchor),
      listScroll.trailingAnchor.constraint(equalTo: root.trailingAnchor),
      listScroll.bottomAnchor.constraint(equalTo: root.bottomAnchor),
      collectionView.widthAnchor.constraint(equalTo: gridScroll.contentView.widthAnchor),
    ])
    self.view = root

    searchField.placeholderString = "Search captions, dates, or shortcodes"
    searchField.delegate = self
    searchField.sendsSearchStringImmediately = true
    searchField.translatesAutoresizingMaskIntoConstraints = false
    searchField.widthAnchor.constraint(equalToConstant: 260).isActive = true

    modeControl.segmentCount = 2
    modeControl.segmentStyle = .texturedRounded
    modeControl.setImage(NSImage(systemSymbolName: "square.grid.3x3", accessibilityDescription: "Thumbnails"), forSegment: 0)
    modeControl.setImage(NSImage(systemSymbolName: "list.bullet", accessibilityDescription: "List"), forSegment: 1)
    modeControl.setToolTip("Tiny thumbnails", forSegment: 0)
    modeControl.setToolTip("Sortable list", forSegment: 1)
    modeControl.selectedSegment = min(1, max(0, UserDefaults.standard.integer(forKey: "ArchiveViewMode")))
    modeControl.target = self
    modeControl.action = #selector(changeViewMode)
    gridScroll.isHidden = modeControl.selectedSegment == 1
    listScroll.isHidden = modeControl.selectedSegment == 0

  }

  func installToolbar(on window: NSWindow) {
    let toolbar = NSToolbar(identifier: "ArchiveToolbar")
    toolbar.delegate = self
    toolbar.displayMode = .iconOnly
    toolbar.allowsUserCustomization = false
    window.toolbar = toolbar
    window.toolbarStyle = .unified
  }

  func showListView() {
    _ = view
    modeControl.selectedSegment = 1
    changeViewMode()
  }

  func numberOfSections(in collectionView: NSCollectionView) -> Int { 1 }
  func collectionView(_ collectionView: NSCollectionView, numberOfItemsInSection section: Int) -> Int { filteredPosts.count }

  func collectionView(_ collectionView: NSCollectionView, itemForRepresentedObjectAt indexPath: IndexPath) -> NSCollectionViewItem {
    let item = collectionView.makeItem(withIdentifier: ArchiveCardItem.identifier, for: indexPath) as! ArchiveCardItem
    item.configure(post: filteredPosts[indexPath.item], archiveRoot: archiveRoot)
    return item
  }

  func collectionView(_ collectionView: NSCollectionView, layout collectionViewLayout: NSCollectionViewLayout,
                      sizeForItemAt indexPath: IndexPath) -> NSSize {
    let available = max(520, collectionView.bounds.width - 32)
    let columns = max(3, Int((available + 10) / 190))
    let width = floor((available - CGFloat(columns - 1) * 10) / CGFloat(columns))
    return NSSize(width: width, height: width + 88)
  }

  func collectionView(_ collectionView: NSCollectionView, didSelectItemsAt indexPaths: Set<IndexPath>) {
    shareItem.isEnabled = !indexPaths.isEmpty
  }

  func collectionView(_ collectionView: NSCollectionView, didDeselectItemsAt indexPaths: Set<IndexPath>) {
    shareItem.isEnabled = !collectionView.selectionIndexPaths.isEmpty
  }

  func numberOfRows(in tableView: NSTableView) -> Int { filteredPosts.count }

  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    guard filteredPosts.indices.contains(row), let column = tableColumn else { return nil }
    let post = filteredPosts[row]
    let id = column.identifier
    if id.rawValue == "thumbnail" {
      let cell = (tableView.makeView(withIdentifier: id, owner: self) as? ThumbnailTableCell) ?? {
        let created = ThumbnailTableCell()
        created.identifier = id
        return created
      }()
      let url = post.stills.first?.file.map { archiveRoot.appendingPathComponent($0) }
      cell.configure(url: url)
      return cell
    }
    let cell = (tableView.makeView(withIdentifier: id, owner: self) as? NSTableCellView) ?? {
      let created = NSTableCellView()
      created.identifier = id
      let label = NSTextField(labelWithString: "")
      label.translatesAutoresizingMaskIntoConstraints = false
      label.lineBreakMode = .byTruncatingTail
      label.maximumNumberOfLines = 2
      created.textField = label
      created.addSubview(label)
      NSLayoutConstraint.activate([
        label.leadingAnchor.constraint(equalTo: created.leadingAnchor, constant: 4),
        label.trailingAnchor.constraint(equalTo: created.trailingAnchor, constant: -4),
        label.centerYAnchor.constraint(equalTo: created.centerYAnchor),
      ])
      return created
    }()
    switch id.rawValue {
    case "date":
      cell.textField?.stringValue = String(post.date.prefix(10))
      cell.textField?.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular)
      cell.textField?.textColor = .secondaryLabelColor
    case "caption":
      cell.textField?.stringValue = post.caption.isEmpty ? "No caption" : post.caption
      cell.textField?.font = .systemFont(ofSize: 13)
      cell.textField?.textColor = post.caption.isEmpty ? .secondaryLabelColor : .labelColor
    case "images":
      cell.textField?.stringValue = String(post.stills.count)
      cell.textField?.alignment = .center
      cell.textField?.font = .monospacedDigitSystemFont(ofSize: 12, weight: .medium)
      cell.textField?.textColor = .secondaryLabelColor
    default:
      cell.textField?.stringValue = post.shortcode
      cell.textField?.font = .monospacedSystemFont(ofSize: 11, weight: .regular)
      cell.textField?.textColor = .secondaryLabelColor
    }
    return cell
  }

  func tableViewSelectionDidChange(_ notification: Notification) {
    shareItem.isEnabled = tableView.selectedRow >= 0
  }

  func tableView(_ tableView: NSTableView, sortDescriptorsDidChange oldDescriptors: [NSSortDescriptor]) {
    guard let descriptor = tableView.sortDescriptors.first, let key = descriptor.key else { return }
    sortColumn = key
    sortAscending = descriptor.ascending
    applyFilterAndSort()
  }

  func controlTextDidChange(_ obj: Notification) {
    applyFilterAndSort()
  }

  private func applyFilterAndSort() {
    let needle = searchField.stringValue.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
    let matches = needle.isEmpty ? manifest.posts : manifest.posts.filter {
      $0.shortcode.lowercased().contains(needle) || $0.date.lowercased().contains(needle) || $0.caption.lowercased().contains(needle)
    }
    filteredPosts = matches.sorted { left, right in
      let comparison: ComparisonResult
      switch sortColumn {
      case "caption": comparison = left.caption.localizedCaseInsensitiveCompare(right.caption)
      case "images":
        if left.stills.count == right.stills.count { comparison = left.shortcode.compare(right.shortcode) }
        else { comparison = left.stills.count < right.stills.count ? .orderedAscending : .orderedDescending }
      case "shortcode": comparison = left.shortcode.compare(right.shortcode)
      default: comparison = left.date.compare(right.date)
      }
      if comparison == .orderedSame { return left.shortcode < right.shortcode }
      return sortAscending ? comparison == .orderedAscending : comparison == .orderedDescending
    }
    collectionView.selectionIndexPaths = []
    tableView.deselectAll(nil)
    shareItem.isEnabled = false
    collectionView.reloadData()
    collectionView.collectionViewLayout?.invalidateLayout()
    tableView.reloadData()
    updateSubtitle()
  }

  @objc private func changeViewMode() {
    let list = modeControl.selectedSegment == 1
    UserDefaults.standard.set(modeControl.selectedSegment, forKey: "ArchiveViewMode")
    gridScroll.isHidden = list
    listScroll.isHidden = !list
    collectionView.selectionIndexPaths = []
    tableView.deselectAll(nil)
    shareItem.isEnabled = false
    if !list { collectionView.collectionViewLayout?.invalidateLayout() }
  }

  private func updateSubtitle() {
    let visibleStills = filteredPosts.reduce(0) { $0 + $1.stills.count }
    subtitle.stringValue = "@\(manifest.account)   ·   \(filteredPosts.count) posts   ·   \(visibleStills) stills   ·   offline archive"
  }

  private func selectedPost() -> ArchivePost? {
    let index = modeControl.selectedSegment == 1 ? tableView.selectedRow : (collectionView.selectionIndexPaths.first?.item ?? -1)
    guard filteredPosts.indices.contains(index) else { return nil }
    return filteredPosts[index]
  }

  private func urls(for post: ArchivePost) -> [URL] {
    post.stills.compactMap { $0.file }.map { archiveRoot.appendingPathComponent($0) }
  }

  @objc private func openSelection() {
    guard let post = selectedPost() else { return }
    previewURLs = urls(for: post)
    guard !previewURLs.isEmpty, let panel = QLPreviewPanel.shared() else { return }
    panel.dataSource = self
    panel.currentPreviewItemIndex = 0
    panel.makeKeyAndOrderFront(nil)
  }

  @objc private func handleDoubleClick(_ recognizer: NSClickGestureRecognizer) {
    let point = recognizer.location(in: collectionView)
    if let indexPath = collectionView.indexPathForItem(at: point) {
      collectionView.selectItems(at: [indexPath], scrollPosition: [])
      shareItem.isEnabled = true
    }
    openSelection()
  }

  func numberOfPreviewItems(in panel: QLPreviewPanel!) -> Int { previewURLs.count }
  func previewPanel(_ panel: QLPreviewPanel!, previewItemAt index: Int) -> QLPreviewItem! { previewURLs[index] as NSURL }

  func items(for pickerToolbarItem: NSSharingServicePickerToolbarItem) -> [Any] {
    guard let post = selectedPost() else { return [] }
    var items: [Any] = urls(for: post)
    if !post.caption.isEmpty { items.append(post.caption as NSString) }
    if let source = URL(string: post.url) { items.append(source as NSURL) }
    return items
  }

  func toolbarAllowedItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    [Self.searchID, .flexibleSpace, Self.modeID, Self.shareID]
  }

  func toolbarDefaultItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    [Self.searchID, .flexibleSpace, Self.modeID, Self.shareID]
  }

  func toolbar(_ toolbar: NSToolbar, itemForItemIdentifier itemIdentifier: NSToolbarItem.Identifier,
               willBeInsertedIntoToolbar flag: Bool) -> NSToolbarItem? {
    if itemIdentifier == Self.searchID {
      let item = NSToolbarItem(itemIdentifier: itemIdentifier)
      item.label = "Search"
      item.view = searchField
      return item
    }
    if itemIdentifier == Self.shareID {
      return shareItem
    }
    if itemIdentifier == Self.modeID {
      let item = NSToolbarItem(itemIdentifier: itemIdentifier)
      item.label = "View"
      item.paletteLabel = "View mode"
      item.view = modeControl
      return item
    }
    return nil
  }
}

final class AppDelegate: NSObject, NSApplicationDelegate {
  private var window: NSWindow?
  private var controller: GalleryController?

  func applicationDidFinishLaunching(_ notification: Notification) {
    do {
      guard let resources = Bundle.main.resourceURL else { throw CocoaError(.fileNoSuchFile) }
      let archiveRoot = resources.appendingPathComponent("archive", isDirectory: true)
      let data = try Data(contentsOf: archiveRoot.appendingPathComponent("manifest.json"))
      let manifest = try JSONDecoder().decode(ArchiveManifest.self, from: data)
      let controller = GalleryController(manifest: manifest, archiveRoot: archiveRoot)
      installMainMenu()
      let window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 1240, height: 860),
                            styleMask: [.titled, .closable, .miniaturizable, .resizable],
                            backing: .buffered, defer: false)
      window.title = manifest.title.components(separatedBy: " —").first ?? manifest.title
      window.titleVisibility = .hidden
      window.isRestorable = false
      window.minSize = NSSize(width: 720, height: 560)
      window.contentViewController = controller
      controller.installToolbar(on: window)
      if CommandLine.arguments.contains("--list") { controller.showListView() }
      window.center()
      window.makeKeyAndOrderFront(nil)
      NSApp.activate(ignoringOtherApps: true)
      self.controller = controller
      self.window = window
    } catch {
      let alert = NSAlert(error: error)
      alert.messageText = "The archive could not be opened"
      alert.runModal()
      NSApp.terminate(nil)
    }
  }

  func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }

  private func installMainMenu() {
    let main = NSMenu()

    let appItem = NSMenuItem()
    let appMenu = NSMenu()
    appMenu.addItem(withTitle: "About \(ProcessInfo.processInfo.processName)", action: #selector(NSApplication.orderFrontStandardAboutPanel(_:)), keyEquivalent: "")
    appMenu.addItem(.separator())
    appMenu.addItem(withTitle: "Hide \(ProcessInfo.processInfo.processName)", action: #selector(NSApplication.hide(_:)), keyEquivalent: "h")
    appMenu.addItem(withTitle: "Hide Others", action: #selector(NSApplication.hideOtherApplications(_:)), keyEquivalent: "h").keyEquivalentModifierMask = [.command, .option]
    appMenu.addItem(withTitle: "Show All", action: #selector(NSApplication.unhideAllApplications(_:)), keyEquivalent: "")
    appMenu.addItem(.separator())
    appMenu.addItem(withTitle: "Quit \(ProcessInfo.processInfo.processName)", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
    appItem.submenu = appMenu
    main.addItem(appItem)

    let fileItem = NSMenuItem()
    let fileMenu = NSMenu(title: "File")
    fileMenu.addItem(withTitle: "Close", action: #selector(NSWindow.performClose(_:)), keyEquivalent: "w")
    fileItem.submenu = fileMenu
    main.addItem(fileItem)

    let editItem = NSMenuItem()
    let editMenu = NSMenu(title: "Edit")
    editMenu.addItem(withTitle: "Undo", action: Selector(("undo:")), keyEquivalent: "z")
    editMenu.addItem(withTitle: "Redo", action: Selector(("redo:")), keyEquivalent: "Z")
    editMenu.addItem(.separator())
    editMenu.addItem(withTitle: "Cut", action: #selector(NSText.cut(_:)), keyEquivalent: "x")
    editMenu.addItem(withTitle: "Copy", action: #selector(NSText.copy(_:)), keyEquivalent: "c")
    editMenu.addItem(withTitle: "Paste", action: #selector(NSText.paste(_:)), keyEquivalent: "v")
    editMenu.addItem(withTitle: "Select All", action: #selector(NSText.selectAll(_:)), keyEquivalent: "a")
    editItem.submenu = editMenu
    main.addItem(editItem)

    let windowItem = NSMenuItem()
    let windowMenu = NSMenu(title: "Window")
    windowMenu.addItem(withTitle: "Minimize", action: #selector(NSWindow.performMiniaturize(_:)), keyEquivalent: "m")
    windowMenu.addItem(withTitle: "Zoom", action: #selector(NSWindow.performZoom(_:)), keyEquivalent: "")
    windowMenu.addItem(.separator())
    windowMenu.addItem(withTitle: "Bring All to Front", action: #selector(NSApplication.arrangeInFront(_:)), keyEquivalent: "")
    windowItem.submenu = windowMenu
    main.addItem(windowItem)
    NSApp.windowsMenu = windowMenu
    NSApp.mainMenu = main
  }
}

let app = NSApplication.shared
let delegate = AppDelegate()
app.setActivationPolicy(.regular)
app.delegate = delegate
app.run()
