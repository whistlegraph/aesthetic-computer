import AppKit
import ImageIO
import Quartz

struct ArchiveManifest: Decodable {
  struct Counts: Decodable {
    let posts: Int
    let stills: Int
    let videos: Int?
    let taggedPosts: Int?
    let taggedStills: Int?
    let taggedVideos: Int?
    let followers: Int?
  }
  let title: String
  let account: String
  let counts: Counts
  let posts: [ArchivePost]
  let taggedPosts: [ArchivePost]?
  let followers: [ArchiveFollower]?
}

struct ArchivePost: Decodable {
  let shortcode: String
  let url: String
  let date: String
  let caption: String
  let author: String?
  let stills: [ArchiveStill]
  let videos: [ArchiveVideo]?
}

struct ArchiveStill: Decodable {
  let ordinal: Int?
  let file: String?
  let alt: String?
}

struct ArchiveVideo: Decodable {
  let ordinal: Int
  let file: String?
  let duration: Double?
}

struct ArchiveFollower: Decodable {
  let ordinal: Int?
  let username: String
  let fullName: String
  let `private`: Bool
  let verified: Bool
}

final class ThumbnailLoader {
  static let shared = ThumbnailLoader()
  private let cache = NSCache<NSString, NSImage>()
  private let queue = DispatchQueue(label: "computer.aesthetic.dmgify.thumbnails", qos: .userInitiated, attributes: .concurrent)

  private init() {
    cache.countLimit = 128
    cache.totalCostLimit = 224 * 1024 * 1024
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
      self.cache.setObject(image, forKey: key, cost: cgImage.bytesPerRow * cgImage.height)
      DispatchQueue.main.async { completion(image) }
    }
  }
}

final class ResponsiveFlowLayout: NSCollectionViewFlowLayout {
  override func shouldInvalidateLayout(forBoundsChange newBounds: NSRect) -> Bool { true }
}

final class CardSurface: NSView {
  var selected = false { didSet { updateStyle() } }
  var onHover: ((Bool) -> Void)?
  var onSwipe: ((Int) -> Void)?
  private var hovered = false
  private var tracking: NSTrackingArea?
  private var horizontalDelta: CGFloat = 0

  override init(frame frameRect: NSRect) {
    super.init(frame: frameRect)
    wantsLayer = true
    layer?.cornerRadius = 12
    layer?.masksToBounds = true
    layer?.backgroundColor = NSColor.controlBackgroundColor.cgColor
    updateStyle()
  }

  required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

  override func updateTrackingAreas() {
    super.updateTrackingAreas()
    if let tracking { removeTrackingArea(tracking) }
    let area = NSTrackingArea(rect: .zero,
      options: [.mouseEnteredAndExited, .activeInKeyWindow, .inVisibleRect, .cursorUpdate], owner: self)
    addTrackingArea(area)
    tracking = area
  }

  override func mouseEntered(with event: NSEvent) { hovered = true; updateStyle(); onHover?(true) }
  override func mouseExited(with event: NSEvent) { hovered = false; updateStyle(); onHover?(false) }
  override func cursorUpdate(with event: NSEvent) { NSCursor.pointingHand.set() }

  override func scrollWheel(with event: NSEvent) {
    guard abs(event.scrollingDeltaX) > abs(event.scrollingDeltaY), abs(event.scrollingDeltaX) > 0.5 else {
      super.scrollWheel(with: event)
      return
    }
    horizontalDelta += event.scrollingDeltaX
    if event.phase == .ended || event.momentumPhase == .began || abs(horizontalDelta) >= 36 {
      if abs(horizontalDelta) >= 18 { onSwipe?(horizontalDelta > 0 ? 1 : -1) }
      horizontalDelta = 0
    }
  }

  private func updateStyle() {
    layer?.borderWidth = selected ? 3 : (hovered ? 2 : 1)
    layer?.borderColor = (selected ? NSColor.controlAccentColor :
      (hovered ? NSColor.controlAccentColor.withAlphaComponent(0.72) : NSColor.separatorColor)).cgColor
  }
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
      picture.centerXAnchor.constraint(equalTo: centerXAnchor), picture.centerYAnchor.constraint(equalTo: centerYAnchor),
      picture.widthAnchor.constraint(equalToConstant: 56), picture.heightAnchor.constraint(equalToConstant: 56),
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
  private let videoLabel = NSTextField(labelWithString: "▶ Video")
  private let previousButton = NSButton()
  private let nextButton = NSButton()
  private var surface: CardSurface?
  private var representedURL: URL?
  private var post: ArchivePost?
  private var archiveRoot: URL?
  private var thumbnailRoot: URL?
  private var currentIndex = 0
  private var dense = false

  override var isSelected: Bool { didSet { updateSelection() } }

  override func loadView() {
    let card = CardSurface()
    surface = card
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
    captionLabel.maximumNumberOfLines = 0
    captionLabel.lineBreakMode = .byWordWrapping

    for label in [countLabel, videoLabel] {
      label.translatesAutoresizingMaskIntoConstraints = false
      label.font = .systemFont(ofSize: 11, weight: .semibold)
      label.textColor = .white
      label.alignment = .center
      label.wantsLayer = true
      label.layer?.backgroundColor = NSColor.black.withAlphaComponent(0.72).cgColor
      label.layer?.cornerRadius = 10
    }

    for (button, symbol, tip) in [(previousButton, "chevron.left", "Previous image"), (nextButton, "chevron.right", "Next image")] {
      button.translatesAutoresizingMaskIntoConstraints = false
      button.bezelStyle = .circular
      button.image = NSImage(systemSymbolName: symbol, accessibilityDescription: tip)
      button.title = ""
      button.toolTip = tip
      button.isBordered = true
      button.alphaValue = 0
    }
    previousButton.target = self; previousButton.action = #selector(showPrevious)
    nextButton.target = self; nextButton.action = #selector(showNext)

    card.addSubview(picture); card.addSubview(dateLabel); card.addSubview(captionLabel)
    card.addSubview(countLabel); card.addSubview(videoLabel); card.addSubview(previousButton); card.addSubview(nextButton)
    NSLayoutConstraint.activate([
      picture.topAnchor.constraint(equalTo: card.topAnchor), picture.leadingAnchor.constraint(equalTo: card.leadingAnchor),
      picture.trailingAnchor.constraint(equalTo: card.trailingAnchor), picture.heightAnchor.constraint(equalTo: card.widthAnchor),
      dateLabel.topAnchor.constraint(equalTo: picture.bottomAnchor, constant: 12),
      dateLabel.leadingAnchor.constraint(equalTo: card.leadingAnchor, constant: 13),
      dateLabel.trailingAnchor.constraint(equalTo: card.trailingAnchor, constant: -13),
      captionLabel.topAnchor.constraint(equalTo: dateLabel.bottomAnchor, constant: 7),
      captionLabel.leadingAnchor.constraint(equalTo: card.leadingAnchor, constant: 13),
      captionLabel.trailingAnchor.constraint(equalTo: card.trailingAnchor, constant: -13),
      captionLabel.bottomAnchor.constraint(lessThanOrEqualTo: card.bottomAnchor, constant: -13),
      countLabel.topAnchor.constraint(equalTo: picture.topAnchor, constant: 10),
      countLabel.trailingAnchor.constraint(equalTo: picture.trailingAnchor, constant: -10),
      countLabel.heightAnchor.constraint(equalToConstant: 20), countLabel.widthAnchor.constraint(greaterThanOrEqualToConstant: 38),
      videoLabel.topAnchor.constraint(equalTo: picture.topAnchor, constant: 10),
      videoLabel.leadingAnchor.constraint(equalTo: picture.leadingAnchor, constant: 10),
      videoLabel.heightAnchor.constraint(equalToConstant: 20), videoLabel.widthAnchor.constraint(greaterThanOrEqualToConstant: 60),
      previousButton.leadingAnchor.constraint(equalTo: picture.leadingAnchor, constant: 9), previousButton.centerYAnchor.constraint(equalTo: picture.centerYAnchor),
      previousButton.widthAnchor.constraint(equalToConstant: 30), previousButton.heightAnchor.constraint(equalToConstant: 30),
      nextButton.trailingAnchor.constraint(equalTo: picture.trailingAnchor, constant: -9), nextButton.centerYAnchor.constraint(equalTo: picture.centerYAnchor),
      nextButton.widthAnchor.constraint(equalToConstant: 30), nextButton.heightAnchor.constraint(equalToConstant: 30),
    ])
    card.onHover = { [weak self] inside in self?.setControlsVisible(inside) }
    card.onSwipe = { [weak self] direction in direction > 0 ? self?.showPrevious() : self?.showNext() }
    self.view = card
    updateSelection()
  }

  override func prepareForReuse() {
    super.prepareForReuse()
    representedURL = nil; picture.image = nil; post = nil; currentIndex = 0
    dateLabel.stringValue = ""; captionLabel.stringValue = ""; countLabel.isHidden = true; videoLabel.isHidden = true
    surface?.selected = false
  }

  func configure(post: ArchivePost, archiveRoot: URL, thumbnailRoot: URL, dense: Bool) {
    self.post = post; self.archiveRoot = archiveRoot; self.thumbnailRoot = thumbnailRoot; self.dense = dense; currentIndex = 0
    let day = String(post.date.prefix(10))
    let author = (post.author?.isEmpty == false) ? "@\(post.author!)  ·  " : ""
    dateLabel.stringValue = "\(author)\(day)  ·  \(post.shortcode)"
    captionLabel.stringValue = post.caption.isEmpty ? "No caption" : post.caption
    captionLabel.textColor = post.caption.isEmpty ? .secondaryLabelColor : .labelColor
    dateLabel.isHidden = dense; captionLabel.isHidden = dense
    updateSelection()
    updateImage()
  }

  @objc private func showPrevious() { step(-1) }
  @objc private func showNext() { step(1) }

  private func step(_ delta: Int) {
    guard let post, post.stills.count > 1 else { return }
    currentIndex = (currentIndex + delta + post.stills.count) % post.stills.count
    updateImage()
  }

  private func updateImage() {
    guard let post, post.stills.indices.contains(currentIndex), let archiveRoot else { return }
    let still = post.stills[currentIndex]
    let original = still.file.map { archiveRoot.appendingPathComponent($0) }
    var url = original
    if currentIndex == 0, let file = still.file, let thumbnailRoot {
      let thumb = thumbnailRoot.appendingPathComponent(file + ".jpg")
      if FileManager.default.fileExists(atPath: thumb.path) { url = thumb }
    }
    countLabel.isHidden = post.stills.count < 2
    countLabel.stringValue = " \(currentIndex + 1) / \(post.stills.count) "
    videoLabel.isHidden = !(post.videos ?? []).contains { $0.ordinal == (still.ordinal ?? currentIndex + 1) }
    guard let url else { picture.image = nil; return }
    representedURL = url; picture.image = nil
    ThumbnailLoader.shared.load(url, maximumPixelSize: dense ? 320 : 700) { [weak self] image in
      guard let self, self.representedURL == url else { return }
      self.picture.image = image
    }
  }

  private func setControlsVisible(_ visible: Bool) {
    let hasMany = (post?.stills.count ?? 0) > 1
    previousButton.alphaValue = visible && hasMany ? 1 : 0
    nextButton.alphaValue = visible && hasMany ? 1 : 0
  }

  private func updateSelection() {
    guard isViewLoaded else { return }
    surface?.selected = isSelected
  }
}

final class GalleryController: NSViewController, NSCollectionViewDataSource,
  NSCollectionViewDelegate, NSCollectionViewDelegateFlowLayout, NSSearchFieldDelegate,
  NSTableViewDataSource, NSTableViewDelegate, NSToolbarDelegate,
  NSSharingServicePickerToolbarItemDelegate, NSSharingServiceDelegate, QLPreviewPanelDataSource {

  private enum Section: Int { case posts, tagged, followers }
  private static let searchID = NSToolbarItem.Identifier("ArchiveSearch")
  private static let sectionID = NSToolbarItem.Identifier("ArchiveSection")
  private static let modeID = NSToolbarItem.Identifier("ArchiveViewMode")
  private static let shareID = NSToolbarItem.Identifier("ArchiveShare")
  private let manifest: ArchiveManifest
  private let archiveRoot: URL
  private let thumbnailRoot: URL
  private var filteredPosts: [ArchivePost]
  private var filteredFollowers: [ArchiveFollower] = []
  private let collectionView = NSCollectionView()
  private let tableView = NSTableView()
  private let gridScroll = NSScrollView()
  private let listScroll = NSScrollView()
  private let searchField = NSSearchField()
  private let sectionControl = NSSegmentedControl(labels: ["Posts", "Tagged", "Followers"], trackingMode: .selectOne, target: nil, action: nil)
  private let modeControl = NSSegmentedControl(labels: ["Grid", "Thumbnails", "List"], trackingMode: .selectOne, target: nil, action: nil)
  private var section: Section = .posts
  private var sortColumn = "date"
  private var sortAscending = false
  private var lastPostMode = 0
  private lazy var shareItem: NSSharingServicePickerToolbarItem = {
    let item = NSSharingServicePickerToolbarItem(itemIdentifier: Self.shareID)
    item.label = "Share"
    item.paletteLabel = "Share selected post"
    item.toolTip = "Share originals, original caption, and Instagram URL"
    item.delegate = self; item.isEnabled = false
    return item
  }()
  private var previewURLs: [URL] = []
  private let subtitle = NSTextField(labelWithString: "")

  init(manifest: ArchiveManifest, archiveRoot: URL) {
    self.manifest = manifest; self.archiveRoot = archiveRoot
    self.thumbnailRoot = archiveRoot.deletingLastPathComponent().appendingPathComponent("thumbnails", isDirectory: true)
    self.filteredPosts = manifest.posts
    self.filteredFollowers = manifest.followers ?? []
    super.init(nibName: nil, bundle: nil)
  }

  required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

  override func loadView() {
    let root = NSView(); root.wantsLayer = true; root.layer?.backgroundColor = NSColor.windowBackgroundColor.cgColor
    let heading = NSTextField(labelWithString: manifest.title.components(separatedBy: " —").first ?? manifest.title)
    heading.translatesAutoresizingMaskIntoConstraints = false; heading.font = .systemFont(ofSize: 29, weight: .bold)
    heading.lineBreakMode = .byTruncatingTail
    subtitle.translatesAutoresizingMaskIntoConstraints = false; subtitle.font = .systemFont(ofSize: 13); subtitle.textColor = .secondaryLabelColor
    updateSubtitle()
    let header = NSView(); header.translatesAutoresizingMaskIntoConstraints = false; header.addSubview(heading); header.addSubview(subtitle)

    let layout = ResponsiveFlowLayout(); layout.minimumInteritemSpacing = 10; layout.minimumLineSpacing = 10
    layout.sectionInset = NSEdgeInsets(top: 14, left: 16, bottom: 20, right: 16)
    collectionView.collectionViewLayout = layout; collectionView.translatesAutoresizingMaskIntoConstraints = false
    collectionView.backgroundColors = [.windowBackgroundColor]; collectionView.dataSource = self; collectionView.delegate = self
    collectionView.isSelectable = true; collectionView.allowsMultipleSelection = false
    collectionView.register(ArchiveCardItem.self, forItemWithIdentifier: ArchiveCardItem.identifier)
    let doubleClick = NSClickGestureRecognizer(target: self, action: #selector(handleDoubleClick(_:)))
    doubleClick.numberOfClicksRequired = 2; doubleClick.delaysPrimaryMouseButtonEvents = false; collectionView.addGestureRecognizer(doubleClick)
    gridScroll.translatesAutoresizingMaskIntoConstraints = false; gridScroll.documentView = collectionView
    gridScroll.hasVerticalScroller = true; gridScroll.autohidesScrollers = true; gridScroll.drawsBackground = false

    configurePostColumns()
    tableView.delegate = self; tableView.dataSource = self; tableView.rowHeight = 68
    tableView.intercellSpacing = NSSize(width: 10, height: 2); tableView.usesAlternatingRowBackgroundColors = true
    tableView.allowsMultipleSelection = false; tableView.allowsEmptySelection = true
    tableView.columnAutoresizingStyle = .lastColumnOnlyAutoresizingStyle
    tableView.target = self; tableView.doubleAction = #selector(openSelection)
    tableView.sortDescriptors = [NSSortDescriptor(key: "date", ascending: false)]
    listScroll.translatesAutoresizingMaskIntoConstraints = false; listScroll.documentView = tableView
    listScroll.hasVerticalScroller = true; listScroll.autohidesScrollers = true; listScroll.drawsBackground = false; listScroll.isHidden = true

    root.addSubview(header); root.addSubview(gridScroll); root.addSubview(listScroll)
    NSLayoutConstraint.activate([
      header.topAnchor.constraint(equalTo: root.topAnchor), header.leadingAnchor.constraint(equalTo: root.leadingAnchor),
      header.trailingAnchor.constraint(equalTo: root.trailingAnchor), header.heightAnchor.constraint(equalToConstant: 86),
      heading.leadingAnchor.constraint(equalTo: header.leadingAnchor, constant: 22), heading.trailingAnchor.constraint(equalTo: header.trailingAnchor, constant: -22),
      heading.topAnchor.constraint(equalTo: header.topAnchor, constant: 15),
      subtitle.leadingAnchor.constraint(equalTo: heading.leadingAnchor), subtitle.trailingAnchor.constraint(equalTo: heading.trailingAnchor),
      subtitle.topAnchor.constraint(equalTo: heading.bottomAnchor, constant: 6),
      gridScroll.topAnchor.constraint(equalTo: header.bottomAnchor), gridScroll.leadingAnchor.constraint(equalTo: root.leadingAnchor),
      gridScroll.trailingAnchor.constraint(equalTo: root.trailingAnchor), gridScroll.bottomAnchor.constraint(equalTo: root.bottomAnchor),
      listScroll.topAnchor.constraint(equalTo: header.bottomAnchor), listScroll.leadingAnchor.constraint(equalTo: root.leadingAnchor),
      listScroll.trailingAnchor.constraint(equalTo: root.trailingAnchor), listScroll.bottomAnchor.constraint(equalTo: root.bottomAnchor),
      collectionView.widthAnchor.constraint(equalTo: gridScroll.contentView.widthAnchor),
    ])
    self.view = root

    searchField.placeholderString = "Search this view"; searchField.delegate = self
    searchField.sendsSearchStringImmediately = true; searchField.translatesAutoresizingMaskIntoConstraints = false
    searchField.widthAnchor.constraint(equalToConstant: 210).isActive = true
    sectionControl.selectedSegment = 0; sectionControl.target = self; sectionControl.action = #selector(changeSection)
    modeControl.selectedSegment = min(2, max(0, UserDefaults.standard.integer(forKey: "ArchiveViewModeV2")))
    lastPostMode = modeControl.selectedSegment; modeControl.target = self; modeControl.action = #selector(changeViewMode)
    updateVisibleMode()
  }

  func installToolbar(on window: NSWindow) {
    let toolbar = NSToolbar(identifier: "ArchiveToolbar"); toolbar.delegate = self
    toolbar.displayMode = .iconAndLabel; toolbar.allowsUserCustomization = false
    window.toolbar = toolbar; window.toolbarStyle = .unified
  }

  func showListView() { _ = view; modeControl.selectedSegment = 2; changeViewMode() }

  func numberOfSections(in collectionView: NSCollectionView) -> Int { 1 }
  func collectionView(_ collectionView: NSCollectionView, numberOfItemsInSection section: Int) -> Int { filteredPosts.count }
  func collectionView(_ collectionView: NSCollectionView, itemForRepresentedObjectAt indexPath: IndexPath) -> NSCollectionViewItem {
    let item = collectionView.makeItem(withIdentifier: ArchiveCardItem.identifier, for: indexPath) as! ArchiveCardItem
    item.configure(post: filteredPosts[indexPath.item], archiveRoot: archiveRoot, thumbnailRoot: thumbnailRoot, dense: modeControl.selectedSegment == 1)
    return item
  }

  func collectionView(_ collectionView: NSCollectionView, layout collectionViewLayout: NSCollectionViewLayout,
                      sizeForItemAt indexPath: IndexPath) -> NSSize {
    let available = max(300, collectionView.bounds.width - 32)
    if modeControl.selectedSegment == 1 {
      let columns = max(2, Int((available + 10) / 150))
      let width = floor((available - CGFloat(columns - 1) * 10) / CGFloat(columns))
      return NSSize(width: width, height: width)
    }
    let columns = max(1, Int((available + 10) / 310))
    let width = floor((available - CGFloat(columns - 1) * 10) / CGFloat(columns))
    let post = filteredPosts[indexPath.item]
    let text = post.caption.isEmpty ? "No caption" : post.caption
    let bounds = (text as NSString).boundingRect(with: NSSize(width: max(80, width - 26), height: .greatestFiniteMagnitude),
      options: [.usesLineFragmentOrigin, .usesFontLeading], attributes: [.font: NSFont.systemFont(ofSize: 12)])
    return NSSize(width: width, height: width + 12 + 14 + 7 + ceil(bounds.height) + 14)
  }

  func collectionView(_ collectionView: NSCollectionView, didSelectItemsAt indexPaths: Set<IndexPath>) { updateShareEnabled() }
  func collectionView(_ collectionView: NSCollectionView, didDeselectItemsAt indexPaths: Set<IndexPath>) { updateShareEnabled() }
  func numberOfRows(in tableView: NSTableView) -> Int { section == .followers ? filteredFollowers.count : filteredPosts.count }

  func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
    guard let column = tableColumn else { return nil }
    let id = column.identifier
    if section != .followers, filteredPosts.indices.contains(row), id.rawValue == "thumbnail" {
      let cell = (tableView.makeView(withIdentifier: id, owner: self) as? ThumbnailTableCell) ?? {
        let created = ThumbnailTableCell(); created.identifier = id; return created
      }()
      cell.configure(url: thumbnailURL(for: filteredPosts[row])); return cell
    }
    let cell = (tableView.makeView(withIdentifier: id, owner: self) as? NSTableCellView) ?? {
      let created = NSTableCellView(); created.identifier = id
      let label = NSTextField(labelWithString: ""); label.translatesAutoresizingMaskIntoConstraints = false
      label.lineBreakMode = .byTruncatingTail; label.maximumNumberOfLines = 2
      created.textField = label; created.addSubview(label)
      NSLayoutConstraint.activate([label.leadingAnchor.constraint(equalTo: created.leadingAnchor, constant: 4),
        label.trailingAnchor.constraint(equalTo: created.trailingAnchor, constant: -4), label.centerYAnchor.constraint(equalTo: created.centerYAnchor)])
      return created
    }()
    guard let label = cell.textField else { return cell }
    label.alignment = .left; label.font = .systemFont(ofSize: 13); label.textColor = .labelColor
    if section == .followers {
      guard filteredFollowers.indices.contains(row) else { return nil }
      let follower = filteredFollowers[row]
      switch id.rawValue {
      case "username": label.stringValue = "@\(follower.username)"
      case "name": label.stringValue = follower.fullName
      case "private": label.stringValue = follower.private ? "Yes" : "No"; label.alignment = .center
      default: label.stringValue = follower.verified ? "Yes" : "No"; label.alignment = .center
      }
      return cell
    }
    guard filteredPosts.indices.contains(row) else { return nil }
    let post = filteredPosts[row]
    switch id.rawValue {
    case "date": label.stringValue = String(post.date.prefix(10)); label.font = .monospacedDigitSystemFont(ofSize: 12, weight: .regular); label.textColor = .secondaryLabelColor
    case "caption": label.stringValue = post.caption.isEmpty ? "No caption" : post.caption; label.textColor = post.caption.isEmpty ? .secondaryLabelColor : .labelColor
    case "images": label.stringValue = String(post.stills.count); label.alignment = .center; label.font = .monospacedDigitSystemFont(ofSize: 12, weight: .medium); label.textColor = .secondaryLabelColor
    case "videos": label.stringValue = String((post.videos ?? []).count); label.alignment = .center; label.font = .monospacedDigitSystemFont(ofSize: 12, weight: .medium); label.textColor = .secondaryLabelColor
    case "author": label.stringValue = post.author.map { "@\($0)" } ?? ""; label.textColor = .secondaryLabelColor
    default: label.stringValue = post.shortcode; label.font = .monospacedSystemFont(ofSize: 11, weight: .regular); label.textColor = .secondaryLabelColor
    }
    return cell
  }

  func tableViewSelectionDidChange(_ notification: Notification) { updateShareEnabled() }
  func tableView(_ tableView: NSTableView, sortDescriptorsDidChange oldDescriptors: [NSSortDescriptor]) {
    guard let descriptor = tableView.sortDescriptors.first, let key = descriptor.key else { return }
    sortColumn = key; sortAscending = descriptor.ascending; applyFilterAndSort()
  }
  func controlTextDidChange(_ obj: Notification) { applyFilterAndSort() }

  @objc private func changeSection() {
    section = Section(rawValue: sectionControl.selectedSegment) ?? .posts
    searchField.stringValue = ""; collectionView.selectionIndexPaths = []; tableView.deselectAll(nil)
    if section == .followers {
      if modeControl.selectedSegment != 2 { lastPostMode = modeControl.selectedSegment }
      modeControl.selectedSegment = 2; modeControl.isEnabled = false; configureFollowerColumns()
    } else {
      modeControl.isEnabled = true
      if modeControl.selectedSegment == 2 && lastPostMode != 2 { modeControl.selectedSegment = lastPostMode }
      configurePostColumns()
    }
    applyFilterAndSort(); updateVisibleMode()
  }

  @objc private func changeViewMode() {
    if section != .followers { lastPostMode = modeControl.selectedSegment; UserDefaults.standard.set(lastPostMode, forKey: "ArchiveViewModeV2") }
    collectionView.selectionIndexPaths = []; tableView.deselectAll(nil); updateShareEnabled(); updateVisibleMode()
  }

  private func updateVisibleMode() {
    let list = modeControl.selectedSegment == 2 || section == .followers
    gridScroll.isHidden = list; listScroll.isHidden = !list
    if !list { collectionView.reloadData(); collectionView.collectionViewLayout?.invalidateLayout() }
  }

  private func applyFilterAndSort() {
    let needle = searchField.stringValue.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
    if section == .followers {
      let rows = manifest.followers ?? []
      filteredFollowers = rows.filter { needle.isEmpty || $0.username.lowercased().contains(needle) || $0.fullName.lowercased().contains(needle) }
      filteredFollowers.sort { left, right in
        let comparison: ComparisonResult
        switch sortColumn {
        case "name": comparison = left.fullName.localizedCaseInsensitiveCompare(right.fullName)
        case "private": comparison = left.private == right.private ? left.username.compare(right.username) : (left.private ? .orderedDescending : .orderedAscending)
        case "verified": comparison = left.verified == right.verified ? left.username.compare(right.username) : (left.verified ? .orderedDescending : .orderedAscending)
        default: comparison = left.username.localizedCaseInsensitiveCompare(right.username)
        }
        return sortAscending ? comparison == .orderedAscending : comparison == .orderedDescending
      }
    } else {
      let source = section == .posts ? manifest.posts : (manifest.taggedPosts ?? [])
      filteredPosts = source.filter { needle.isEmpty || $0.shortcode.lowercased().contains(needle) || $0.date.lowercased().contains(needle) || $0.caption.lowercased().contains(needle) || ($0.author?.lowercased().contains(needle) ?? false) }
      filteredPosts.sort { left, right in
        let comparison: ComparisonResult
        switch sortColumn {
        case "caption": comparison = left.caption.localizedCaseInsensitiveCompare(right.caption)
        case "images": comparison = left.stills.count == right.stills.count ? left.shortcode.compare(right.shortcode) : (left.stills.count < right.stills.count ? .orderedAscending : .orderedDescending)
        case "videos": comparison = (left.videos ?? []).count == (right.videos ?? []).count ? left.shortcode.compare(right.shortcode) : ((left.videos ?? []).count < (right.videos ?? []).count ? .orderedAscending : .orderedDescending)
        case "author": comparison = (left.author ?? "").localizedCaseInsensitiveCompare(right.author ?? "")
        case "shortcode": comparison = left.shortcode.compare(right.shortcode)
        default: comparison = left.date.compare(right.date)
        }
        if comparison == .orderedSame { return left.shortcode < right.shortcode }
        return sortAscending ? comparison == .orderedAscending : comparison == .orderedDescending
      }
    }
    collectionView.selectionIndexPaths = []; tableView.deselectAll(nil); updateShareEnabled()
    collectionView.reloadData(); collectionView.collectionViewLayout?.invalidateLayout(); tableView.reloadData(); updateSubtitle()
  }

  private func makeColumn(_ id: String, _ title: String, _ width: CGFloat, _ minimum: CGFloat, sortable: Bool = true) -> NSTableColumn {
    let column = NSTableColumn(identifier: NSUserInterfaceItemIdentifier(id)); column.title = title; column.width = width; column.minWidth = minimum
    column.sortDescriptorPrototype = sortable ? NSSortDescriptor(key: id, ascending: id != "date") : nil
    return column
  }

  private func clearColumns() { for column in tableView.tableColumns { tableView.removeTableColumn(column) } }
  private func configurePostColumns() {
    clearColumns()
    let thumbnail = makeColumn("thumbnail", "Preview", 76, 70, sortable: false); thumbnail.maxWidth = 82; tableView.addTableColumn(thumbnail)
    if section == .tagged { tableView.addTableColumn(makeColumn("author", "Posted by", 130, 100)) }
    tableView.addTableColumn(makeColumn("date", "Date", 106, 96)); tableView.addTableColumn(makeColumn("caption", "Original caption", 410, 220))
    let images = makeColumn("images", "Images", 68, 62); images.maxWidth = 90; tableView.addTableColumn(images)
    let videos = makeColumn("videos", "Videos", 68, 62); videos.maxWidth = 90; tableView.addTableColumn(videos)
    tableView.addTableColumn(makeColumn("shortcode", "Shortcode", 124, 110)); tableView.rowHeight = 68
    sortColumn = "date"; sortAscending = false; tableView.sortDescriptors = [NSSortDescriptor(key: "date", ascending: false)]
  }

  private func configureFollowerColumns() {
    clearColumns(); tableView.addTableColumn(makeColumn("username", "Username", 220, 150))
    tableView.addTableColumn(makeColumn("name", "Name", 360, 180)); tableView.addTableColumn(makeColumn("private", "Private", 84, 70))
    tableView.addTableColumn(makeColumn("verified", "Verified", 84, 70)); tableView.rowHeight = 34
    sortColumn = "username"; sortAscending = true; tableView.sortDescriptors = [NSSortDescriptor(key: "username", ascending: true)]
  }

  private func updateSubtitle() {
    switch section {
    case .followers: subtitle.stringValue = "@\(manifest.account)   ·   \(filteredFollowers.count) followers   ·   private point-in-time snapshot"
    case .posts, .tagged:
      let stills = filteredPosts.reduce(0) { $0 + $1.stills.count }; let videos = filteredPosts.reduce(0) { $0 + ($1.videos ?? []).count }
      subtitle.stringValue = "@\(manifest.account)   ·   \(filteredPosts.count) \(section == .tagged ? "tagged " : "")posts   ·   \(stills) images   ·   \(videos) videos   ·   offline archive"
    }
  }

  private func updateShareEnabled() { shareItem.isEnabled = section != .followers && selectedPost() != nil }
  private func selectedPost() -> ArchivePost? {
    guard section != .followers else { return nil }
    let index = modeControl.selectedSegment == 2 ? tableView.selectedRow : (collectionView.selectionIndexPaths.first?.item ?? -1)
    return filteredPosts.indices.contains(index) ? filteredPosts[index] : nil
  }

  private func urls(for post: ArchivePost) -> [URL] {
    let stills = post.stills.compactMap { $0.file }.map { archiveRoot.appendingPathComponent($0) }
    let videos = (post.videos ?? []).compactMap { $0.file }.map { archiveRoot.appendingPathComponent($0) }
    return stills + videos
  }

  private func thumbnailURL(for post: ArchivePost) -> URL? {
    guard let file = post.stills.first?.file else { return nil }
    let thumbnail = thumbnailRoot.appendingPathComponent(file + ".jpg")
    return FileManager.default.fileExists(atPath: thumbnail.path) ? thumbnail : archiveRoot.appendingPathComponent(file)
  }

  @objc func openSelection() {
    guard let post = selectedPost() else { return }
    previewURLs = urls(for: post)
    guard !previewURLs.isEmpty, let panel = QLPreviewPanel.shared() else { return }
    panel.dataSource = self; panel.currentPreviewItemIndex = 0; panel.makeKeyAndOrderFront(nil)
  }

  @objc private func handleDoubleClick(_ recognizer: NSClickGestureRecognizer) {
    let point = recognizer.location(in: collectionView)
    if let indexPath = collectionView.indexPathForItem(at: point) { collectionView.selectItems(at: [indexPath], scrollPosition: []); updateShareEnabled() }
    openSelection()
  }

  func numberOfPreviewItems(in panel: QLPreviewPanel!) -> Int { previewURLs.count }
  func previewPanel(_ panel: QLPreviewPanel!, previewItemAt index: Int) -> QLPreviewItem! { previewURLs[index] as NSURL }

  func items(for pickerToolbarItem: NSSharingServicePickerToolbarItem) -> [Any] {
    guard let post = selectedPost() else { return [] }
    var items: [Any] = urls(for: post)
    let context = [post.caption, post.url].filter { !$0.isEmpty }.joined(separator: "\n\n")
    if !context.isEmpty { items.append(context as NSString) }
    return items
  }

  func sharingServicePicker(_ sharingServicePicker: NSSharingServicePicker,
                            delegateFor sharingService: NSSharingService) -> NSSharingServiceDelegate? { self }
  func sharingService(_ sharingService: NSSharingService, didFailToShareItems items: [Any], error: Error) {
    let alert = NSAlert(error: error); alert.messageText = "The post could not be shared"; alert.informativeText = error.localizedDescription; alert.runModal()
  }

  func toolbarAllowedItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    [Self.searchID, Self.sectionID, .flexibleSpace, Self.modeID, Self.shareID]
  }
  func toolbarDefaultItemIdentifiers(_ toolbar: NSToolbar) -> [NSToolbarItem.Identifier] {
    [Self.searchID, Self.sectionID, .flexibleSpace, Self.modeID, Self.shareID]
  }
  func toolbar(_ toolbar: NSToolbar, itemForItemIdentifier itemIdentifier: NSToolbarItem.Identifier,
               willBeInsertedIntoToolbar flag: Bool) -> NSToolbarItem? {
    if itemIdentifier == Self.searchID { let item = NSToolbarItem(itemIdentifier: itemIdentifier); item.label = "Search"; item.view = searchField; return item }
    if itemIdentifier == Self.sectionID { let item = NSToolbarItem(itemIdentifier: itemIdentifier); item.label = "Collection"; item.view = sectionControl; return item }
    if itemIdentifier == Self.modeID { let item = NSToolbarItem(itemIdentifier: itemIdentifier); item.label = "View"; item.view = modeControl; return item }
    if itemIdentifier == Self.shareID { return shareItem }
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
      let manifest = try JSONDecoder().decode(ArchiveManifest.self, from: Data(contentsOf: archiveRoot.appendingPathComponent("manifest.json")))
      let controller = GalleryController(manifest: manifest, archiveRoot: archiveRoot)
      installMainMenu()
      let window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 1320, height: 900),
        styleMask: [.titled, .closable, .miniaturizable, .resizable], backing: .buffered, defer: false)
      window.title = manifest.title.components(separatedBy: " —").first ?? manifest.title
      window.titleVisibility = .hidden; window.isRestorable = false; window.minSize = NSSize(width: 840, height: 560)
      window.contentViewController = controller; controller.installToolbar(on: window)
      if CommandLine.arguments.contains("--list") { controller.showListView() }
      window.center(); window.makeKeyAndOrderFront(nil); NSApp.activate(ignoringOtherApps: true)
      self.controller = controller; self.window = window
    } catch {
      let alert = NSAlert(error: error); alert.messageText = "The archive could not be opened"; alert.runModal(); NSApp.terminate(nil)
    }
  }

  func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }

  private func installMainMenu() {
    let main = NSMenu()
    let appItem = NSMenuItem(); let appMenu = NSMenu()
    appMenu.addItem(withTitle: "About \(ProcessInfo.processInfo.processName)", action: #selector(NSApplication.orderFrontStandardAboutPanel(_:)), keyEquivalent: "")
    appMenu.addItem(.separator()); appMenu.addItem(withTitle: "Hide \(ProcessInfo.processInfo.processName)", action: #selector(NSApplication.hide(_:)), keyEquivalent: "h")
    appMenu.addItem(withTitle: "Hide Others", action: #selector(NSApplication.hideOtherApplications(_:)), keyEquivalent: "h").keyEquivalentModifierMask = [.command, .option]
    appMenu.addItem(withTitle: "Show All", action: #selector(NSApplication.unhideAllApplications(_:)), keyEquivalent: "")
    appMenu.addItem(.separator()); appMenu.addItem(withTitle: "Quit \(ProcessInfo.processInfo.processName)", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
    appItem.submenu = appMenu; main.addItem(appItem)
    let fileItem = NSMenuItem(); let fileMenu = NSMenu(title: "File")
    fileMenu.addItem(withTitle: "Quick Look Selected Post", action: #selector(GalleryController.openSelection), keyEquivalent: "y")
    fileMenu.addItem(.separator()); fileMenu.addItem(withTitle: "Close", action: #selector(NSWindow.performClose(_:)), keyEquivalent: "w")
    fileItem.submenu = fileMenu; main.addItem(fileItem)
    let editItem = NSMenuItem(); let editMenu = NSMenu(title: "Edit")
    editMenu.addItem(withTitle: "Undo", action: Selector(("undo:")), keyEquivalent: "z"); editMenu.addItem(withTitle: "Redo", action: Selector(("redo:")), keyEquivalent: "Z")
    editMenu.addItem(.separator()); editMenu.addItem(withTitle: "Cut", action: #selector(NSText.cut(_:)), keyEquivalent: "x")
    editMenu.addItem(withTitle: "Copy", action: #selector(NSText.copy(_:)), keyEquivalent: "c"); editMenu.addItem(withTitle: "Paste", action: #selector(NSText.paste(_:)), keyEquivalent: "v")
    editMenu.addItem(withTitle: "Select All", action: #selector(NSText.selectAll(_:)), keyEquivalent: "a"); editItem.submenu = editMenu; main.addItem(editItem)
    let windowItem = NSMenuItem(); let windowMenu = NSMenu(title: "Window")
    windowMenu.addItem(withTitle: "Minimize", action: #selector(NSWindow.performMiniaturize(_:)), keyEquivalent: "m")
    windowMenu.addItem(withTitle: "Zoom", action: #selector(NSWindow.performZoom(_:)), keyEquivalent: "")
    windowMenu.addItem(.separator()); windowMenu.addItem(withTitle: "Bring All to Front", action: #selector(NSApplication.arrangeInFront(_:)), keyEquivalent: "")
    windowItem.submenu = windowMenu; main.addItem(windowItem); NSApp.windowsMenu = windowMenu; NSApp.mainMenu = main
  }
}

let app = NSApplication.shared
let delegate = AppDelegate()
app.setActivationPolicy(.regular)
app.delegate = delegate
app.run()
