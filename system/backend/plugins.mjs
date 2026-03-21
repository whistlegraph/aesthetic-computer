// plugins.mjs, 26.02.12
// Manage Max for Live plugin versions and downloads
// Collection: aesthetic.plugins
//
// Code Prefix:
//   & - M4L Plugins (e.g., &np123a for notepat v1.2.3)

// Schema:
// {
//   _id: ObjectId,
//   code: string,                    // Unique short code (6 chars, e.g., "np123a")
//   device: {
//     name: string,                  // "notepat", "pedal", "metronome"
//     displayName: string,           // "AC 🟪 Notepat"
//     category: "instrument" | "effect" | "midi",
//     icon: string,                  // Emoji or icon identifier
//   },
//   version: {
//     major: number,
//     minor: number,
//     patch: number,
//     string: string,                // "1.2.3"
//   },
//   m4l: {
//     downloadUrl: string,           // "https://assets.aesthetic.computer/m4l/..."
//     fileName: string,              // "AC 🟪 notepat (aesthetic.computer).amxd"
//     fileSize: number,              // Bytes
//     checksum: string,              // SHA256
//   },
//   metadata: {
//     description: string,
//     piece: string,                 // AC piece name (e.g., "notepat")
//     width: number,
//     height: number,
//     releaseNotes: string,
//   },
//   stats: {
//     downloads: number,
//     views: number,
//   },
//   createdAt: Date,
//   updatedAt: Date,
//   publishedAt: Date,
//   deprecated: boolean,
// }

// Generate a short unique code for plugins
function generateCode(deviceName, version, length = 6) {
  const chars = "abcdefghijklmnopqrstuvwxyz0123456789";
  // Use first 2 chars of device name + version numbers + random chars
  const prefix = deviceName.substring(0, 2);
  const versionStr = version.string.replace(/\./g, '');
  let code = prefix + versionStr;

  // Pad with random chars if needed
  while (code.length < length) {
    code += chars[Math.floor(Math.random() * chars.length)];
  }

  return code.substring(0, length);
}

/**
 * Create a new plugin record in the database
 * @param {Object} db - Database connection
 * @param {Object} pluginData - Plugin data to insert
 * @returns {Object} Created plugin document
 */
export async function createPlugin(db, pluginData) {
  const plugins = db.collection('plugins');

  // Generate unique code if not provided
  if (!pluginData.code) {
    pluginData.code = generateCode(
      pluginData.device.name,
      pluginData.version
    );

    // Ensure uniqueness
    let exists = await plugins.findOne({ code: pluginData.code });
    while (exists) {
      pluginData.code = generateCode(
        pluginData.device.name,
        pluginData.version
      );
      exists = await plugins.findOne({ code: pluginData.code });
    }
  }

  // Set timestamps
  const now = new Date();
  pluginData.createdAt = now;
  pluginData.updatedAt = now;

  // Initialize stats if not provided
  if (!pluginData.stats) {
    pluginData.stats = { downloads: 0, views: 0 };
  }

  // Insert into database
  const result = await plugins.insertOne(pluginData);
  pluginData._id = result.insertedId;

  console.log(`✅ Created plugin: ${pluginData.device.displayName} v${pluginData.version.string} (${pluginData.code})`);

  return pluginData;
}

/**
 * Get the latest version of each plugin
 * @param {Object} db - Database connection
 * @returns {Array} Array of latest plugin versions
 */
export async function getLatestPlugins(db) {
  const plugins = db.collection('plugins');

  // Aggregation pipeline to get latest version of each device
  const pipeline = [
    // Filter out deprecated plugins
    { $match: { deprecated: { $ne: true } } },

    // Sort by version descending
    { $sort: {
      "device.name": 1,
      "version.major": -1,
      "version.minor": -1,
      "version.patch": -1
    }},

    // Group by device name and take first (highest version)
    {
      $group: {
        _id: "$device.name",
        latestPlugin: { $first: "$$ROOT" }
      }
    },

    // Replace root with the plugin document
    { $replaceRoot: { newRoot: "$latestPlugin" } },

    // Sort by device name for consistent ordering
    { $sort: { "device.name": 1 } },

    // Project only needed fields (exclude MongoDB _id)
    {
      $project: {
        _id: 0,
        code: 1,
        device: 1,
        version: 1,
        m4l: 1,
        metadata: 1,
        stats: 1,
        publishedAt: 1
      }
    }
  ];

  const result = await plugins.aggregate(pipeline).toArray();
  return result;
}

/**
 * Get a specific plugin by code
 * @param {Object} db - Database connection
 * @param {string} code - Plugin code
 * @returns {Object|null} Plugin document or null
 */
export async function getPluginByCode(db, code) {
  const plugins = db.collection('plugins');
  return await plugins.findOne({ code }, { projection: { _id: 0 } });
}

/**
 * Get all versions of a specific device
 * @param {Object} db - Database connection
 * @param {string} deviceName - Device name (e.g., "notepat")
 * @returns {Array} Array of all versions sorted by version descending
 */
export async function getAllVersions(db, deviceName) {
  const plugins = db.collection('plugins');

  const versions = await plugins.find(
    { "device.name": deviceName },
    { projection: { _id: 0 } }
  )
  .sort({
    "version.major": -1,
    "version.minor": -1,
    "version.patch": -1
  })
  .toArray();

  return versions;
}

/**
 * Increment download counter for a plugin
 * @param {Object} db - Database connection
 * @param {string} code - Plugin code
 * @returns {Object} Update result
 */
export async function incrementDownloads(db, code) {
  const plugins = db.collection('plugins');

  const result = await plugins.updateOne(
    { code },
    {
      $inc: { "stats.downloads": 1 },
      $set: { updatedAt: new Date() }
    }
  );

  return result;
}

/**
 * Increment view counter for a plugin
 * @param {Object} db - Database connection
 * @param {string} code - Plugin code
 * @returns {Object} Update result
 */
export async function incrementViews(db, code) {
  const plugins = db.collection('plugins');

  const result = await plugins.updateOne(
    { code },
    {
      $inc: { "stats.views": 1 },
      $set: { updatedAt: new Date() }
    }
  );

  return result;
}

/**
 * Mark a plugin as deprecated
 * @param {Object} db - Database connection
 * @param {string} code - Plugin code
 * @returns {Object} Update result
 */
export async function deprecatePlugin(db, code) {
  const plugins = db.collection('plugins');

  const result = await plugins.updateOne(
    { code },
    {
      $set: {
        deprecated: true,
        updatedAt: new Date()
      }
    }
  );

  return result;
}

/**
 * Update plugin metadata
 * @param {Object} db - Database connection
 * @param {string} code - Plugin code
 * @param {Object} updates - Fields to update
 * @returns {Object} Update result
 */
export async function updatePlugin(db, code, updates) {
  const plugins = db.collection('plugins');

  const result = await plugins.updateOne(
    { code },
    {
      $set: {
        ...updates,
        updatedAt: new Date()
      }
    }
  );

  return result;
}

/**
 * Search plugins by category
 * @param {Object} db - Database connection
 * @param {string} category - Plugin category ("instrument", "effect", "midi")
 * @returns {Array} Array of matching plugins
 */
export async function getPluginsByCategory(db, category) {
  const plugins = db.collection('plugins');

  // Get latest version of each device in category
  const pipeline = [
    { $match: {
      "device.category": category,
      deprecated: { $ne: true }
    }},
    { $sort: {
      "device.name": 1,
      "version.major": -1,
      "version.minor": -1,
      "version.patch": -1
    }},
    {
      $group: {
        _id: "$device.name",
        latestPlugin: { $first: "$$ROOT" }
      }
    },
    { $replaceRoot: { newRoot: "$latestPlugin" } },
    { $sort: { "device.name": 1 } },
    { $project: { _id: 0 } }
  ];

  return await plugins.aggregate(pipeline).toArray();
}

/**
 * Create indexes for the plugins collection
 * This should be run once during setup
 * @param {Object} db - Database connection
 */
export async function createIndexes(db) {
  const plugins = db.collection('plugins');

  await plugins.createIndex({ code: 1 }, { unique: true });
  await plugins.createIndex({ "device.name": 1 });
  await plugins.createIndex({ "device.category": 1 });
  await plugins.createIndex({ publishedAt: -1 });
  await plugins.createIndex({
    "version.major": 1,
    "version.minor": 1,
    "version.patch": 1
  });
  await plugins.createIndex({ updatedAt: -1 });
  await plugins.createIndex({ deprecated: 1 });

  console.log("✅ Created indexes for plugins collection");
}

export { generateCode };
