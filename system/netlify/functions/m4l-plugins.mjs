// m4l-plugins.mjs
// API endpoint for Max for Live plugin management
// GET /m4l-plugins - List all latest plugins
// POST /m4l-plugins/:code/download - Track download and return URL
// POST /m4l-plugins/:code/view - Track view
// GET /m4l-plugins/:category - List plugins by category

import { connect } from '../../backend/database.mjs';
import {
  getLatestPlugins,
  getPluginByCode,
  getPluginsByCategory,
  incrementDownloads,
  incrementViews,
  getAllVersions
} from '../../backend/plugins.mjs';
import { respond } from '../../backend/http.mjs';

export default async function handler(req, context) {
  const { db, disconnect } = await connect();

  try {
    const url = new URL(req.url, `https://${req.headers.host}`);
    const pathParts = url.pathname.split('/').filter(Boolean);

    // GET /m4l-plugins - List all latest plugins
    if (req.method === 'GET' && pathParts.length === 1) {
      const plugins = await getLatestPlugins(db);
      return respond(200, plugins);
    }

    // GET /m4l-plugins/category/:category - List plugins by category
    if (req.method === 'GET' && pathParts.length === 3 && pathParts[1] === 'category') {
      const category = pathParts[2];
      const plugins = await getPluginsByCategory(db, category);
      return respond(200, plugins);
    }

    // GET /m4l-plugins/versions/:deviceName - Get all versions of a device
    if (req.method === 'GET' && pathParts.length === 3 && pathParts[1] === 'versions') {
      const deviceName = pathParts[2];
      const versions = await getAllVersions(db, deviceName);
      return respond(200, versions);
    }

    // GET /m4l-plugins/:code - Get specific plugin
    if (req.method === 'GET' && pathParts.length === 2) {
      const code = pathParts[1];
      const plugin = await getPluginByCode(db, code);

      if (!plugin) {
        return respond(404, { error: 'Plugin not found' });
      }

      return respond(200, plugin);
    }

    // POST /m4l-plugins/:code/download - Track download
    if (req.method === 'POST' && pathParts.length === 3 && pathParts[2] === 'download') {
      const code = pathParts[1];

      // Get plugin first to ensure it exists
      const plugin = await getPluginByCode(db, code);

      if (!plugin) {
        return respond(404, { error: 'Plugin not found' });
      }

      // Increment download counter
      await incrementDownloads(db, code);

      // Return download URL
      return respond(200, {
        downloadUrl: plugin.m4l.downloadUrl,
        fileName: plugin.m4l.fileName,
        fileSize: plugin.m4l.fileSize
      });
    }

    // POST /m4l-plugins/:code/view - Track view
    if (req.method === 'POST' && pathParts.length === 3 && pathParts[2] === 'view') {
      const code = pathParts[1];

      // Increment view counter
      const result = await incrementViews(db, code);

      if (result.matchedCount === 0) {
        return respond(404, { error: 'Plugin not found' });
      }

      return respond(200, { success: true });
    }

    // Method not allowed
    return respond(405, { error: 'Method not allowed' });

  } catch (error) {
    console.error('Error in m4l-plugins API:', error);
    return respond(500, { error: 'Internal server error', message: error.message });
  } finally {
    await disconnect();
  }
}
