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

const json = (status, data) =>
  new Response(JSON.stringify(data), {
    status,
    headers: {
      "Content-Type": "application/json",
      "Access-Control-Allow-Origin": "*",
    },
  });

export default async function handler(req, context) {
  const { db, disconnect } = await connect();

  try {
    const url = new URL(req.url);
    const pathParts = url.pathname.split('/').filter(Boolean);

    // GET /m4l-plugins - List all latest plugins
    if (req.method === 'GET' && pathParts.length === 1) {
      const plugins = await getLatestPlugins(db);
      return json(200, plugins);
    }

    // GET /m4l-plugins/category/:category - List plugins by category
    if (req.method === 'GET' && pathParts.length === 3 && pathParts[1] === 'category') {
      const category = pathParts[2];
      const plugins = await getPluginsByCategory(db, category);
      return json(200, plugins);
    }

    // GET /m4l-plugins/versions/:deviceName - Get all versions of a device
    if (req.method === 'GET' && pathParts.length === 3 && pathParts[1] === 'versions') {
      const deviceName = pathParts[2];
      const versions = await getAllVersions(db, deviceName);
      return json(200, versions);
    }

    // GET /m4l-plugins/:code - Get specific plugin
    if (req.method === 'GET' && pathParts.length === 2) {
      const code = pathParts[1];
      const plugin = await getPluginByCode(db, code);

      if (!plugin) {
        return json(404, { error: 'Plugin not found' });
      }

      return json(200, plugin);
    }

    // POST /m4l-plugins/:code/download - Track download
    if (req.method === 'POST' && pathParts.length === 3 && pathParts[2] === 'download') {
      const code = pathParts[1];

      const plugin = await getPluginByCode(db, code);

      if (!plugin) {
        return json(404, { error: 'Plugin not found' });
      }

      await incrementDownloads(db, code);

      return json(200, {
        downloadUrl: plugin.m4l.downloadUrl,
        fileName: plugin.m4l.fileName,
        fileSize: plugin.m4l.fileSize
      });
    }

    // POST /m4l-plugins/:code/view - Track view
    if (req.method === 'POST' && pathParts.length === 3 && pathParts[2] === 'view') {
      const code = pathParts[1];

      const result = await incrementViews(db, code);

      if (result.matchedCount === 0) {
        return json(404, { error: 'Plugin not found' });
      }

      return json(200, { success: true });
    }

    return json(405, { error: 'Method not allowed' });

  } catch (error) {
    console.error('Error in m4l-plugins API:', error);
    return json(500, { error: 'Internal server error', message: error.message });
  } finally {
    await disconnect();
  }
}
