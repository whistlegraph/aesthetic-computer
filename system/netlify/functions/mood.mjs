// mood, 23.07.09.17.54

// 1. GET `api/mood/@handle`
// Get the most recent mood from the user's handle or email.
// 1.1 GET `api/mood/moods-of-the-day`
// Pick the latest @jeffrey mood or any handled mood in the last 24 hours.

// 2. POST `api/mood`
// Allows a logged in user to set their mood, which
// updates the aesthetic.computer motd as well.

/* #region 🏁 TODO 
  - [] Write a GET request that retrieves all of a user's moods, paginated? 
    `const moods = await collection.find({ user }).toArray();` // Get all moods.
  - [] Write a GET request for getting a mood feed that consists of multiple users.
       (Just use the latest for now?)
  + Later
  - [] Trigger a notification via FCM on a user status update.
  💡
  - Actually... should the statuses be recordable text?
  - And should these be stored as separate files in a user's
    bucket, with merely an association for the status?
  - Or what is a cool way to store the recording in a linear
    fashion?
  + Done
  - [x] Test GET request.
  - [x] Write GET request for retrieving the latest mood from individual users.
  - [x] Test POST request.
#endregion */

import {
  authorize,
  userIDFromHandleOrEmail,
  getHandleOrEmail,
} from "../../backend/authorization.mjs";
import { connect, moodFor, allMoods, getMoodByRkey } from "../../backend/database.mjs";
import { respond, pathParams } from "../../backend/http.mjs";
import { createMoodOnAtproto } from "../../backend/mood-atproto.mjs";
import { shouldMirror, postMoodToBluesky } from "../../backend/bluesky-mirror.mjs";
import { fetchBlueskyEngagement } from "../../backend/bluesky-engagement.mjs";
// const dev = process.env.CONTEXT === "dev";

import { broadcastToTopic } from "../../../shared/push.mjs"; // Push notifications.

import { shell } from "../../backend/shell.mjs";
import { publishProfileEvent } from "../../backend/profile-stream.mjs";

import { filter } from "../../backend/filter.mjs"; // Profanity filtering.
// import { promises as fs } from "fs";
// import path from "path";

// let app;

const MOODS_OF_THE_DAY_WINDOW_MS = 24 * 60 * 60 * 1000;

async function fetchLatestMoodForSub(database, sub) {
  if (!sub) return null;
  const collection = database.db.collection("moods");
  const pipeline = [
    { $match: { user: sub, deleted: { $ne: true } } },
    { $sort: { when: -1 } },
    { $limit: 1 },
    {
      $lookup: {
        from: "@handles",
        localField: "user",
        foreignField: "_id",
        as: "handleInfo",
      },
    },
    { $unwind: { path: "$handleInfo", preserveNullAndEmptyArrays: true } },
    {
      $project: {
        _id: 0,
        mood: 1,
        when: 1,
        handle: {
          $cond: [
            { $ifNull: ["$handleInfo.handle", false] },
            { $concat: ["@", "$handleInfo.handle"] },
            null,
          ],
        },
      },
    },
  ];

  const records = await collection.aggregate(pipeline).toArray();
  return records?.[0] || null;
}

async function fetchRecentHandledMoods(database, since) {
  const collection = database.db.collection("moods");
  const pipeline = [
    {
      $match: {
        deleted: { $ne: true },
        when: { $gte: since },
      },
    },
    {
      $lookup: {
        from: "@handles",
        localField: "user",
        foreignField: "_id",
        as: "handleInfo",
      },
    },
    { $unwind: "$handleInfo" },
    {
      $project: {
        _id: 0,
        mood: 1,
        when: 1,
        handle: { $concat: ["@", "$handleInfo.handle"] },
      },
    },
    { $sort: { when: -1 } },
    { $limit: 200 },
  ];

  return await collection.aggregate(pipeline).toArray();
}

export async function handler(event, context) {
  if (event.httpMethod === "GET") {
    // 1. GET: Look up moods for user.
    const database = await connect();
    const slug = pathParams(event.path)[2];
    const handle = event.queryStringParameters?.for || null;

    if (slug === "moods-of-the-day" || slug === "motd") {
      const since = new Date(Date.now() - MOODS_OF_THE_DAY_WINDOW_MS);
      const listRequested = Boolean(event.queryStringParameters?.list);
      const jeffreyResult = await userIDFromHandleOrEmail("@jeffrey", database);
      const jeffreySub = typeof jeffreyResult === "string"
        ? jeffreyResult
        : jeffreyResult?.userID;

      const candidates = [];
      const seen = new Set();

      const addCandidate = (candidate) => {
        if (!candidate?.mood) return;
        const whenValue = candidate.when ? new Date(candidate.when).getTime() : "";
        const key = `${candidate.handle || ""}-${candidate.mood}-${whenValue}`;
        if (seen.has(key)) return;
        seen.add(key);
        candidates.push(candidate);
      };

      const jeffreyMood = await fetchLatestMoodForSub(database, jeffreySub);
      addCandidate(jeffreyMood);

      const recentMoods = await fetchRecentHandledMoods(database, since);
      for (const mood of recentMoods) addCandidate(mood);

      await database.disconnect();

      if (candidates.length === 0) {
        return respond(404, { message: "No mood found." });
      }

      if (listRequested) {
        return respond(200, { moods: candidates });
      }

      const pick = candidates[Math.floor(Math.random() * candidates.length)];
      return respond(200, pick);
    } else if (slug === "all") {
      // List all moods from the database and return
      // them as { moods }.
      const moods = await allMoods(database, handle);
      await database.disconnect();
      return moods && moods.length > 0
        ? respond(200, { moods })
        : respond(500, { message: "No mood found." });
    } else if (slug === "search") {
      // GET /api/mood/search?q=<text>&limit=N
      // Case-insensitive substring search over recent moods, newest first,
      // with @handles resolved. Powers the universal AC search box.
      const qRaw = (
        event.queryStringParameters?.q ||
        event.queryStringParameters?.search ||
        ""
      ).trim();
      const limit = Math.min(
        parseInt(event.queryStringParameters?.limit || "20", 10) || 20,
        100,
      );
      if (!qRaw) {
        await database.disconnect();
        return respond(400, { message: "Missing q/search parameter" });
      }
      const escaped = qRaw.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
      const collection = database.db.collection("moods");
      const results = await collection
        .aggregate([
          {
            $match: {
              deleted: { $ne: true },
              mood: { $regex: escaped, $options: "i" },
            },
          },
          {
            $lookup: {
              from: "@handles",
              localField: "user",
              foreignField: "_id",
              as: "handleInfo",
            },
          },
          { $unwind: "$handleInfo" },
          {
            $project: {
              _id: 0,
              mood: 1,
              when: 1,
              handle: { $concat: ["@", "$handleInfo.handle"] },
            },
          },
          { $sort: { when: -1 } },
          { $limit: limit },
        ])
        .toArray();
      await database.disconnect();
      return respond(200, {
        search: qRaw,
        count: results.length,
        moods: results,
      });
    } else if (slug === "single") {
      // GET /api/mood/single?handle=@jeffrey&rkey=<atproto_rkey>
      // Returns a single mood by handle and ATProto rkey (for permalinks)
      const handleParam = event.queryStringParameters?.handle;
      const rkeyParam = event.queryStringParameters?.rkey;

      if (!handleParam || !rkeyParam) {
        await database.disconnect();
        return respond(400, { message: "Missing handle or rkey parameter" });
      }

      const mood = await getMoodByRkey(database, handleParam, rkeyParam);
      await database.disconnect();

      if (!mood) {
        return respond(404, { message: "Mood not found" });
      }

      // Fetch Bluesky engagement if mood was mirrored
      let engagement = null;
      if (mood.bluesky?.uri) {
        try {
          engagement = await fetchBlueskyEngagement(mood.bluesky.uri);
        } catch (e) {
          shell.log("⚠️ Failed to fetch Bluesky engagement:", e.message);
        }
      }

      return respond(200, { ...mood, engagement });
    } else {
      shell.log("Getting user id from:", slug);
      const user = await userIDFromHandleOrEmail(slug, database);
      if (user && user.length > 0) {
        const mood = await moodFor(user, database);
        await database.disconnect();
        return mood
          ? respond(200, mood)
          : respond(500, { message: "No mood found." });
      } else {
        return respond(401, { message: "User not found." });
      }
    }
  } else if (event.httpMethod !== "POST") {
    return respond(405, { message: "Method Not Allowed" });
  }

  // 2. POST: Set / post a mood.
  try {
    const body = JSON.parse(event.body);
    const user = await authorize(event.headers);
    const handle = await getHandleOrEmail(user.sub);

    // console.log("🌙 Posting mood for:", handle, body);

    if (user && handle?.startsWith("@")) {
      const database = await connect();
      const collection = database.db.collection("moods"); // Make tweet-like collection.
      // Assume a mood update.
      if (body.mood) {
        const mood = filter(body.mood.trim()); // Trim extra whitespace off mood.

        // 📕 Database
        await collection.createIndex({ user: 1 }); // Index for `user`.
        await collection.createIndex({ when: 1 }); // Index for `when`.

        const lastMood = await collection.findOne(
          { user: user.sub },
          { sort: { when: -1 } },
        );

        if (!lastMood || lastMood.mood !== mood) {
          const insertResult = await collection.insertOne({
            user: user.sub,
            mood,
            when: new Date(),
          });

          publishProfileEvent({
            handle,
            event: {
              type: "mood",
              when: Date.now(),
              label: `Mood: ${mood}`,
            },
            countsDelta: { moods: 1 },
          }).catch((err) => {
            shell.log("⚠️ Mood profile-event publish failed:", err?.message || err);
          });

          // Dual-write: Also create mood on ATProto
          console.log("🔄 Syncing mood to ATProto...");
          let atprotoRkey = null;
          try {
            const atprotoResult = await createMoodOnAtproto(
              database,
              user.sub,
              mood,
              new Date(),
              insertResult.insertedId.toString(),
            );

            if (atprotoResult.rkey) {
              atprotoRkey = atprotoResult.rkey;
              // Update MongoDB with the ATProto rkey
              await collection.updateOne(
                { _id: insertResult.insertedId },
                { $set: { atproto: { rkey: atprotoResult.rkey } } },
              );
              console.log(
                "✅ Mood synced to ATProto with rkey:",
                atprotoResult.rkey,
              );
            }
          } catch (atprotoError) {
            // Log the error but don't fail the request - mood is already saved in MongoDB
            console.error("⚠️  Failed to sync mood to ATProto:", atprotoError);
          }

          // Mirror to Bluesky for configured handles (e.g., @jeffrey)
          if (atprotoRkey && await shouldMirror(database, handle)) {
            console.log(`🦋 Mirroring mood to Bluesky for ${handle}...`);
            try {
              const blueskyResult = await postMoodToBluesky(database, mood, handle, atprotoRkey);
              if (blueskyResult) {
                // Store Bluesky post reference for engagement tracking
                await collection.updateOne(
                  { _id: insertResult.insertedId },
                  { $set: { bluesky: { uri: blueskyResult.uri, cid: blueskyResult.cid, rkey: blueskyResult.rkey } } },
                );
                console.log("✅ Mood mirrored to Bluesky:", blueskyResult.rkey);
              }
            } catch (blueskyError) {
              // Don't fail the request - mood is already saved
              console.error("⚠️  Failed to mirror to Bluesky:", blueskyError);
            }
          }

          console.log("🌙 Setting a mood for:", handle, body.mood);

          await broadcastToTopic(database.db, "mood", {
            title: `${handle}'s mood is`,
            body: `${mood}`,
            data: { piece: "moods" },
          });
        }

        await database.disconnect();
        return respond(200, { mood }); // Successful mood change.
      } else if (body.nuke !== undefined) {
        // Flag all a user's moods as deleted.
        const result = await collection.updateMany(
          { user: user.sub },
          { $set: { deleted: body.nuke } },
        );
        await database.disconnect();
        return respond(200, { altered: result.modifiedCount });
        // Reply with number of moods modified.
      }
    } else {
      return respond(401, { message: "Authorization failure..." });
    }
  } catch (error) {
    return respond(500, { message: error });
  }
}
