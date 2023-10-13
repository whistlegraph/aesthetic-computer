// Slash, 23.10.13.00.08
// A helpful Discord webhook for auto-linking AC pieces.

// TODO: Test via the Netlify logs.

import { respond } from "../../backend/http.mjs";
import {
  verifyKey,
  InteractionResponseType,
  InteractionType,
} from "discord-interactions";

export async function handler(event) {
  // ⚠️ Uncomment these routes for manual initialization.
  // if (event.httpMethod === "DELETE") {
  //   await deleteAllCommands(process.env.DISCORD_PAL_APP_ID);
  //   return respond(200, { message: "Commands deleted successfully." });
  // }
  // if (event.httpMethod === "PUT") {
  //   await createACCommand(process.env.DISCORD_PAL_APP_ID);
  //   return respond(200, { message: "Command created successfully!" });
  // }

  const timestamp = event.headers["x-signature-timestamp"];
  const signature = event.headers["x-signature-ed25519"];

  if (!timestamp && !signature) {
    return respond(500, { message: "😈 Unauthorized." });
  }

  const isValidRequest = verifyKey(
    Buffer.from(event.body, "utf8"),
    signature,
    timestamp,
    process.env.DISCORD_PAL_PUBLIC,
  );

  console.log("Valid request:", isValidRequest);

  if (!isValidRequest) {
    return respond(401, { message: "😫 Invalid request signature." });
  }

  const body = JSON.parse(event.body);
  console.log("Body:", body);

  if (body.type === InteractionType.PING) {
    return respond(200, { type: InteractionResponseType.PONG });
  }

  console.log("Types:", body.type, InteractionType.APPLICATION_COMMAND);

  // Check for proprt slash command, translate, and reply.
  if (
    body.type === InteractionType.APPLICATION_COMMAND &&
    body.data.name === "ac"
  ) {
    // Respond with a deferred message
    const deferredResponse = respond(200, {
      type: InteractionResponseType.DEFERRED_CHANNEL_MESSAGE_WITH_SOURCE,
    });

    // Simulate some processing delay
    await new Promise((resolve) => setTimeout(resolve, 2000)); // 2 seconds delay

    const userInput = body.data.options[0].value;
    const transformedInput = userInput + " 😀";
    await sendFollowUpMessage(body.token, transformedInput);

    return deferredResponse;
  }

  return respond(400, { message: "🫠 Unhandled interaction type." });
}

const sendFollowUpMessage = async (interactionToken, content) => {
  const url = `https://discord.com/api/v10/webhooks/${process.env.DISCORD_PAL_APP_ID}/${interactionToken}`;

  const headers = {
    Authorization: `Bot ${process.env.DISCORD_PAL_BOT}`,
    "Content-Type": "application/json",
  };

  await fetch(url, {
    method: "POST",
    headers: headers,
    body: JSON.stringify({
      content: content,
    }),
  });
};

// Removes all discord commands.
const deleteAllCommands = async (clientId) => {
  // Guild-specific commands URL
  // const url = `https://discord.com/api/v10/applications/${clientId}/guilds/${process.env.DISCORD_SERVER_ID}/commands`;

  // Global commands URL
  const url = `https://discord.com/api/v10/applications/${clientId}/commands`;

  console.log(url);

  const headers = {
    Authorization: `Bot ${process.env.DISCORD_PAL_BOT}`,
    "Content-Type": "application/json",
  };

  const response = await fetch(url, {
    method: "GET",
    headers: headers,
  });

  const commands = await response.json();

  console.log("Commands available:", commands.errors);

  for (const command of commands) {
    await fetch(`${url}/${command.id}`, {
      method: "DELETE",
      headers: headers,
    });
  }
};

const createACCommand = async (clientId) => {
  const url = `https://discord.com/api/v10/applications/${clientId}/commands`;

  const headers = {
    Authorization: `Bot ${process.env.DISCORD_PAL_BOT}`,
    "Content-Type": "application/json",
  };

  const commandData = {
    name: "ac",
    description: "The catch-all aesthetic.computer command!",
    options: [
      {
        name: "input",
        type: 3,
        description: "Surround pieces like: `piece`.",
        required: true,
      },
    ],
  };

  await fetch(url, {
    method: "POST",
    headers: headers,
    body: JSON.stringify(commandData),
  });
};
