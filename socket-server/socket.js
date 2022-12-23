// 🐕‍ Server
// Handles *some* online multiplayer and realtime interaction @ server.aesthetic.computer.
// TODO: Move multiplayer functionality over to session-server but keep this
//       in production for "development" features only? (Research it... 22.12.04.16.06)

// UDP Server (using Twilio ICE servers)
// const accountSid = process.env.TWILIO_ACCOUNT_SID;
// const authToken = process.env.TWILIO_AUTH_TOKEN;
// See also: https://www.twilio.com/docs/stun-turn/api?code-sample=code-create-a-token-resource&code-language=Node.js&code-sdk-version=3.x#
//import twilio from "twilio";
//const client = twilio(accountSid, authToken);
// console.log(client);
//client.tokens.create({ ttl: 3600 }).then((token) => {
//  console.log("Twilio:", token);
//});

/*
import geckos from "@geckos.io/server";

const io = geckos();

io.listen(3000); // default port is 9208

io.onConnection((channel) => {
  channel.onDisconnect(() => {
    console.log(`${channel.id} got disconnected`);
  });

  channel.on("chat message", (data) => {
    console.log(`got ${data} from "chat message"`);
    // emit the "chat message" data to all channels in the same room
    io.room(channel.roomId).emit("chat message", data);
  });
});
*/