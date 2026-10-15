// Public mood record shared by AC and product clients.
export function moodPost(row, handle, code) {
  return {
    code: "mood_" + String(row._id), parent: null, board: "text/plain", name: handle, text: row.mood,
    when: row.when, replies: 0, file: null, origin: { name: "mood", url: "https://aesthetic.computer/" + encodeURIComponent(handle) },
    media: { kind: "mood", id: String(row._id), code, url: "https://aesthetic.computer/" + encodeURIComponent(handle), author: handle },
  };
}
