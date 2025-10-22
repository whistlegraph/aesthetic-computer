Consolidate tape playback in video piece with loading progress UI

- Video piece now handles both recording and !code playback, removing dependency on separate replay piece
- Added real-time loading progress with yellow bar tracking download → unpacking → frames phases  
- Backend infrastructure for tape short codes, MongoDB tracking, and ATProto sync APIs
