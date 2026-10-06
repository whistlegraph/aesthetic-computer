import {validateRoom} from './room-schema.mjs';
import {ROOM_RUNTIME} from './roblox-runtime.mjs';
const xml=value=>String(value).replaceAll('&','&amp;').replaceAll('<','&lt;').replaceAll('>','&gt;').replaceAll('"','&quot;');
export function roomPlace(room){
  const data=JSON.stringify(validateRoom(room));
  const source=`local Http = game:GetService("HttpService")\nlocal Room = (function()\n${ROOM_RUNTIME}\nend)()\nlocal room = Http:JSONDecode([==[${data}]==])\nlocal model = Room.build(room, Vector3.zero)\nmodel.Parent = workspace\nworkspace.FallenPartsDestroyHeight = -60\n`;
  return `<roblox version="4"><Item class="Workspace" referent="Workspace"><Properties><string name="Name">Workspace</string></Properties></Item><Item class="ServerScriptService" referent="ServerScriptService"><Properties><string name="Name">ServerScriptService</string></Properties><Item class="Script" referent="RoomScript"><Properties><string name="Name">WhistlegraphRoom</string><ProtectedString name="Source">${xml(source)}</ProtectedString></Properties></Item></Item></roblox>`;
}
