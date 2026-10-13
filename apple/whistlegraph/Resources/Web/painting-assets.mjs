// Pixels live in IndexedDB; project versions hold small immutable references.
let database;
function open(){return database??=new Promise((resolve,reject)=>{const request=indexedDB.open('whistlegraph-painting-assets',1);request.onupgradeneeded=()=>request.result.createObjectStore('png');request.onsuccess=()=>resolve(request.result);request.onerror=()=>reject(request.error);});}
async function access(mode,operation){const db=await open();return new Promise((resolve,reject)=>{const transaction=db.transaction('png',mode),request=operation(transaction.objectStore('png'));transaction.oncomplete=()=>resolve(request.result);transaction.onerror=()=>reject(transaction.error);transaction.onabort=()=>reject(transaction.error||Error('Could not save painting pixels.'));});}
export async function savePaintingAsset(data){
 if(typeof data!=='string'||data.length>11200000||!/^iVBORw0KGgo[A-Za-z0-9+/]*={0,2}$/.test(data))throw Error('Use a PNG under 8 MB.');
 const id=crypto.randomUUID();await access('readwrite',store=>store.add(data,id));return id;
}
export async function loadPaintingAsset(id){const data=await access('readonly',store=>store.get(id));if(!data)throw Error('This painting’s pixels are unavailable.');return data;}
export async function removePaintingAsset(id){await access('readwrite',store=>store.delete(id));}
