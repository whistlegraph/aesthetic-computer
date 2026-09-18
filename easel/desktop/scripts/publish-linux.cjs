#!/usr/bin/env node
// Run with @aws-sdk/client-s3 available and SPACES_KEY / SPACES_SECRET set.
// aesel has its own prefix: never register this with the AC Desktop release API.
const fs = require('node:fs');
const path = require('node:path');
const crypto = require('node:crypto');
const {S3Client, PutObjectCommand, CreateMultipartUploadCommand, UploadPartCommand,
 CompleteMultipartUploadCommand, AbortMultipartUploadCommand, PutObjectAclCommand} = require('@aws-sdk/client-s3');
const dir = path.resolve(process.argv[2] || 'dist');
const names = fs.readdirSync(dir).filter(n => /^aesel-[\d.]+-linux-(x64|x86_64|amd64)\.(AppImage|deb|tar\.gz)$/.test(n));
if (names.length !== 3 || !fs.existsSync(path.join(dir,'latest-linux.yml'))) throw Error('Expected three Linux artifacts and update manifest');
const manifest=fs.readFileSync(path.join(dir,'latest-linux.yml'),'utf8');
const appImage=names.find(n=>n.endsWith('.AppImage'));
const digest=crypto.createHash('sha512').update(fs.readFileSync(path.join(dir,appImage))).digest('base64');
if(!manifest.includes('path: '+appImage) || !manifest.includes('sha512: '+digest)) throw Error('AppImage update manifest does not match binary');
if(!fs.existsSync(path.join(dir,'index.html'))) throw Error('Missing download page');
const sums = names.map(n => crypto.createHash('sha256').update(fs.readFileSync(path.join(dir,n))).digest('hex')+'  '+n).join('\n')+'\n';
fs.writeFileSync(path.join(dir,'SHA256SUMS'),sums);
if (!process.env.SPACES_KEY || !process.env.SPACES_SECRET) throw Error('Missing Spaces credentials');
const client = new S3Client({endpoint:process.env.SPACES_ENDPOINT || 'https://sfo3.digitaloceanspaces.com',region:'us-east-1',credentials:{accessKeyId:process.env.SPACES_KEY,secretAccessKey:process.env.SPACES_SECRET},requestChecksumCalculation:'WHEN_REQUIRED',responseChecksumValidation:'WHEN_REQUIRED'});
const Bucket='releases-aesthetic-computer';
async function upload(name) {
 const file=path.join(dir,name),size=fs.statSync(file).size;
 const Key='easel/desktop/'+name;
 const metadata={Bucket,Key,ContentType:name.endsWith('.html')?'text/html; charset=utf-8':name.endsWith('.yml')?'text/yaml':'application/octet-stream',CacheControl:name.startsWith('aesel-')?'public, max-age=31536000, immutable':'no-cache'};
 if(size<8*1024*1024) await client.send(new PutObjectCommand({...metadata,Body:fs.readFileSync(file),ACL:'public-read'}));
 else {
  const {UploadId}=await client.send(new CreateMultipartUploadCommand(metadata));
  const handle=fs.openSync(file,'r');
  try {
   const Parts=[];
   for(let offset=0;offset<size;offset+=8*1024*1024){
    const Body=Buffer.alloc(Math.min(8*1024*1024,size-offset));fs.readSync(handle,Body,0,Body.length,offset);
    const PartNumber=Parts.length+1;
    const {ETag}=await client.send(new UploadPartCommand({Bucket,Key,UploadId,PartNumber,Body}));Parts.push({ETag,PartNumber});
   }
   await client.send(new CompleteMultipartUploadCommand({Bucket,Key,UploadId,MultipartUpload:{Parts}}));
   await client.send(new PutObjectAclCommand({Bucket,Key,ACL:'public-read'}));
  } catch(error){await client.send(new AbortMultipartUploadCommand({Bucket,Key,UploadId})).catch(()=>{});throw error;}
  finally{fs.closeSync(handle);}
 }
 console.log('Published '+name);
}
(async()=>{for(const name of [...names,'SHA256SUMS','latest-linux.yml','index.html'])await upload(name);})().catch(error=>{console.error(error.message);process.exitCode=1;});
