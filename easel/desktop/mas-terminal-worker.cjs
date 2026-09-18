const {PassThrough}=require('node:stream');
const {pathToFileURL}=require('node:url');
const input=new PassThrough();
input.isTTY=true;
input.isRaw=false;
input.setRawMode=value=>{input.isRaw=!!value;return input;};
Object.defineProperty(process,'stdin',{value:input});
for(const output of [process.stdout,process.stderr]){
  output.isTTY=true;
  output.columns=Number(process.env.EASEL_COLUMNS)||100;
  output.rows=Number(process.env.EASEL_ROWS)||32;
  output.getWindowSize=()=>[output.columns,output.rows];
}
process.parentPort.on('message',({data})=>{
  if(data.type==='input')input.write(data.data);
  else if(data.type==='resize'){
    for(const output of [process.stdout,process.stderr]){output.columns=data.cols;output.rows=data.rows;output.emit('resize');}
  }else if(data.type==='signal'&&data.signal==='SIGUSR2')process.emit('SIGUSR2');
});
const entry=process.argv[2];
process.argv=[process.execPath,...process.argv.slice(2)];
import(pathToFileURL(entry).href).catch(error=>{console.error(error);process.exit(1);});
