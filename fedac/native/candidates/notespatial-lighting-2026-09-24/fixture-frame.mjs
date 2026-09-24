import {lookAt} from './score-look.mjs';
// Physical room fixtures omit the center-rear laptop; held wedge is a separate USB.
export const ROOM_FIXTURES=[{address:1,seat:0},{address:11,seat:1},{address:31,seat:2},{address:21,seat:3}];
export function fixtureFrame(timeline,t) {
  return {
    room:ROOM_FIXTURES.map(({address,seat})=>({address,rgb:lookAt(timeline,t,seat).rgb})),
    held:{address:41,rgb:lookAt(timeline,t,5).rgb},
  };
}
export function heldSlots(timeline,t) {
  const slots=new Array(64).fill(0);
  lookAt(timeline,t,5).rgb.forEach((value,i)=>{slots[40+i]=value;});
  return slots;
}
