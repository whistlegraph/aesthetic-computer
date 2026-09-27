#import <Foundation/Foundation.h>
int main(int argc, char **argv) {
 @autoreleasepool {
  if(argc!=3)return 2;
  NSData *data=[NSData dataWithContentsOfFile:[NSString stringWithUTF8String:argv[2]]];
  NSError *error=nil;
  NSDictionary *info=[NSJSONSerialization JSONObjectWithData:data options:0 error:&error];
  if(![info isKindOfClass:[NSDictionary class]]||error)return 2;
  [[NSDistributedNotificationCenter defaultCenter] postNotificationName:[NSString stringWithUTF8String:argv[1]] object:nil userInfo:info deliverImmediately:YES];
  [[NSRunLoop currentRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:.1]];
 }
 return 0;
}
