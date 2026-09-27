// Real plaintext websocket handshake, masked send, receive and quiet-peer pacing.
#include <assert.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <time.h>
#include "../src/ws-client.c"
void ac_log(const char *fmt, ...) { (void)fmt; }
static int listener;
static atomic_int received;
static double received_at;
static double ms(void) { struct timespec t; clock_gettime(CLOCK_MONOTONIC,&t); return t.tv_sec*1000.0+t.tv_nsec/1e6; }
static void *server(void *unused) {
  (void)unused;
  int fd=accept(listener,NULL,NULL); assert(fd>=0);
  struct timeval timeout={3,0};setsockopt(fd,SOL_SOCKET,SO_RCVTIMEO,&timeout,sizeof(timeout));
  char request[1024]={0};int used=0;
  while(used<1023 && !strstr(request,"\r\n\r\n")) { assert(recv(fd,request+used,1,0)==1);used++; }
  assert(strstr(request,"GET /oskiewar-live?match=ow-test123 HTTP/1.1"));
  const char *response="HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n\r\n";
  assert(send(fd,response,strlen(response),MSG_NOSIGNAL)==(int)strlen(response));
  unsigned char frame[11];used=0;
  while(used<11) { int n=recv(fd,frame+used,11-used,0); assert(n>0);used+=n; }
  received_at=ms();
  assert(frame[0]==0x81 && frame[1]==0x85);
  for(int i=0;i<5;i++) assert((frame[6+i]^frame[2+(i&3)])=="hello"[i]);
  atomic_store(&received,1);
  unsigned char reply[]={0x81,2,'{','}'};
  assert(send(fd,reply,sizeof(reply),MSG_NOSIGNAL)==sizeof(reply));
  usleep(100000);close(fd);return NULL;
}
int main(void) {
  listener=socket(AF_INET,SOCK_STREAM,0);assert(listener>=0);
  struct sockaddr_in address={.sin_family=AF_INET,.sin_addr.s_addr=htonl(INADDR_LOOPBACK)};
  assert(bind(listener,(struct sockaddr *)&address,sizeof(address))==0);assert(listen(listener,1)==0);
  socklen_t length=sizeof(address);assert(getsockname(listener,(struct sockaddr *)&address,&length)==0);
  pthread_t peer;pthread_create(&peer,NULL,server,NULL);
  ACWs *ws=ws_create();assert(ws);
  char url[160];snprintf(url,sizeof(url),"ws://127.0.0.1:%d/oskiewar-live?match=ow-test123",ntohs(address.sin_port));
  ws_connect(ws,url);
  for(int i=0;i<1500;i++) { pthread_mutex_lock(&ws->mu);int connected=ws->connected;pthread_mutex_unlock(&ws->mu);if(connected)break;usleep(2000); }
  assert(ws->connected);
  usleep(10000); // Deliberately send while the peer has no inbound traffic.
  double started=ms();ws_send(ws,"hello");
  for(int i=0;i<1500&&!atomic_load(&received);i++)usleep(2000);
  assert(atomic_load(&received));
  assert(received_at-started<30); // The old 50ms blocking pump fails this case.
  for(int i=0;i<1000;i++) { pthread_mutex_lock(&ws->mu);int count=ws->msg_count;pthread_mutex_unlock(&ws->mu);if(count)break;usleep(2000); }
  assert(ws_poll(ws)==1 && !strcmp(ws->messages[0],"{}"));
  printf("PASS plaintext local relay and quiet-peer send latency %.3fms\n",received_at-started);
  ws_destroy(ws);pthread_join(peer,NULL);close(listener);return 0;
}
