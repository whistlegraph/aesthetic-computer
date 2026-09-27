// The real parser against an endlessly readable TLS stream and a server ping.
#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/ssl.h>
static int fake_read(SSL *, void *, int);
static int fake_write(SSL *, const void *, int);
#define SSL_pending(ssl) 1
#define SSL_read fake_read
#define SSL_write fake_write
#include "../src/ws-client.c"
#undef SSL_read
#undef SSL_write
void ac_log(const char *fmt, ...) { (void)fmt; }
static int reads, writes, ping;
static unsigned char sent[256];
static int fake_read(SSL *ssl, void *data, int cap) {
  (void)ssl;
  unsigned char frame[]={0x81,2,'{','}'};
  if(ping)frame[0]=0x89;
  assert(cap>=4);memcpy(data,frame,4);reads++;return 4;
}
static int fake_write(SSL *ssl,const void *data,int len) {
  (void)ssl;assert(len<256);memcpy(sent,data,len);writes++;return len;
}
int main(void) {
  ACWs *ws=calloc(1,sizeof(*ws));assert(ws);
  pthread_mutex_init(&ws->mu,NULL);ws->ssl=(void *)1;
  thread_recv(ws);
  assert(reads==1 && ws->msg_count==1 && !strcmp(ws->messages[0],"{}"));
  ping=1;thread_recv(ws);
  assert(reads==2 && writes==1 && sent[0]==0x8a && sent[1]==0x82);
  assert((sent[6]^sent[2])=='{' && (sent[7]^sent[3])=='}');
  thread_send(ws,"hello");
  assert(writes==2 && sent[0]==0x81 && sent[1]==0x85);
  for(int i=0;i<5;i++)assert((sent[6+i]^sent[2+(i&3)])=="hello"[i]);
  pthread_mutex_destroy(&ws->mu);free(ws);
  return 0;
}
