#define _POSIX_C_SOURCE 200809L
#define _XOPEN_SOURCE 700
#include <errno.h>
#include <fcntl.h>
#include <locale.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <spawn.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#include <wchar.h>

// UI owns the terminal. I/O owns the child and pipes. Shared data is bounded,
// mutex-protected and never held while spawning, reading, writing or painting.
enum { INPUT_CAP=8192, TEXT_CAP=524288, PACKET_CAP=65536, QUEUE_CAP=8,
       MAX_ROWS=160, MAX_COLS=300, ROW_BYTES=MAX_COLS*4+1 };
typedef struct { char type; size_t size; char data[INPUT_CAP]; } Command;
typedef struct {
  pthread_mutex_t lock;
  char response[TEXT_CAP], status[512], thread[160];
  size_t used;
  bool busy, dead, truncated, stopping;
  Command commands[QUEUE_CAP]; unsigned head, count;
  int ui_wake[2], io_wake[2];
  char **child_argv;
} Shared;
static Shared shared={.lock=PTHREAD_MUTEX_INITIALIZER};
static struct termios saved;
static bool terminal_active;
static volatile sig_atomic_t interrupted;

static void on_signal(int number) { (void)number; interrupted=1; }
static void restore(void) {
  if(terminal_active) {
    tcsetattr(STDIN_FILENO,TCSANOW,&saved);
    const char *s="\033[?2004l\033[?25h\033[?1049l";
    (void)write(STDOUT_FILENO,s,strlen(s)); terminal_active=false;
  }
}
static void ping(int fd) { char c=1; (void)write(fd,&c,1); }
static void drain(int fd) { char b[128]; while(read(fd,b,sizeof b)>0) {} }
static int pipe_flags(int fds[2],bool nonblocking) {
  if(pipe(fds))return -1;
  for(int i=0;i<2;i++) {
    if(fcntl(fds[i],F_SETFD,FD_CLOEXEC)<0 ||
       (nonblocking && fcntl(fds[i],F_SETFL,O_NONBLOCK)<0)) {
      close(fds[0]);close(fds[1]);return -1;
    }
  }
  return 0;
}
static bool stopping(void) {
  pthread_mutex_lock(&shared.lock); bool stop=shared.stopping;
  pthread_mutex_unlock(&shared.lock); return stop;
}
static void status(const char *text,bool dead) {
  pthread_mutex_lock(&shared.lock);
  snprintf(shared.status,sizeof shared.status,"%s",text);
  if(dead){shared.dead=true;shared.busy=false;}
  pthread_mutex_unlock(&shared.lock); ping(shared.ui_wake[1]);
}
static size_t clean(char *to,size_t cap,const char *from,size_t n) {
  size_t used=0;
  for(size_t i=0;i<n && used+1<cap;i++) {
    unsigned char c=(unsigned char)from[i];
    if(c=='\t')to[used++]=' ';
    else if(c=='\n'||c>=32) { if(c!=127)to[used++]=(char)c; }
  }
  to[used]=0;return used;
}
static void received(char type,const char *data,size_t size) {
  pthread_mutex_lock(&shared.lock);
  if(type=='D') {
    if(size>=TEXT_CAP-shared.used)shared.truncated=true;
    shared.used+=clean(shared.response+shared.used,TEXT_CAP-shared.used,data,size);
  } else if(type=='S'||type=='E') {
    clean(shared.status,sizeof shared.status,data,size);
    if(type=='E')shared.busy=false;
  } else if(type=='B')shared.busy=size && data[0]=='1';
  else if(type=='H')clean(shared.thread,sizeof shared.thread,data,size);
  pthread_mutex_unlock(&shared.lock); ping(shared.ui_wake[1]);
}
static bool enqueue(char type,const char *data,size_t size) {
  pthread_mutex_lock(&shared.lock);
  bool ok=!shared.dead && shared.count<QUEUE_CAP && size<INPUT_CAP;
  if(ok) {
    Command *c=&shared.commands[(shared.head+shared.count)%QUEUE_CAP];
    c->type=type;c->size=size;if(size)memcpy(c->data,data,size);
    shared.count++;
    if(type=='P'||type=='R')shared.busy=true;
    if(type=='P'){shared.used=0;shared.response[0]=0;shared.truncated=false;}
  }
  pthread_mutex_unlock(&shared.lock);
  if(ok)ping(shared.io_wake[1]);else status("Bridge unavailable or command queue full; draft kept",false);
  return ok;
}

static void *bridge_io(void *unused) {
  (void)unused;
  int input[2],output[2]; pid_t child=-1;
  if(pipe_flags(input,false)){status("Cannot open bridge input",true);return NULL;}
  if(pipe_flags(output,false)){close(input[0]);close(input[1]);status("Cannot open bridge output",true);return NULL;}
  posix_spawn_file_actions_t actions;posix_spawn_file_actions_init(&actions);
  posix_spawn_file_actions_adddup2(&actions,input[0],STDIN_FILENO);
  posix_spawn_file_actions_adddup2(&actions,output[1],STDOUT_FILENO);
  // Stderr is separate from the framed protocol and may contain sensitive
  // provider logs. This experiment displays bounded structured errors only.
  posix_spawn_file_actions_addopen(&actions,STDERR_FILENO,"/dev/null",O_WRONLY,0);
  posix_spawnattr_t attr;posix_spawnattr_init(&attr);
  posix_spawnattr_setflags(&attr,POSIX_SPAWN_SETPGROUP);posix_spawnattr_setpgroup(&attr,0);
  extern char **environ;
  int error=posix_spawnp(&child,shared.child_argv[0],&actions,&attr,shared.child_argv,environ);
  posix_spawnattr_destroy(&attr);posix_spawn_file_actions_destroy(&actions);
  close(input[0]);close(output[1]);
  if(error){close(input[1]);close(output[0]);status(strerror(error),true);return NULL;}
  fcntl(input[1],F_SETFL,O_NONBLOCK);fcntl(output[0],F_SETFL,O_NONBLOCK);
  unsigned char inbound[PACKET_CAP+5],outbound[INPUT_CAP+5];
  size_t have=0,sending=0,sent=0;bool failed=false;
  while(!stopping()) {
    if(!sending) {
      pthread_mutex_lock(&shared.lock);
      if(shared.count) {
        Command *c=&shared.commands[shared.head];
        outbound[0]=(unsigned char)c->type;
        for(int i=0;i<4;i++)outbound[i+1]=(unsigned char)(c->size>>(24-8*i));
        memcpy(outbound+5,c->data,c->size);sending=5+c->size;sent=0;
        shared.head=(shared.head+1)%QUEUE_CAP;shared.count--;
      }
      pthread_mutex_unlock(&shared.lock);
    }
    struct pollfd fds[]={{output[0],POLLIN,0},{shared.io_wake[0],POLLIN,0},{input[1],sending?POLLOUT:0,0}};
    int ready=poll(fds,3,100);
    if(ready<0){if(errno==EINTR)continue;failed=true;break;}
    if(fds[1].revents&POLLIN)drain(shared.io_wake[0]);
    if(fds[2].revents&(POLLERR|POLLHUP|POLLNVAL)){failed=true;break;}
    if(sending && (fds[2].revents&POLLOUT)) {
      ssize_t n=write(input[1],outbound+sent,sending-sent);
      if(n>0){sent+=(size_t)n;if(sent==sending)sending=0;}
      else if(n<0 && errno!=EAGAIN && errno!=EINTR){failed=true;break;}
    }
    if(fds[0].revents&(POLLIN|POLLHUP|POLLERR)) {
      ssize_t n=read(output[0],inbound+have,sizeof inbound-have);
      if(n==0){failed=true;break;}
      if(n<0){if(errno==EAGAIN||errno==EINTR)continue;failed=true;break;}
      have+=(size_t)n;
      while(have>=5) {
        uint32_t length=((uint32_t)inbound[1]<<24)|((uint32_t)inbound[2]<<16)|((uint32_t)inbound[3]<<8)|inbound[4];
        if(length>PACKET_CAP){status("Bridge protocol frame exceeded 64 KiB",true);failed=true;break;}
        if(have<length+5)break;
        received((char)inbound[0],(char*)inbound+5,length);
        have-=length+5;memmove(inbound,inbound+length+5,have);
      }
      if(failed)break;
    }
  }
  close(input[1]);close(output[0]);
  kill(-child,SIGTERM);
  bool reaped=false;
  for(int i=0;i<50;i++) {
    if(waitpid(child,NULL,WNOHANG)==child){reaped=true;break;}
    struct timespec delay={0,10000000};nanosleep(&delay,NULL);
  }
  kill(-child,SIGKILL);if(!reaped)while(waitpid(child,NULL,0)<0 && errno==EINTR){}
  if(failed)status("Bridge disconnected; draft kept. Reopen with --resume THREAD",true);
  return NULL;
}

static char frame[MAX_ROWS][ROW_BYTES],previous[MAX_ROWS][ROW_BYTES];
static int prior_rows,prior_cols;
static int display_width(const char *text,size_t size) {
  int width=0;
  while(size) {
    mbstate_t state={0};wchar_t c;size_t n=mbrtowc(&c,text,size,&state);
    if(n==(size_t)-2)break;
    if(n==(size_t)-1||n==0){text++;size--;width++;continue;}
    int w=wcwidth(c);width+=w<0?1:w;text+=n;size-=n;
  }
  return width;
}
static void wrap(const char *text,int start,int end,int columns) {
  const char *origin=text;
  int row=start,width=0;size_t at=0;
  while(*text && row<end) {
    if(*text=='\n'){row++;at=0;width=0;text++;continue;}
    if(width && *text!=' ' && (text==origin||text[-1]==' ')) {
      int word=display_width(text,strcspn(text," \n"));
      if(word<=columns && width+word>columns){row++;at=0;width=0;if(row>=end)break;}
    }
    mbstate_t conversion={0};wchar_t c;
    size_t n=mbrtowc(&c,text,strlen(text),&conversion);
    if(n==(size_t)-2)break;
    bool invalid=n==(size_t)-1;
    if(invalid)n=1;
    int w=invalid?1:wcwidth(c);if(w<0)w=1;
    if(width+w>columns){row++;at=0;width=0;if(row>=end)break;}
    if(at+n>=ROW_BYTES)break;
    if(invalid)frame[row][at++]='?';else {memcpy(frame[row]+at,text,n);at+=n;}
    frame[row][at]=0;width+=w;text+=n;
  }
}
static void render(const char *draft) {
  struct winsize size={0};ioctl(STDOUT_FILENO,TIOCGWINSZ,&size);
  int rows=size.ws_row?size.ws_row:30,cols=size.ws_col?size.ws_col:80;
  if(rows<6)rows=6;if(rows>MAX_ROWS)rows=MAX_ROWS;
  if(cols<8)cols=8;if(cols>MAX_COLS)cols=MAX_COLS;
  memset(frame,0,sizeof frame);
  char info[768],response[TEXT_CAP],label[256];bool dead,truncated,busy;
  pthread_mutex_lock(&shared.lock);
  snprintf(info,sizeof info,"%s",shared.status);
  snprintf(label,sizeof label,"%s",shared.thread);
  memcpy(response,shared.response,shared.used+1);
  dead=shared.dead;truncated=shared.truncated;busy=shared.busy;
  pthread_mutex_unlock(&shared.lock);
  wrap("aesel / C prototype",0,1,cols-1);
  if(label[0])wrap(label,1,2,cols-1);
  // The prototype shows the end of the current reply; no scrollback database.
  const char *tail=response;size_t limit=(size_t)(rows-6)*(size_t)(cols-1);
  if(strlen(tail)>limit){tail+=strlen(tail)-limit;while(((unsigned char)*tail&0xc0)==0x80)tail++;}
  wrap(tail,3,rows-3,cols-1);
  char editor[INPUT_CAP+4];snprintf(editor,sizeof editor,"> %s",draft);
  const char *visible=editor;
  if(strlen(editor)>(size_t)(cols-2)){visible=editor+strlen(editor)-(cols-2);while(((unsigned char)*visible&0xc0)==0x80)visible++;}
  wrap(visible,rows-2,rows-1,cols-1);
  char footer[1024];
  snprintf(footer,sizeof footer,"%s%s%s%s",getenv("AESEL_BENCH_ROOT")?"@bench | ":"",dead?"offline | ":busy?"working | ":"",truncated?"reply truncated | ":"",info);
  wrap(footer,rows-1,rows,cols-1);
  printf("\033[?2026h\033[?25l");
  bool resized=rows!=prior_rows||cols!=prior_cols;
  if(resized)printf("\033[2J");
  for(int r=0;r<rows;r++)if(resized||strcmp(frame[r],previous[r]))printf("\033[%d;1H\033[2K%s",r+1,frame[r]);
  printf("\033[%d;%dH\033[?25h\033[?2026l",rows-1,display_width(frame[rows-2],strlen(frame[rows-2]))+1);fflush(stdout);
  memcpy(previous,frame,sizeof frame);prior_rows=rows;prior_cols=cols;
}

int main(int argc,char **argv) {
  const char *runtime="bun",*bridge=NULL,*resume=NULL,*model=NULL;
  for(int i=1;i<argc;i++) {
    if(!strcmp(argv[i],"--help")){puts("aesel-c --bridge FILE [--runtime bun|node] [--resume THREAD] [--model MODEL]\nTwo-thread terminal experiment. /quit, /retry, Ctrl-C stop, Ctrl-U clear.");return 0;}
    if(i+1>=argc){fprintf(stderr,"Missing option value\n");return 2;}
    if(!strcmp(argv[i],"--bridge"))bridge=argv[++i];
    else if(!strcmp(argv[i],"--runtime"))runtime=argv[++i];
    else if(!strcmp(argv[i],"--resume"))resume=argv[++i];
    else if(!strcmp(argv[i],"--model"))model=argv[++i];
    else{fprintf(stderr,"Unknown option: %s\n",argv[i]);return 2;}
  }
  if(!bridge||!isatty(STDIN_FILENO)||!isatty(STDOUT_FILENO)){fprintf(stderr,"An interactive terminal and --bridge FILE are required\n");return 2;}
  char *child_args[9];int n=0;child_args[n++]=(char*)runtime;child_args[n++]=(char*)bridge;
  if(resume){child_args[n++]="--resume";child_args[n++]=(char*)resume;}
  if(model){child_args[n++]="--model";child_args[n++]=(char*)model;}
  child_args[n]=NULL;shared.child_argv=child_args;
  if(pipe_flags(shared.ui_wake,true)||pipe_flags(shared.io_wake,true)){perror("pipe");return 1;}
  setlocale(LC_CTYPE,"");signal(SIGPIPE,SIG_IGN);
  struct sigaction action={0};action.sa_handler=on_signal;sigemptyset(&action.sa_mask);
  sigaction(SIGTERM,&action,NULL);sigaction(SIGHUP,&action,NULL);sigaction(SIGINT,&action,NULL);
  if(tcgetattr(STDIN_FILENO,&saved)){perror("tcgetattr");return 1;}
  struct termios raw=saved;
  raw.c_lflag&=~(ECHO|ICANON|IEXTEN|ISIG);raw.c_iflag&=~(IXON|ICRNL);raw.c_cc[VMIN]=1;raw.c_cc[VTIME]=0;
  if(tcsetattr(STDIN_FILENO,TCSANOW,&raw)){perror("tcsetattr");return 1;}
  terminal_active=true;atexit(restore);printf("\033[?1049h\033[?2004h");
  snprintf(shared.status,sizeof shared.status,"Connecting; you can type now");
  char draft[INPUT_CAP]={0};size_t used=0;render(draft);
  pthread_t worker;
  if(pthread_create(&worker,NULL,bridge_io,NULL)){status("Cannot start I/O thread",true);restore();return 1;}
  bool quit=false,paste=false;char escape[24];size_t escaped=0;
  while(!quit&&!interrupted) {
    struct pollfd fds[]={{STDIN_FILENO,POLLIN,0},{shared.ui_wake[0],POLLIN,0}};
    int result=poll(fds,2,100);
    if(result<0){if(errno==EINTR)continue;break;}
    bool dirty=false;
    if(fds[1].revents&POLLIN){drain(shared.ui_wake[0]);dirty=true;}
    if(fds[0].revents&(POLLHUP|POLLERR|POLLNVAL))break;
    if(fds[0].revents&POLLIN) {
      unsigned char keys[1024];ssize_t count=read(STDIN_FILENO,keys,sizeof keys);
      if(count<=0)break;
      for(ssize_t i=0;i<count;i++) {
        unsigned char c=keys[i];dirty=true;
        if(escaped) {
          if(escaped<sizeof escape-1)escape[escaped++]=(char)c;else escaped=0;
          if((c>='@'&&c<='~'&&c!='[')||escaped==0) {
            escape[escaped]=0;
            if(!strcmp(escape,"\033[200~"))paste=true;
            else if(!strcmp(escape,"\033[201~"))paste=false;
            escaped=0;
          }
          continue;
        }
        if(c==27){escape[0]=27;escaped=1;continue;}
        if(paste&&(c=='\r'||c=='\n'))c=' ';
        if(c==4){quit=true;break;}
        if(c==3) {
          pthread_mutex_lock(&shared.lock);bool busy=shared.busy;pthread_mutex_unlock(&shared.lock);
          if(busy)enqueue('I',NULL,0);else if(used){used=0;draft[0]=0;}else quit=true;
        } else if(c==21){used=0;draft[0]=0;}
        else if(c==127||c==8){if(used){do{used--;}while(used&&((unsigned char)draft[used]&0xc0)==0x80);draft[used]=0;}}
        else if(c=='\r'||c=='\n') {
          if(!strcmp(draft,"/quit")){quit=true;break;}
          bool submitted=false;
          if(!strcmp(draft,"/retry"))submitted=enqueue('R',NULL,0);
          else if(used) {
            pthread_mutex_lock(&shared.lock);bool busy=shared.busy;pthread_mutex_unlock(&shared.lock);
            if(busy)status("Still working; draft kept. Ctrl-C stops the turn",false);
            else submitted=enqueue('P',draft,used);
          }
          if(submitted){used=0;draft[0]=0;}
        } else if(c>=32 && used+1<sizeof draft){draft[used++]=(char)c;draft[used]=0;}
      }
    }
    struct winsize size={0};ioctl(STDOUT_FILENO,TIOCGWINSZ,&size);
    if(dirty||(size.ws_row&&size.ws_row!=prior_rows)||(size.ws_col&&size.ws_col!=prior_cols))render(draft);
  }
  // Restore the user's terminal immediately; join may wait for provider exit.
  restore();pthread_mutex_lock(&shared.lock);shared.stopping=true;pthread_mutex_unlock(&shared.lock);
  ping(shared.io_wake[1]);pthread_join(worker,NULL);
  for(int i=0;i<2;i++){close(shared.ui_wake[i]);close(shared.io_wake[i]);}
  return 0;
}
