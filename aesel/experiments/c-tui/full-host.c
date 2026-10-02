#define _DEFAULT_SOURCE
#define _DARWIN_C_SOURCE
#define _XOPEN_SOURCE 700
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#ifdef __APPLE__
#include <util.h>
#else
#include <pty.h>
#endif

// Raw PTY transport preserves every existing terminal feature. The private
// control pipe only arbitrates opening drafts vs sign-in/disclosure input.
enum { CAP=1048576, EARLY_CAP=65536, BOOT, GATE, READY };
typedef struct {unsigned char data[CAP];size_t start,used;} Ring;
static struct {
  pthread_mutex_t lock;
  Ring input,output;
  unsigned char early[EARLY_CAP];size_t early_used;
  int phase;bool done,stop,remote_output,control_lost,overflow;
  int wake_ui[2],wake_io[2],master,control,child_status;pid_t child;
  double began,ready_ms;
} state={.lock=PTHREAD_MUTEX_INITIALIZER,.phase=BOOT,.ready_ms=-1};
static struct termios original;
static int original_flags,original_input_flags;
static bool terminal_active;
static volatile sig_atomic_t stopped,resized;
static double now(void){struct timespec ts;clock_gettime(CLOCK_MONOTONIC,&ts);return ts.tv_sec+ts.tv_nsec/1e9;}
static void signal_stop(int signal){(void)signal;stopped=1;}
static void signal_resize(int signal){(void)signal;resized=1;}
static void ping(int fd){char byte=1;(void)write(fd,&byte,1);}
static void drain(int fd){char b[128];while(read(fd,b,sizeof b)>0){}}
static int make_pipe(int p[2]){
  if(pipe(p))return -1;
  for(int i=0;i<2;i++)if(fcntl(p[i],F_SETFD,FD_CLOEXEC)<0||fcntl(p[i],F_SETFL,O_NONBLOCK)<0)return -1;
  return 0;
}
static size_t put(Ring *r,const void *bytes,size_t n){
  if(n>CAP-r->used)n=CAP-r->used;
  size_t end=(r->start+r->used)%CAP,first=CAP-end;if(first>n)first=n;
  memcpy(r->data+end,bytes,first);memcpy(r->data,(const char*)bytes+first,n-first);r->used+=n;return n;
}
static size_t peek(Ring *r,void *bytes,size_t n){
  if(n>r->used)n=r->used;size_t first=CAP-r->start;if(first>n)first=n;
  memcpy(bytes,r->data+r->start,first);memcpy((char*)bytes+first,r->data,n-first);return n;
}
static void consumed(Ring *r,size_t n){r->start=(r->start+n)%CAP;r->used-=n;}
static void restore(void){
  if(!terminal_active)return;
  tcsetattr(STDIN_FILENO,TCSANOW,&original);fcntl(STDIN_FILENO,F_SETFL,original_input_flags);fcntl(STDOUT_FILENO,F_SETFL,original_flags);
  const char *reset="\033[?1000l\033[?1002l\033[?1003l\033[?1006l\033[?2004l\033[?25h\033[?1049l";
  (void)write(STDOUT_FILENO,reset,strlen(reset));terminal_active=false;
}
static void phase(const char *line){
  int next=!strcmp(line,"AESEL/1 ready")?READY:!strcmp(line,"AESEL/1 gate")?GATE:!strcmp(line,"AESEL/1 boot")?BOOT:-1;
  if(next<0)return;
  pthread_mutex_lock(&state.lock);
  if(next==READY){
    // At most 64 KiB of early input plus the bounded live input queue. Never
    // consume opening bytes in an account or consent question.
    if(CAP-state.input.used>=state.early_used){put(&state.input,state.early,state.early_used);state.early_used=0;}
    else {state.overflow=true;next=BOOT;}
    if(state.ready_ms<0&&next==READY)state.ready_ms=(now()-state.began)*1000;
  }
  state.phase=next;
  pthread_mutex_unlock(&state.lock);ping(state.wake_ui[1]);
}
static void *transport(void *unused){
  (void)unused;char control_line[80];size_t line_used=0;bool eof=false,control_open=true;
  unsigned char buffer[32768];
  while(!eof){
    pthread_mutex_lock(&state.lock);
    bool stop=state.stop;size_t room=CAP-state.output.used,pending=state.input.used;
    pthread_mutex_unlock(&state.lock);
    if(stop)break;
    struct pollfd fds[]={{state.master,(short)((room?POLLIN:0)|(pending?POLLOUT:0)),0},
      {control_open?state.control:-1,POLLIN,0},{state.wake_io[0],POLLIN,0}};
    int result=poll(fds,3,100);if(result<0){if(errno==EINTR)continue;break;}
    if(fds[2].revents&POLLIN)drain(state.wake_io[0]);
    // Process ownership changes before accepting another batch of input.
    if(control_open&&(fds[1].revents&(POLLIN|POLLHUP|POLLERR))){
      char messages[512];ssize_t n=read(state.control,messages,sizeof messages);
      if(n==0){control_open=false;pthread_mutex_lock(&state.lock);state.control_lost=true;pthread_mutex_unlock(&state.lock);}
      for(ssize_t i=0;i<n;i++){
        if(messages[i]=='\n'){control_line[line_used]=0;phase(control_line);line_used=0;}
        else if(line_used+1<sizeof control_line)control_line[line_used++]=messages[i];else line_used=0;
      }
    }
    if(pending&&(fds[0].revents&POLLOUT)){
      pthread_mutex_lock(&state.lock);size_t n=peek(&state.input,buffer,sizeof buffer);pthread_mutex_unlock(&state.lock);
      ssize_t wrote=write(state.master,buffer,n);
      if(wrote>0){pthread_mutex_lock(&state.lock);consumed(&state.input,(size_t)wrote);pthread_mutex_unlock(&state.lock);ping(state.wake_ui[1]);}
      else if(wrote<0&&errno!=EAGAIN&&errno!=EINTR)eof=true;
    }
    if(room&&(fds[0].revents&(POLLIN|POLLHUP|POLLERR))){
      size_t want=room<sizeof buffer?room:sizeof buffer;
      ssize_t n=read(state.master,buffer,want);
      if(n>0){pthread_mutex_lock(&state.lock);put(&state.output,buffer,(size_t)n);state.remote_output=true;pthread_mutex_unlock(&state.lock);ping(state.wake_ui[1]);}
      else if(n==0||(n<0&&errno!=EAGAIN&&errno!=EINTR))eof=true;
    }
  }
  pthread_mutex_lock(&state.lock);bool requested=state.stop;pthread_mutex_unlock(&state.lock);
  // Let Aesel interrupt its provider and save first. Killing the provider's
  // process group at the same instant races that checkpoint sequence.
  if(requested)kill(state.child,SIGTERM);
  int child_status=0;bool reaped=false;
  for(int i=0;i<600;i++){
    pid_t got=waitpid(state.child,&child_status,WNOHANG);
    if(got==state.child||got<0){reaped=true;break;}
    struct timespec delay={0,10000000};nanosleep(&delay,NULL);
  }
  if(!reaped){kill(-state.child,SIGKILL);while(waitpid(state.child,&child_status,0)<0&&errno==EINTR){}}
  else kill(-state.child,SIGTERM);
  close(state.master);close(state.control);
  pthread_mutex_lock(&state.lock);state.child_status=child_status;state.done=true;pthread_mutex_unlock(&state.lock);
  ping(state.wake_ui[1]);return NULL;
}
static void opening_frame(const unsigned char *draft,size_t size){
  struct winsize w={0};ioctl(STDOUT_FILENO,TIOCGWINSZ,&w);
  unsigned columns=w.ws_col?w.ws_col:80;if(columns<8)columns=8;
  char frame[EARLY_CAP+256];
  int start=snprintf(frame,sizeof frame,"\033[H\033[2JAesel starting\r\n%s> ",getenv("AESEL_BENCH_ROOT")?"@bench\r\n":"");
  // This preview is disposable. The exact byte stream, including all editing
  // keys and bracketed paste delimiters, is replayed to the authoritative editor.
  unsigned char visible[EARLY_CAP];size_t used=0;bool escape=false;
  for(size_t i=0;i<size;i++){
    unsigned char c=draft[i];
    if(escape){if(c>='@'&&c<='~'&&c!='[')escape=false;continue;}
    if(c==27){escape=true;continue;}
    if(c==21||c=='\r'||c=='\n'){used=0;continue;}
    if(c==127||c==8){if(used){do{used--;}while(used&&(visible[used]&0xc0)==0x80);}continue;}
    if(c>=32)visible[used++]=c;
  }
  size_t offset=used>columns-4?used-(columns-4):0;
  while(offset<used&&(visible[offset]&0xc0)==0x80)offset++;
  memcpy(frame+start,visible+offset,used-offset);size_t count=(size_t)start+used-offset;
  memcpy(frame+count,"\033[?25h",6);count+=6;
  pthread_mutex_lock(&state.lock);if(!state.remote_output)put(&state.output,frame,count);pthread_mutex_unlock(&state.lock);
}

int main(int argc,char **argv){
  if(argc<3||strcmp(argv[1],"--")){fprintf(stderr,"usage: aesel-native -- PROGRAM [ARGUMENT ...]\n");return 2;}
  if(!isatty(0)||!isatty(1)){fprintf(stderr,"aesel-native requires a terminal\n");return 2;}
  state.began=now();
  if(tcgetattr(0,&original)){perror("termios");return 1;}
  original_flags=fcntl(1,F_GETFL);
  original_input_flags=fcntl(0,F_GETFL);
  struct termios raw=original;
  raw.c_lflag&=~(ECHO|ICANON|IEXTEN|ISIG);raw.c_iflag&=~(IXON|ICRNL);
  raw.c_oflag&=~OPOST;
  raw.c_cc[VMIN]=1;raw.c_cc[VTIME]=0;
  if(tcsetattr(0,TCSANOW,&raw)){perror("raw terminal");return 1;}
  terminal_active=true;atexit(restore);signal(SIGPIPE,SIG_IGN);
  struct sigaction action={0};sigemptyset(&action.sa_mask);action.sa_handler=signal_stop;
  sigaction(SIGTERM,&action,NULL);sigaction(SIGHUP,&action,NULL);sigaction(SIGINT,&action,NULL);
  action.sa_handler=signal_resize;sigaction(SIGWINCH,&action,NULL);
  const char *enter="\033[?1049h\033[?2004h";(void)write(1,enter,strlen(enter));
  // Show the editor before creating any provider/runtime process.
  opening_frame(NULL,0);unsigned char buffer[32768];
  size_t initial=peek(&state.output,buffer,sizeof buffer);ssize_t first=write(1,buffer,initial);
  if(first>0)consumed(&state.output,(size_t)first);
  int controls[2];
  if(make_pipe(controls)||make_pipe(state.wake_ui)||make_pipe(state.wake_io)){perror("pipe");return 1;}
  struct winsize size={0};ioctl(1,TIOCGWINSZ,&size);
  char tty[256];snprintf(tty,sizeof tty,"%s",ttyname(0)?ttyname(0):"");
  // Fork before starting our I/O thread. The child has a controlling PTY, so
  // native provider tools and Node's terminal APIs retain their usual semantics.
  struct termios inner=raw;inner.c_oflag=original.c_oflag;
  state.child=forkpty(&state.master,NULL,&inner,&size);
  if(state.child<0){perror("forkpty");return 1;}
  if(state.child==0){
    close(controls[0]);if(controls[1]!=3){dup2(controls[1],3);close(controls[1]);}
    fcntl(3,F_SETFD,0);fcntl(3,F_SETFL,0);
    setenv("AESEL_NATIVE_CONTROL_FD","3",1);
    if(!getenv("SLAB_TERMINAL_TTY")&&tty[0])setenv("SLAB_TERMINAL_TTY",tty,1);
    execvp(argv[2],argv+2);perror("aesel-native exec");_exit(127);
  }
  close(controls[1]);state.control=controls[0];
  fcntl(state.master,F_SETFD,FD_CLOEXEC);fcntl(state.master,F_SETFL,O_NONBLOCK);
  fcntl(1,F_SETFL,original_flags|O_NONBLOCK);
  fcntl(0,F_SETFL,original_input_flags|O_NONBLOCK);
  pthread_t worker;
  if(pthread_create(&worker,NULL,transport,NULL)){kill(-state.child,SIGKILL);waitpid(state.child,NULL,0);return 1;}
  bool finished=false;
  while(!stopped&&!finished){
    pthread_mutex_lock(&state.lock);
    size_t outgoing=state.output.used,available=state.phase==BOOT?EARLY_CAP-state.early_used:CAP-state.input.used;
    bool done=state.done;
    pthread_mutex_unlock(&state.lock);
    if(done&&!outgoing)break;
    struct pollfd fds[]={{0,available?POLLIN:0,0},{state.wake_ui[0],POLLIN,0},{1,outgoing?POLLOUT:0,0}};
    int result=poll(fds,3,100);
    if(result<0){if(errno==EINTR)continue;break;}
    if(fds[1].revents&POLLIN)drain(state.wake_ui[0]);
    if(fds[0].revents&(POLLHUP|POLLERR|POLLNVAL))break;
    if(outgoing&&(fds[2].revents&POLLOUT)){
      pthread_mutex_lock(&state.lock);size_t n=peek(&state.output,buffer,sizeof buffer);pthread_mutex_unlock(&state.lock);
      ssize_t count=write(1,buffer,n);
      if(count>0){pthread_mutex_lock(&state.lock);consumed(&state.output,(size_t)count);pthread_mutex_unlock(&state.lock);ping(state.wake_io[1]);}
      else if(count<0&&errno!=EAGAIN&&errno!=EINTR)break;
    }
    if(available&&(fds[0].revents&POLLIN)){
      unsigned char draft[EARLY_CAP];size_t draft_size=0;bool preview=false;
      pthread_mutex_lock(&state.lock);
      size_t room=state.phase==BOOT?EARLY_CAP-state.early_used:CAP-state.input.used;
      size_t want=room<sizeof buffer?room:sizeof buffer;
      if(!want){pthread_mutex_unlock(&state.lock);continue;}
      // fd 0 is nonblocking: input ownership and buffer reservation stay atomic
      // without ever waiting for a keystroke while holding the mutex.
      ssize_t count=read(0,buffer,want);
      if(count<=0){pthread_mutex_unlock(&state.lock);if(count<0&&(errno==EAGAIN||errno==EINTR))continue;break;}
      if(state.phase==BOOT){
        size_t n=(size_t)count;
        if(n>EARLY_CAP-state.early_used)n=EARLY_CAP-state.early_used;
        for(size_t i=0;i<n;i++)if(buffer[i]==3||buffer[i]==4){finished=true;break;}
        memcpy(state.early+state.early_used,buffer,n);state.early_used+=n;
        if(!state.remote_output){memcpy(draft,state.early,state.early_used);draft_size=state.early_used;preview=true;}
      }else put(&state.input,buffer,(size_t)count);
      pthread_mutex_unlock(&state.lock);
      if(preview)opening_frame(draft,draft_size);ping(state.wake_io[1]);
    }
    if(resized){ioctl(1,TIOCGWINSZ,&size);ioctl(state.master,TIOCSWINSZ,&size);kill(state.child,SIGWINCH);resized=0;}
  }
  restore();pthread_mutex_lock(&state.lock);state.stop=true;pthread_mutex_unlock(&state.lock);ping(state.wake_io[1]);
  pthread_join(worker,NULL);
  const char *trace_path=getenv("AESEL_NATIVE_TRACE");
  if(trace_path){
    FILE *trace=fopen(trace_path,"w");
    if(trace){fprintf(trace,"{\"core_ready_ms\":%.3f,\"pending_input_bytes\":%zu,\"control_lost\":%s}\n",state.ready_ms,state.early_used,state.control_lost?"true":"false");fclose(trace);}
  }
  for(int i=0;i<2;i++){close(state.wake_ui[i]);close(state.wake_io[i]);}
  return stopped?128:WIFEXITED(state.child_status)?WEXITSTATUS(state.child_status):128;
}
