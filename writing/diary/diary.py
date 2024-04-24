import time
import math
import datetime
import cmd
import sys
import csv
import os
import termios, fcntl, struct
import textwrap


showtimestamp = False
entry = []
entry_duration = 0.0
entry_start = 0.0
entries = []

def terminal_size():
    s = struct.pack("HHHH", 0, 0, 0, 0)
    try:
        fd_stdout = sys.stdout.fileno()
        x = fcntl.ioctl(fd_stdout, termios.TIOCGWINSZ, s)
        return struct.unpack("HHHH", x)
    except:
        return (5,80)

def timed_input():
    global entry
    global entry_duration
    global entry_start
    start = time.time()
    s = input("    ")
    if s == "...":
        s = ""
        interval = time.time() - start
        entry_duration = math.trunc((time.time() - entry_start))
        entry.append((interval,s))
        save_entry()
    else:
        interval = time.time() - start
        entry.append((interval,s))
        timed_input()
    return s, interval, len(entry)

def save_entry():
    global entry
    date = time.ctime(time.time())
    t = datetime.datetime.now()
    saved_entry = (t.strftime("%Y-%m-%d_%H-%M"))
    f = open('entries/'+saved_entry,'w')
    csv_writer = csv.writer(f, delimiter='\t')
    csv_writer.writerows(entry)
    entry = []    
    return "entry saved as "+saved_entry

def read_entry(chosen_entry): # make a csv reader here!
    global entry
    global entry_duration
    global wrapper
    
    try:
        f = open('entries/'+chosen_entry,'r')
    except:
        return print("entry not found")
    csv_reader = csv.reader(f, delimiter='\t')

    entry = []
    entry_duration = 0.0
    
    for row in csv_reader:
        tp = row
        tp[0] = float(tp[0]) / 6 
        entry.append(tp)
        entry_duration += tp[0]

    print("("+chosen_entry+")    "+str(len(entry)-1)+" messages | "+str(entry_duration)+" seconds")
    
    print("\n"*(terminal_size()[0]-3))

    time.sleep(3.0)

    print("\n")

    remaining_messages = len(entry)-1
    
    for s in entry:
        t = time.time()
        time.sleep(s[0])
        entry_duration -= s[0]
        remaining_messages -= 1
        timestamp = "\033[92m"+str(math.trunc(entry_duration)).zfill(4)+" | "+str(remaining_messages).zfill(4)+"\033[0m"
        message = s[1]
        

        if s == entry[len(entry)-1]:
            break

        if showtimestamp == True:
            print(timestamp.ljust(16),end="    ")
            wrappedmessage = textwrap.wrap(message,terminal_size()[1]-16)
            for s in wrappedmessage:
                if s == wrappedmessage[0]:
                    print(s)
                else:
                    print(" "*15+s)
        elif showtimestamp == False:
            wrappedmessage = textwrap.wrap(message,terminal_size()[1]-4)
            for s in wrappedmessage:
                print("    "+s)
            
    signature = "\033[93mJAS\033[0m"
    print("\n")
    print(signature.rjust(terminal_size()[1]-4))
    print("\n"*3)
    time.sleep(3.0)
    
def get_entries():
    global entries
    entries = []
    path="entries/"
    try:
        dirList=os.listdir(path)
        numbers = 1
    except:
        print("could not locate the directory of entries")
    for fname in dirList:
        if len(fname) == 16:
            #print(fname, "\033[91m"+str(numbers).zfill(4)+"\033[0m",sep="    ")
            entries.append((fname,str(numbers).zfill(4)))
            numbers += 1

    
class diary(cmd.Cmd):
    """Simple command processor example."""

    use_rawinput = False
    prompt = "\033[90m♂\033[0m   "
    undoc_header="commands"
    ruler = "^"
    
    def default(self,line):
        print("? (type help to get a list of commands)")

    def emptyline(self,line):
        print("? (type help to get a list of commands)")
    
    def do_we(self, line):
        global entry
        global entry_start
        print("---")
        time.sleep(0.1)
        print("\n"*(terminal_size()[0]-3))
        entry = []
        entry_start = time.time()
        timed_input()

    def do_le(self, line):# list all files in the entries directory
        global entries
        print("listing entries...")
        get_entries()
        for entry in entries:
            print(entry[0], "\033[91m"+entry[1]+"\033[0m",sep="    ")

    def do_de(self, entrynumber):
        global entries
        get_entries()
        for entry in entries:
            if entry[1] == entrynumber:
                entrynumber = entry[0]
        try:
            os.remove("entries/"+entrynumber)
            print("entry",entrynumber,"was erased")
        except:
            print("could not delete specified entry")
        

    def do_re(self, entrynumber):
        global entries
        get_entries()
        for entry in entries:
            if entry[1] == entrynumber:
                entrynumber = entry[0]
        read_entry(entrynumber)

    def do_ts(self, line):
        global showtimestamp
        if line == "True":
            showtimestamp = True
            print("time stamping enabled")
        elif line == "False":
            showtimestamp = False
            print("time stamping disabled")
        else:
            print("time stamping is",showtimestamp)

    def do_ra(self, line):
        print("reading all entries...")
        time.sleep(3.0)
        path="entries/"
        try:
            dirList=os.listdir(path)
            reverse(dirList)
        except:
            print("could not locate the directory of entries")
        for fname in dirList:
            if len(fname) == 16:
                read_entry(fname)
        
        
    def do_quit(self, line):
        try:
           sys.exit()
        except:
            print("\n\n    goodbye :)\n")
        return True

if __name__ == '__main__':

    print(("\n"*20)+"    \033[91mhello JAS!\033[0m")
    
    try:
        diary().cmdloop()
    except KeyboardInterrupt:
       try:
           sys.exit()
       except:
            print("\n\n    goodbye :)\n")
