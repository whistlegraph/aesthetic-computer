#include <assert.h>
#include <stdlib.h>
#include "log-mount.h"
static int lookup(const char *table,unsigned long id,char *device) {
    FILE *f=tmpfile();assert(f);fputs(table,f);rewind(f);
    int mounted=ac_log_mount_record(f,id,device,32);fclose(f);return mounted;
}
int main(int argc,char **argv) {
    if(argc==2) {
        char device[32]="";
        assert(ac_log_existing_mount(device,sizeof(device))==atoi(argv[1]));
        return 0;
    }
    // The visible config volume is selected by fdinfo's mount ID. Hidden
    // entries and ordering of /proc/mountinfo cannot change that decision.
    const char *stack="31 1 8:2 / /mnt rw - vfat /dev/sda2 rw\n"
        "45 31 8:1 / /mnt rw - vfat /dev/sda1 rw\n"
        "20 1 0:1 / / rw - rootfs rootfs rw\n";
    char device[32]="";
    assert(lookup(stack,45,device));assert(!strcmp(device,"/dev/sda1"));
    assert(lookup(stack,31,device));assert(!strcmp(device,"/dev/sda2"));
    // Directory on rootfs is not a mounted /mnt: normal USB discovery runs.
    device[0]=0;assert(!lookup(stack,20,device));assert(!device[0]);
    assert(!lookup(stack,999,device));
    // Preserve unfamiliar mounts too; never hide one to obtain a log file.
    assert(lookup("57 20 0:4 / /mnt ro - tmpfs tmpfs ro\n",57,device));
    assert(!device[0]);
    puts("PASS existing visible mount reuse, stacked volumes, unmounted fallback");
}
