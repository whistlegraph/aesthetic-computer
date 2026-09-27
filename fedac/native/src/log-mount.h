#ifndef AC_LOG_MOUNT_H
#define AC_LOG_MOUNT_H
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

// Resolve the kernel's visible mount ID, not the first/last /mnt entry:
// older native versions may have left several volumes stacked at /mnt.
static int ac_log_mount_record(FILE *mountinfo, unsigned long visible_id,
    char *device, size_t capacity) {
    char line[4096];
    while (fgets(line, sizeof(line), mountinfo)) {
        unsigned long id=0;
        char target[256];
        if (sscanf(line,"%lu %*u %*s %*s %255s",&id,target)!=2 ||
            id!=visible_id || strcmp(target,"/mnt")) continue;
        char *fields=strstr(line," - ");
        char source[256]="";
        if(fields && sscanf(fields," - %*s %255s",source)==1 &&
            !strncmp(source,"/dev/",5) && strlen(source)<capacity)
            snprintf(device,capacity,"%s",source);
        return 1; // Even an unfamiliar mount must not be shadowed.
    }
    return 0;
}
static int ac_log_existing_mount(char *device, size_t capacity) {
    int fd=open("/mnt",O_RDONLY|O_DIRECTORY);
    if(fd<0)return 0;
    char path[64],line[256];unsigned long id=0;
    snprintf(path,sizeof(path),"/proc/self/fdinfo/%d",fd);
    FILE *info=fopen(path,"r");
    if(info) {
        while(fgets(line,sizeof(line),info))
            if(sscanf(line,"mnt_id: %lu",&id)==1)break;
        fclose(info);
    }
    FILE *mounts=id?fopen("/proc/self/mountinfo","r"):NULL;
    int found=mounts?ac_log_mount_record(mounts,id,device,capacity):0;
    if(mounts)fclose(mounts);
    close(fd);
    return found;
}
#endif
