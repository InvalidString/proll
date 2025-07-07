#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>

void* mmap_file(char* path, size_t *size){
    int fd;
    if ((fd = open(path, O_RDONLY)) == -1)
        fprintf(stderr, "%s: %s\n", path, strerror(errno)), exit(1);

    struct stat fs;
    if (fstat(fd, &fs) == -1) {
        perror("fstat"), exit(1);
    }
    void* data = mmap(0, fs.st_size, PROT_READ|PROT_WRITE, MAP_PRIVATE, fd, 0);
    *size = fs.st_size;
    return data;
}
