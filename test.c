#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
    FILE *fp = fopen(argv[1], "r");
    if(fp != NULL) {
        char linebuf[24];
        while(fgets(linebuf, sizeof(linebuf), fp) != NULL) {
            if(linebuf[0] != '\0') {
                const char *cmd = "./occ -print-ast ";
                char cmdbuf[80];
                char *p = cmdbuf;
                size_t l_len = strlen(linebuf);
                size_t c_len = strlen(cmd);
                FILE *t_fp = fopen(linebuf, "r");

                memcpy(p, cmd, c_len);
                p[c_len] = '\0';
                p += c_len;
                memcpy(p, linebuf, l_len);
                p[l_len] = '\0';
                p += l_len;

                fclose(t_fp);
            }
        }
        fclose(fp);
    }
    return 0;
}
