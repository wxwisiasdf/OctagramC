typedef unsigned int size_t;
typedef signed int ssize_t;

extern int printf(const char *fmt, ...);
int main(void) {
    size_t a = 0;
    ssize_t b = 0;
    printf("%i %i\n", (int)a, (int)b);
}
