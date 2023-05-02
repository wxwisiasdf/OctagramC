struct st {
    int a;
    int b;
    int c;
} st;

extern int printf(const char *fmt, ...);
int main(void) {
    struct st s;
    s.a = 5;
    printf("%i\n", s.a);
}
