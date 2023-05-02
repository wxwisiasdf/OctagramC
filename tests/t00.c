int f(int a, int b) {
    return a + b;
}

extern int printf(const char *fmt, ...);
int main(void) {
    printf("%i = 4\n", f(2, 2));
}
