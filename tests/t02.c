int a = 5;
extern int printf(const char *fmt, ...);
int main(void) {
    printf("%li %f\n", (long)a, (float)a);
}
