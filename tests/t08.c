int arr[16];

int f(int n) {
    return &arr[n];
}

int g(int n) {
    return *(arr + n);
}

int h(int n) {
    return *arr + n;
}
