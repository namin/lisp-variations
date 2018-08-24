int factorial(int arg);
int factorial(int  x15) {
  int x16 = x15;
  int x17 = x15 < 2;
  int x18;
  if (x17) {
    x18 = 1;
  } else {
    x18 = 0;
  }
  int x19 = x18 == 0;
  int x24;
  if (x19) {
    int x20 = x15 - 1;
    int x21 = factorial(x20);
    int x22 = x15 * x21;
    x24 = x22;
  } else {
    x24 = x15;
  }
  return x24;
}
int Snippet(int  x0) {
  int x13 = factorial(x0);
  return x13;
}
