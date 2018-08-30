int odd(int  x13);
int even(int  x2);
int odd(int  x25) {
  int x26 = x25;
  int x27 = x25 == 0;
  int x28;
  if (x27) {
    x28 = 1;
  } else {
    x28 = 0;
  }
  int x29 = x28 == 0;
  int x33;
  if (x29) {
    int x30 = x25 - 1;
    int x31 = even(x30);
    x33 = x31;
  } else {
    x33 = 0;
  }
  return x33;
}
int even(int  x35) {
  int x36 = x35;
  int x37 = x35 == 0;
  int x38;
  if (x37) {
    x38 = 1;
  } else {
    x38 = 0;
  }
  int x39 = x38 == 0;
  int x43;
  if (x39) {
    int x40 = x35 - 1;
    int x41 = odd(x40);
    x43 = x41;
  } else {
    x43 = 1;
  }
  return x43;
}
int Snippet(int  x0) {
  int x23 = odd(x0);
  return x23;
}
