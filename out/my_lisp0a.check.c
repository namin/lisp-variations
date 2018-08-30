int my(int  x2);
int foo(int  x2, int  x5);
int my(int  x16) {
  int x17 = x16;
  int x24 = foo(x17,1);
  int x25 = foo(x17,2);
  int x26 = x24 + x25;
  return x26;
}
int foo(int  x16, int  x28) {
  int x29 = x28;
  int x17 = x16;
  int x30 = x17;
  int x31 = x28 + x30;
  return x31;
}
int Snippet(int  x0) {
  int x14 = my(x0);
  return x14;
}
