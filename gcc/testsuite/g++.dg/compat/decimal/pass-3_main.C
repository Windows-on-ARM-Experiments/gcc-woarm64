/* { dg-require-effective-target dfp } */
/* { dg-require-effective-target dfprt } */

/* Test passing decimal scalars and classes by value.  */

extern void pass_3_x (void);
int fails;

int
main ()
{
  pass_3_x ();
  return 0;
}
