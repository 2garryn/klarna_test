## Solutions for Klarna tasks.

1. Maskify function: [first.erl](https://github.com/2garryn/klarna_test/blob/master/src/first.erl)
2. Add correct ordinal indicator suffix to number: [second.erl](https://github.com/2garryn/klarna_test/blob/master/src/second.erl)
3. Calculator: [third.erl](https://github.com/2garryn/klarna_test/blob/master/src/third.erl)

Simply execute <code>make</code> to compile and execute tests.
Example: 

```
$ make
rm -f ebin/*.beam
erlc -o ebin/ src/*
./rebar3 eunit --dir ebin/
===> Verifying dependencies...
===> Performing EUnit tests...
.....

Top 5 slowest tests (0.010 seconds, 7.3% of total time):
  second:compare_test/0
    0.010 seconds
  third:calculator_test/0: module 'third'
    0.000 seconds
  first:first_test/0: module 'first'
    0.000 seconds
  second:v1_test/0
    0.000 seconds
  second:v2_test/0
    0.000 seconds

Finished in 0.137 seconds
5 tests, 0 failures
```
