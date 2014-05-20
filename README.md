# DLEAK: a dynamic memory leak debugger

Dleak grew out of frustration, searching  for   a  tool  for memory leak
detection and inconsistent handling  of   allocation  functions in Linux
that satisfies the following criteria:

  - No significant impact on memory usage
  - No huge impact on runtime performance
  - Thread-safe
  - Able to handle dynamically loaded shared objects
  - Provide a substantial (stack) context where memory was allocated
  - If possible, be able to run on unmodified executables

There are zillions of tools for Linux, but  none was able to do the job,
so I decided to roll my own.  It   combines  some  techniques I found in
various slides and blogs to  wrap   the  Linux  allocation functions and
dynamically print stack traces.

There are lots of things that could be improved.  To name a few:

  - Have a possibilty to inject messages into the log stream from the
    application
  - Add real time stamps
  - Add thread information
  - Provide a nice graphical display that shows the real time and allows
    you getting information about the gathered contexts

## License

Dleak is public domain software. If you have improvements, please send a
GitHub pull request.


