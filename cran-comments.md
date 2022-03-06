## Note

R CMD CHECK complained on mac-builder because of the presence of an `assert` 
in the compiled C++ code. This is why I added `#define NDEBUG` in the 
`delaunay.cpp` file. The issue has gone thanks to this line.


## Testing environments

- local R 4.1.2 installation, Windows 10
- win-builder
- Ubuntu 20, via Github action
- mac-builder


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
