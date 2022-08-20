There was an issue with a unit test on Mac with the previous version. It 
failed, because the Delaunay triangulation is not unique, and on Mac the 
result is different than the result obtained with Windows or Unix, but it is 
correct. So I relaxed this unit test. 


## Testing environments

- local R 4.1.2 installation, Linux Mint 20.3
- win-builder
- Ubuntu 20, via Github action
- mac-builder


## R CMD check results

OK.
