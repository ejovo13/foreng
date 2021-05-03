## Exercises

This folder contains a mangled list of .f90 source files that are my solutions to the exercises that come at the end of each chapter.
Some of the exercises are questions about the behavior of specific snippets of code, or how many times a loop is run. Therefore, not all of 
the source files contained in this directory are compileable Fortran programs. You can see for yourself which programs will be compiled by consulting
the CMakeLists.txt file in each subdirectory. These exercises are in their own directory because I didn't want to provide documentation for incomplete programs.
FORD ([FORtran Documentation](https://github.com/cmacmackin/ford)) automatically creates documentation for all the files found in the source folder, therefore these exercises 
are kept in a separate location.

Some of the exercises solved interesting problems and demanded specific functions to be written. All of the procedures that I found worthy to be documented for 
reuse can be found in the ../src directory, consolidated into separate modules.