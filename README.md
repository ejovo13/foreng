## Fortran for Scientists and Engineers 

Fortran is the dominant language used in HPC and scientific computing applications. In fact, according to the [May 2021 TIOBE](https://www.tiobe.com/tiobe-index/) Index, Fortran is now back in the top 20, a jump from 34 since last year's ranking. To learn this archaic (some would say "dinosaur") language, I worked through the exercises in the book [Fortran for Scientists and Engineers](https://www.amazon.com/FORTRAN-SCIENTISTS-ENGINEERS-Stephen-Chapman/dp/0073385891) which focused on taking advantages of the modern features (2008/2015 Standard) when writing new code.

This repository is a collection of my solutions to a majority of the exercises. My motivations for learning Fortran are because I am currently working as a research assistant in the physics department of Kalamazoo College, and I am working with Fortran source code. I'm also quite interested in breaking through into the field of Numerical Analysis. Working through the textbook exercises taught me how to write high quality, well-documented code for Engineering applications. 

## Build

Foreng can be built with Cmake. Because chapter 17 explores parallel programming, Foreng depends on OpenMP and Coarray libraries.

## Documentation

Demonstrations of some of the more interesting problems have been compiled into the "Foreng" module, with complete documentation [here](https://ejovo13.github.io/foreng/)

