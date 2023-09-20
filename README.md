# String-Variety

**Author:** Furkan Semih Dündar

**E-mail:** f.semih.dundar@yandex.com

# About

This GitHub repository contains two source codes for calculation of variety of
a string that consists of two letters, a function to determine whether a given
string is Leibnizian and so on. The codes are licensed under the GPL3 license.

## The Haskell Code (`sv.hs`)

This is the code written for Ref. [1]. It can be used to calculate, for example,
maximum variety for $N \leq 20$ where $N$ is the length of character string.
Because it is not optimized, it is hard to move to larger values of
$N$. 

## The C Code (`svParallel.c`)

Because it was harder to move to larger values of $N$, we have written this
optimized C code. Instead of keeping the Leibnizian string as a chracter array
we store a string as an `unsigned long int` variable. This allows calculations
up to strings of length 64. Moreover the operations on the strings are done
using bit operations.

The code uses parallelization. However, in its current form, it is not possible
to run it on different nodes of a cluster because of implementation of parallelization.
At the moment, it is possible to run the C code on a single node.

On GNU/Linux, you should be able to compile the C code using:

`gcc -fopenmp svParallel.c -o svParallel`

Last but not least, do not forget to change the line `#define NUM_THREADS 8` 
at the beginning of the code to suit your number of threads.

# Results

## Tables

Here we present two tables that list various results we obtained. In order to
decide which publication to cite, please read the table captions.

**Table 1.** N (string length) and MV (maximum variety). Maximum varieties for
$N \leq 20$ have been published in Ref. [1] and for $21 \leq N \leq 37$ have
been published in Ref. [2].

| N  	| MV  	| N  	| MV  	|
|:---:	|:---:	|:---:	|:---:	|
| 6  	| 4  	| 21  	| 10  	|
| 7  	| 5  	| 22  	| 31/3 	|
| 8  	| 6  	| 23  	| 31/3 	|
| 9  	| 17/3 	| 24  	| 32/3 	|
| 10  	| 37/6	| 25  	| 11  	|
| 11 	| 20/3	| 26  	| 34/3 	|
| 12  	| 8  	| 27  	| 23/2 	|
| 13  	| 23/3 	| 28  	| 12  	|
| 14  	| 49/6 	| 29  	| 12  	|
| 15  	| 49/6 	| 30  	| 37/3 	|
| 16  	| 9  	| 31  	| 38/3 	|
| 17  	| 26/3 	| 32  	| 13  	|
| 18  	| 55/6 	| 33  	| 40/3 	|
| 19  	| 29/3 	| 34  	| 40/3 	|
| 20  	| 59/6 	| 35  	| 41/3 	|
|   	|   	| 36  	| 14  	|
|   	|   	| 37  	| 43/3 	|


**Table 2.** N (string length) and #MVLS (number of maximum variety Leibnizian
modulo symmetries strings). Symmetries are mirror reflection and cyclic rotations.
These values have been published in Ref. [2].

| N  	| #MVLS	| N  	| #MVLS	|
|:---:	|:---:	|:---:	|:---:	|
| 6  	| 1  	| 21  	| 1  	|
| 7  	| 1  	| 22  	| 13 	|
| 8  	| 1	    | 23  	| 48 	|
| 9  	| 2 	| 24  	| 18 	|
| 10  	| 1 	| 25  	| 18 	|
| 11 	| 2 	| 26  	| 20 	|
| 12  	| 2 	| 27  	| 12 	|
| 13  	| 2 	| 28  	| 14 	|
| 14  	| 2 	| 29  	| 72 	|
| 15  	| 2 	| 30  	| 7 	|
| 16  	| 1  	| 31  	| 70 	|
| 17  	| 3 	| 32  	| 58 	|
| 18  	| 2 	| 33  	| 48 	|
| 19  	| 1 	| 34  	| 377 	|
| 20  	| 2 	| 35  	| 264 	|

## Maximal Variety Strings

List of all Leibnizian maximal variety strings (modulo symmetries, which are 
mirror reflections and cyclic rotations)
for $N \leq 35$ has been calculated as part of Ref. [2], using the
resources at "UHeM" (see **Acknowledgements**). They are provided here in the
folder **MVLS**. The list of strings are licensed under the
[CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/) license.

# Acknowledgements

Computing resources used in calculating the maximum variety ($20 < N \leq 37$)
and listing the maximal variety Leibnizian strings ($N \leq 35$) were provided
by the National Center for High Performance Computing of Turkey (UHeM) under grant
number 1011732022.

# Citation

If you use the code in your studies, please acknowledge it by citing the article published in Complex Systems as well as the url of this repo.

# References
1. Furkan Semih Dündar. "A use of variety as a law of the universe".
Complex Systems. Vol. 31, Issue 2, pp. 247--260. 2022. [https://doi.org/10.25088/ComplexSystems.31.2.247](https://doi.org/10.25088/ComplexSystems.31.2.247)

2. Furkan Semih Dündar. "Maximal variety Leibnizian strings for large N".
 9th International Congress on Fundamental and Applied Sciences. İstanbul/Turkey,
June 28--30, 2022. [Proceeding Book](http://icfas2022.intsa.org/belg/ICFAS2022_Book_v2.pdf)
