/*
  # LICENSE
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  Copyright 2022, Furkan Semih DÜNDAR
  Email: f.semih.dundar@yandex.com


  In this code, we try to implement Barbour & Smolin's maximal
  variety function. The one suggested by David Deutsch.
  Barbour refers to arXiv:9203041

  This code is an optimized C translation of some parts of the
  original Haskell code (sv.hs) available under the GPL 3 license in
  https://github.com/fsdundar/string-variety

  In this code we represented a string as a unsigned long int whose
  maximum number of bits is 64. Therefore, this code can calculate the
  variety of strings of maximum length 64.
*/

#include "stdio.h"
#include "stdlib.h"
#include "omp.h"

#define NUM_THREADS 8

// A struct to represent positive rational numbers in the form num/denom.
struct frac{
  unsigned long int num;
  unsigned long int denom;
};

// adds two fracs
struct frac plus(struct frac a, struct frac b){
  struct frac c;
  c.num = a.num*b.denom + a.denom*b.num;
  c.denom = a.denom*b.denom;
	
  return c;
}

// Returns the minimum of two arguments
int min(unsigned long int a,unsigned long int b){
  if (a < b)
    return a;
  
  return b;
}

// Returns the greatest common divisor of a and b.
unsigned long int gcd(unsigned long int a,unsigned long int b){
  unsigned long int c = 1;
  unsigned long int i = 2;
  unsigned long int m = min(a,b);
  while (i <= m){
    if ((a%i == 0) && (b%i == 0)){
      a /= i;
      b /= i;
      c *= i;	
    }else
      i++;
    
  }
  
  return c;
}

// Simplifies a frac number.
struct frac simplify(struct frac a){
  unsigned long int n = a.num;
  unsigned long int d = a.denom;
  unsigned long int g = gcd(n,d);
	
  struct frac s;
  s.num = n/g;
  s.denom = d/g;
	
  return s;
}

// Returns 1 if a1 > a2
int greaterThan(struct frac a1, struct frac a2){
  unsigned long int n1 = a1.num;
  unsigned long int d1 = a1.denom;
  unsigned long int n2 = a2.num;
  unsigned long int d2 = a2.denom;

  return n1*d2 > n2*d1;
}

// Returns 1 if a1 == a2
int eq(struct frac a1, struct frac a2){
  a1 = simplify(a1);
  a2 = simplify(a2);

  return (a1.num == a2.num) && (a1.denom == a2.denom);
}

// Returns i mod n
int mod(int i, int n){
  while(i < 0)
    i += n;
	
  return i % n;
}

// Returns the bit at position i mod L
int bit_at(unsigned long int a, int i, int L){
  unsigned long int b = 1;
  int m = mod(i,L);

  b <<= m;

  if (b & a)
    return 1;

  return 0;
}


// Returns the mirror image of a number as a bit sequence
// L is the length of the bit sequence
unsigned long int mirrorNum(unsigned long int a, int L){
  unsigned long int b = 0;
  unsigned long int c = 1;
  int i;

  for(i = 0; i < L; i++){
    c = 1;
    if (bit_at(a,i,L)){
      c <<= L-i-1;
      b += c;
    }
  }

  return b;
}

// Cycles number 'a' by an amount of i
// L is the length of the bit sequence
unsigned long int numCycle(unsigned long int a, int i, int L){
  unsigned long int mask = 1;
  unsigned long int b = a;
  unsigned long int c = a;
  
  int m = mod(i,L);
  
  mask <<= L;
  mask--;
  
  b <<= m;
  b = b & mask;

  c >>= L-m;

  return b+c;
}

// Barbour p .13 Definition due to Deutsch
// L is the length of the bit sequence
unsigned long int nimNum(unsigned long int a, int i, int m, int L){
  

  unsigned long int b = numCycle(a, m-i, L);
  unsigned long int mask = 1;

  mask <<= 2*m+1;
  mask--;

  return b & mask;
}

// Returns m^*
int mStar(int L){
  int m;
	
  if (L%2 == 0)
    m = L/2 - 1;
  else
    m = (L-1)/2;
	
  return m;
}

// Barbour p. 14
// L is the length of the bit sequence
unsigned long int kijNum(unsigned long int a, int i, int j, int L){
  int ms = mStar(L);
  unsigned long int m;
  unsigned long int a1, a2;

  for(m = 1; m <= ms; m++){
    a1 = nimNum(a, i, m, L);
    a2 = nimNum(a, j, m, L);

    if ((a1 != a2) && (a1 != mirrorNum(a2, 2*m+1)))
      return m;

  }

  return 0;
}

// Barbour p. 14
// L is the length of the bit sequence
unsigned long int riNum(unsigned long int a, int i, int L){
  unsigned long int maximum = 0;
  unsigned long int k;
  int j;

  for(j = 0; j < L; j++){
    if (j != i){
      k = kijNum(a, i, j, L);
      if (maximum < k)
	maximum = k;
    }
  }
  
  return maximum;
}

// Barbour p. 14 eqn. (5)
// L is the length of the bit sequence
struct frac varietyPNum(unsigned long int a, int L){
  struct frac s, r;
  s.num = 0;
  s.denom = 1;

  int i;

  for(i = 0; i < L; i++){
    r.num = 1;
    r.denom = riNum(a,i,L);
    s = plus(s,r);
    s = simplify(s);
  }

  return s;
}


// Returns True if the configuration is Leibnizian in the sense of
// Barbour p. 17 and returns False otherwise.
unsigned long int leibnizianQNum(unsigned long int a, int L){
  int i, j;

  for (i = 0; i < L; i++)
    for (j = 0; j < i; j++)
      if (!kijNum(a,i,j,L))
	return 0;
  
  return 1;
}

// Converts a number to string. 1 -> "X", 0 -> "-".
// L is the length of the bit sequence
char *numToStr(unsigned long int a, int L){
  char *s = calloc(L, sizeof(char));
  unsigned long int b = 1;
  int i;
	
  for (i = 0; i < L; i++){
    if ((a & b) == 0)
      s[L-i-1] = '-';
    else
      s[L-i-1] = 'X';

    b <<= 1;
  }
	
  return s;
}


// Calculates the maximal variety of Leibnizian strings of length N.
// Returns a frac number.
struct frac maximalVarietyNum(int N){
  unsigned long int a;
  struct frac v, max;
  max.num = 0;
  max.denom = 1;

  int id;
  
  unsigned long int lim = 1;
  lim <<= N;

  omp_set_num_threads(NUM_THREADS);

  /* Due to symmetry 0 <-> 1 we can change the upper bound of for */
  /* loop to lim/2. */
  #pragma omp parallel for private(v)
  for (a = 0; a < lim/2; a+=2){
    if (leibnizianQNum(a+1, N)){
      v = varietyPNum(a+1, N);
      if (greaterThan(v, max)){
	max = v;
      }
    }
  }
  
  return max;
}


void printLeibnizianOfVar(struct frac given_var, int N){
  unsigned long int a;
  struct frac v;
  char *s;
  
  int id;
  
  unsigned long int lim = 1;
  lim <<= N;

  omp_set_num_threads(NUM_THREADS);

  given_var = simplify(given_var);

  #pragma omp parallel for private(v,s)
  for (a = 0; a < lim; a++){
    if (leibnizianQNum(a, N)){
      v = varietyPNum(a, N);
      if (eq(given_var, v)){
	s = numToStr(a, N);
	printf("%s\n", s);
	free(s);
      }
    }
  }


}

// Calculates the number of Leibnizian strings of length N
unsigned long int LeibnizianStrOfNum(int N){
  unsigned long int a;
  unsigned long int i = 0;
  
  unsigned long int lim = 1;
  lim <<= N;

  omp_set_num_threads(NUM_THREADS);

  #pragma omp parallel
  for (a = 0; a < lim; a++){
    if (leibnizianQNum(a, N)){
      i++;
    }
  }

      
  return i;
}


int main(){
  int N;
  struct frac m;

  // The following code block prints N vs Max. Variety.
  for (N = 6; N < 16; N++){
    m = maximalVarietyNum(N);
    printf("N = %d, Variety = %lu/%lu\n", N, m.num, m.denom);
  }

  

  /* m.num = 37; */
  /* m.denom = 3; */

  /* printLeibnizianOfVar(m, 30); */


  /* for (N = 6; N < 10; N++) */
  /*   printf("N:%d,%lu\n",N,LeibnizianStrOfNum(N)); */

  
  // FILE *f;

  // f = fopen("./foobar", "w");
  
  /* for (N = 20; N < 21; N++){ */
  /*   m = simplify(maximalVarietyNum(N)); */
  /*   printf("N: %d, MaximalVariety: %ld/%ld\n", N, m.num, m.denom); */
  /*   // fprintf(f, "N: %d, MaximalVariety: %ld/%ld\n", N, m.num, m.denom); */
  /* } */

  // fclose(f);

  return 0;
}
