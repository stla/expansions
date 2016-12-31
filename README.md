# The `expansions` package
Expansions of integer and real numbers, and odometers.

## Installation

To install on Windows (64-bit only):

```r
devtools::find_rtools()
devtools::install_github('stla/expansions', args="--no-multiarch")
```

On Linux:

```r
devtools::install_github('stla/expansions')
```

## Examples 

### Decimal integer to integer base:


```r
n <- 14
(n_to_base3 <- intAtBase(n, base=3) )
## [1] 2 1 1
sum(n_to_base3*c(1,3,3^2)) == n
## [1] TRUE
```

### Cantor expansion:


```r
# Cartesian product {0,1}x{0,1,2}x{0,1}:
sapply(0:11, function(x) intToAry(x, sizes=c(2,3,2)))
##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
## [1,]    0    1    0    1    0    1    0    1    0     1     0     1
## [2,]    0    0    1    1    2    2    0    0    1     1     2     2
## [3,]    0    0    0    0    0    0    1    1    1     1     1     1
```

### Float expansion in integer base, number between 0 and 1:


```r
x <- 0.625
( x_to_base2 <- floatExpand01(0.625, base=2) )
## [1] 1 0 1
sum(x_to_base2 / 2^(1:3)) == x
## [1] TRUE
```

### Float expansion in integer base, positive number:


```r
x <- 1.125
( x_to_base2 <- floatExpand(1.125, base=2) )
## $digits
## [1] 1 0 0 1
## 
## $exponent
## [1] 1
sum(x_to_base2$digits / 2^(1:4)) * 2^x_to_base2$exponent == x
## [1] TRUE
```

### Odometer:


```r
odometer(c(1,0,1), base=2)
## [1] 0 1 1
```

### Addition of adic integers:


```r
sumadic(c(0,1,1), c(1,0,1), base=2)
## [1] 1 1 0 1
```

### To know more:


http://stla.github.io/stlapblog/posts/expansions_package.html
