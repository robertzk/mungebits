library(devtools)
load_all()
append(list(1), list(2,3))
append(list(1), list(a=2,a=3))
append(list(1), list(1,a=2,a=3))
append(list(a=1,b=2),list(b=3,c=4))
xyz <- function(x) x + 1
attr(xyz, 'test') <- list(x=1, y=2)
attr(xyz, 'test')
str(xyz)
saveRDS(xyz, '~/tmp/xyz.rds')
str(readRDS('~/tmp/xyz.rds'))
attr(readRDS('~/tmp/xyz.rds'))
attr(readRDS('~/tmp/xyz.rds'),'test')
f1 <- function(df, col) { df[col] <- 2*df[col]; df }
`!`
`!` <- function(x) x
!TRUE
rm(`!`)
eval(substitute(x <- 1))
x
f2 <- function(df, col) { eval(substitute(df[col] <- 2*df[col]), envir = parent.frame()) }
iris2 <- iris
dim(iris2)
iris2
head(iris2)
f1
f2
f2(iris2, 'Sepal.Length')
head(iris2)
f1(iris2, 'Sepal.Length')
iris2 <- f1(iris2, 'Sepal.Length')
head(iris2)
iris2 <- iris
f1
f2
runif(0,2)
runif(1,0,2)
runif(1,0,2)
runif(1,0,2)
runif(1,0,2)
runif(1,0,2)
runif(1,0,2)
runif(1,0,2)
f1 <- function(df, col) { df[col] <- runif(1,0.5,1.5)*df[col]; df }
f2
f2 <- function(df, col) { eval(substitute(df[col] <- runif(1,0.5,1.5)*df[col]), envir = parent.frame()) }
library(microbenchmark)
microbenchmark(iris2 <- f1(iris2), f2(iris2))
head(iris2)
f1
f2
iris2 <- iris
microbenchmark(iris2 <- f1(iris2,'Sepal.Length'), f2(iris2,'Sepal.Length'))
`[<-`
`<-`
`[<-`
ChickWeight
f1
f2
?"[<-"
`[<-.data.frame"
`[<-.data.frame`
str(iris)
attributes(iris)
str(as.list(df))
str(as.list(iris))
F1
f1
f2
f3 <- function(df, col) { class(df) <- 'list'; for(colname in col) df[[colname]] <- runif(1,0.5,1.5)*df[[colname]]; class(df) <- 'data.frame'; df }
f2
substitute({df <- 1; df * 2; })
f4 <- function(df, col) { eval(substitute({ class(df) <- 'list'; for(colname in col) df[[col]] <- runif(1,0.5,1.5)*df[[col]]; class(df) <- 'data.frame'; df }), envir = parent.frame()) }
f4
.do_subassign2_dflt
.Call("SubassignTypeFix", x, y, 5, 1, 2)
.Call("SubassignTypeFix", x, 3, 5, 1, 2)
.Call
f1
f2
f3
f4
iris2 <- iris
head(f3(iris))
head(f3(iris, 'Sepal.Length'))
head(iris2)
head(iris)
head(f3(iris2, 'Sepal.Length'))
f3
f4
head(f4(iris2, 'Sepal.Length'))
head(f4(iris2, 'Sepal.Length'))
head(iris2)
f1
f2
f3
f4
iris2 <- iris
microbenchmark(f1(iris2), f2(iris2), f3(iris2), f4(iris2))
microbenchmark(f1(iris2, 'Sepal.Length'), f2(iris2, 'Sepal.Length'), f3(iris2, 'Sepal.Length'), f4(iris2, 'Sepal.Length'))
microbenchmark(f1(iris2, 'Sepal.Length'), f2(iris2, 'Sepal.Length'), f3(iris2, 'Sepal.Length'), f4(iris2, 'Sepal.Length'))
f3
all.equal
all.equal(f1(iris2), f2(iris2))
iris2 <- iris
f1
f1d <- function(df, col) { df[col] <- 2*df[col]; df }
f2 
f2d <- function(df, col) { eval(substitute(df[col] <- 2*df[col]), envir = parent.frame()) }
f3
f3d <- function(df, col) { class(df) <- 'list'; for(colname in col) df[[colname]] <- 2*df[[colname]]; class(df) <- 'data.frame'; df }
f4
f4d <- function(df, col) { eval(substitute({ class(df) <- 'list'; for(colname in col) df[[col]] <- 2*df[[col]]; class(df) <- 'data.frame'; df }), envir = parent.frame()) }
all.equal(f1d(iris2), f2d(iris2))
all.equal(f1d(iris2, 'Sepal.Length'), f2d(iris2, 'Sepal.Length'))
head(iris2)
iris2 <- iris
all.equal(f1d(iris2, 'Sepal.Length'), f2d(iris2, 'Sepal.Length'))
iris2 <- iris
head(iris2)
head(f1d(iris2))
head(f1d(iris2, 'Sepal.Length'))
head(f2d(iris2, 'Sepal.Length'))
f2d
f2
head(iris2)
head(iris2)
iris2 <- iris
iris3 <- f2d(iris2)
iris3 <- f2d(iris2, 'Sepal.Length')
iris4 <- f1d(iris2, 'Sepal.Length')
all.equal(iris3, iris4)
iris2 <- iris
iris3 <- f1d(iris2, 'Sepal.Length')
iris4 <- iris3
f2d(iris4, 'Sepal.Length')
all.equal(iris3, iris4)
head(iris3)
head(iris4)
iris3 <- f1d(iris2, 'Sepal.Length')
iris4 <- iris2
f2d(iris4, 'Sepal.Length')
all.equal(iris3, iris4)
iris5 <- f3d(iris2, 'Sepal.Length')
iris6 <- iris2
f4d(iris6, 'Sepal.Length'
all.equal(iris5, iris4)
f4d(iris6, 'Sepal.Length')
all.equal(iris5, iris4)
all.equal(iris5, iris6)
history(100)
savehistory('~/dev/mungebits/scrapbook/subassign_benchmark.r')
