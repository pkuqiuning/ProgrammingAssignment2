## Cached Matrix Inversion
## Define a special 'matrix' object that can calculate and cache the expensive matrix inversion result

## CacheMatrix is makeCacheMatrix(matrix)
## A special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## CacheMatrix$x is matrix
    ## the matrix to be inversed
    
    ## CacheMatrix$m is one of:
    ## - NULL
    ## - matrix
    ## NULL means not cached, matrix is the cached result of inverse of x
    m = NULL
    
    ## CacheMatrix$set sets the matrix x to be y and reset cache m
    ## matrix -> None
    set = function(y){
        x <<- y
        m <<- NULL
    }
    
    ## CacheMatrix$get gets the matrix x
    ## None -> matrix
    get = function() x
    
    ## CacheMatrix$setm sets the cached inverse matrix
    ## matrix -> None
    setm = function(inverse) m <<- inverse
    
    ## CacheMatrix$getm gets the cached inverse matrix
    ## None -> matrix
    getm = function() m
    
    list(set=set, get=get, setm=setm, getm=getm)
}


## return inverse of special 'matrix' object, using and updating cached result if appliable
## CacheMatrix, ... -> matrix
cacheSolve <- function(x, ...) {
    m = x$getm()
    if (is.null(m)){
        # no cached data
        
        # calculate inverse
        m = solve(x$get())
        
        # cache inverse
        x$setm(m)
    }
    m
}

## test cachematrix module
## None -> None
test_cachematrix = function(){
    
    # handy variables
    y = matrix(c(1,2,3,4), 2, 2)
    yinv = matrix(c(-2, 1, 1.5, -0.5), 2, 2)
    y2 = matrix(c(2,3,4,5), 2, 2)
    y2inv = solve(y2)
    
    x = makeCacheMatrix(y)
    
    # tests
    test_that('makeCacheMatrix correctly initiates object attributes', {
        # x and m should be initiated
        expect_equal(x$get(), y)
        expect_equal(x$getm(), NULL)
    })
    
    test_that('cacheSolve works after first initiate CacheMatrix', {
        # should inverse matrix correctly
        expect_equal(cacheSolve(x), yinv)
        # after solving, cache should be set
        expect_equal(x$getm(), yinv)
        # should produce the correct result for same 'x'
        expect_equal(cacheSolve(x), yinv)
    })
    
    test_that('cacheSolve works after changing matrix', {
        x$set(y2)
        # cache should be emptied
        expect_equal(x$getm(), NULL)
        # should produce different result when 'x' changed
        expect_equal(cacheSolve(x), y2inv)
    })
}

if (getOption('run.test', default=F)){
    # do test if options(run.test=True) is set before run
    library('testthat')
    test_cachematrix()
}