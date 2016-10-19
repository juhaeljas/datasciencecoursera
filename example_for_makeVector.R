
# I think that one good way to understand this example is trying the follow:
#   
#   First check that when you use the function make_Vector now you have of four different setting

mvec <- makeVector()
x <- 1:4
mvec$set(x)
mvec$get()
# [1] 1 2 3 4
mvec$getmean()
# NULL
mvec$setmean(3.4)
mvec$getmean()
# 3.4

#  It's not the correct mean, I put these number then you can check that you can set whatever number that you want.
# 
# The second part of the assignment is the follow:

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
# These part or code check if you have the mean of the vector of interest. If these exist then you don't need calculate and you can use the cache variable.
# 
# ￼ I put a wrong number for the mean, then you can see that already I set the mean value as follow:
  
cachemean(m)
# 3.4