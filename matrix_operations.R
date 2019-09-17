
a <- c(1,2,3,4)
#a <- matrix(1:8,4,2)
I <- diag(1,4)
W <- matrix(1,4,4)
t(a)
W
a
t(a) %*% W %*% a
t(a) %*% I %*% a
t(a) %*% a
1+2*2+3*3+4*4
