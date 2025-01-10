A = matrix(NA, 3, 3)
A[,1] = c(0,1,4)/5
A[,2] = c(1,0,3)/4
A[,3] = c(2,1,0)/3
A

p0 = c(1,0,0)
p1 = A %*% p0
p1

p2 = A %*% p1
p2

p3 = A %*% p2
p3

pacman::p_load(expm)
A_inf_approx = A %^% 100
p_approx = A_inf_approx %*% p0
p_approx
#very good approximation ... the real p is...
eigen(A)
p = matrix(eigen(A)$vectors[,1] / sum(eigen(A)$vectors[,1]), nrow = 3)
p
#we normalize it to 1 to get a probability distribution 
#(eigenvectors) are arbitrary up to a scale
p
#is this really true???
A %*% p
A %*% A %*% A %*% A %*% p

#maybe we can try a new starting position
p0 = c(0,0,1)
p_approx = A %^% 100 %*% p0
p_approx
