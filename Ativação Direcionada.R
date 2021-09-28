A = matrix(c(  1,  1,  .5), 1)
B = matrix(c( .5,  1, .25), 1)
E = matrix(c(.25,  0,   1), 1)

W = matrix(c(A, B, E), 3)

M = matrix(rep(1, 9), 3)

colnames(A) = 
  colnames(B) = 
  colnames(E) = 
  colnames(M) = 
  rownames(M) = 
  colnames(W) =
  rownames(W) = c("A", "B", "E")

activation = c(0, 0, 1)

M = (activation %*% W)/max(activation %*% W)

M = (M %*% W)/max(M %*% W)
M