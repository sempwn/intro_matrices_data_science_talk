library(plot.matrix)

x <- 1:4
(z <- x %*% x)    # scalar ("inner") product (1 x 1 matrix)
drop(z)             # as scalar

y <- diag(x)
z <- matrix(1:12, ncol = 3, nrow = 4)
y %*% z
y %*% x
x %*% z

# query is dog cat into
# embedding pet, outdoors, food
QX <- matrix(c(
  2.0, 1.0, -5,
  2.0, -2, -6
), ncol = 3, nrow = 2
)

# keys are food blog, pet tips
# # embedding pet, outdoors, food
KX <- matrix(c(
  -2, 0.5, 8.0,
  5.0, 2.0, -3
), ncol = 3, nrow = 2
)

# values are cute cats video, travel blog, cooking with dog
# # keys food blog, pet tips
VX <- matrix(c(
  0, 0.2,
  0.4, 0,
  1.0, 0.2
), ncol = 2, nrow = 3
)

QX %*% t(KX)

QX %*% t(KX) / sqrt(2)


softmaxX <- exp(QX %*% t(KX) / sqrt(2))/rowSums(exp(QX %*% t(KX) / sqrt(2)))
softmaxX

# keys for dogs
t(softmaxX) %*% matrix(c(1,0),ncol = 1)
# keys for cats
t(softmaxX) %*% c(0, 1)

# keys for cats and dogs
t(softmaxX) %*% c(1, 1)

softmaxX %*% t(VX)

# Note that an embedding is multiplied from the left hand side
matrix(c(1, 0), nrow = 1) %*% softmaxX %*% t(VX)

plot(VX, border = NA,key = NULL)
