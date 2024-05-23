library(plot.matrix)
library(tidyverse)

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

xlabs <- c("pet", "outdoors", "food")
ylabs <- c("dog", "cat")


dat2 <-
  t(QX) %>%
  as_tibble() %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value") %>%
  mutate(
    Var2 = gsub("V", "", Var2)
  ) %>%
  left_join(tibble(Var1 = as.character(1:length(xlabs)), xlab = xlabs)) %>%
  left_join(tibble(Var2 = as.character(1:length(ylabs)), ylab = rev(ylabs))) %>%
  mutate(
    Var1 = factor(xlab, levels = xlabs),
    Var2 = factor(ylab, levels = rev(ylabs))
  )


ggplot(dat2, aes(Var1, Var2)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 1))) +
  scale_fill_gradient2() +
  ggplot2::theme_minimal() +
  labs(x="",y="") +
  theme(legend.position = "none")
