r_vm <- function(x, n1, n2, p11, p22, rho1 = 1, rho2 = 0) {
  e1 <- p11 * n1 * (n1 - 1) / 2
  e2 <- p22 * n2 * (n2 - 1) / 2
  num <- (2 * e1 + x) * rho1 + (2 * e2 + x) * rho2
  den <- 2 * (e1 + e2 + x)
  num / den
}
r_ip <- function(x, n1, n2, p11, p22, rho1 = 1, rho2 = 0) {
  e1 <- p11 * n1 * (n1 - 1) / 2
  e2 <- p22 * n2 * (n2 - 1) / 2
  num <- n1^2 * (2 * e2 + x) * rho1 + n2^2 * (2 * e1 + x) * rho2
  den <- n1^2 * (2 * e2 + x) + n2^2 * (2 * e1 + x)
  num / den
}
r_ld <- function(x, n1, n2, p11, p22, rho1 = 1, rho2 = 0) {
  (n1 * rho1 + n2 * rho2) / (n1 + n2)
}
a_vm <- function(x, n1, n2, p11, p22) {
  e1 <- p11 * n1 * (n1 - 1) / 2
  e2 <- p22 * n2 * (n2 - 1) / 2
  num <- 4 * n1 * n2 * (e1 + e2 + x)^2
  den <- n1 * (2 * e2 + x)^2 + n2 * (2 * e1 + x)^2
  num / den
}
a_ip <- function(x, n1, n2, p11, p22) {
  e1 <- p11 * n1 * (n1 - 1) / 2
  e2 <- p22 * n2 * (n2 - 1) / 2
  a12 <- n1^2 * (2*e2 + x)
  a21 <- n2^2 * (2*e1 + x)
  b12 <- n1^2 * (2 * e2 + x)^2 *
    (n1^3 * p11 * (2 * e2 + x) + n2 * x * (2 * e1 + x))
  b21 <- n2^2 * (2 * e1 + x)^2 *
    (n2^3 * p22 * (2 * e1 + x) + n1 * x * (2 * e2 + x))
  num <- (2 * e1 + x) * (2 * e2 + x) * (a12 + a21)^2
  den <- b12 + b21
  num / den
}
a_ld <- function(x, n1, n2, p11, p22) {
  n1 + n2
}

t_dense_vm <- function(x, n1, n2, p11, p22) {
  r <- r_vm(x, n1, n2, p11, p22)
  a_vm(x, n1, n2, p11, p22) * (r * log(r) + (1-r) * log(1-r))
}
t_dense_ip <- function(x, n1, n2, p11, p22) {
  r <- r_ip(x, n1, n2, p11, p22)
  a_ip(x, n1, n2, p11, p22) * (r * log(r) + (1-r) * log(1-r))
}
t_dense_ld <- function(x, n1, n2, p11, p22) {
  r <- r_ld(x, n1, n2, p11, p22)
  a_ld(x, n1, n2, p11, p22) * (r * log(r) + (1-r) * log(1-r))
}

# Set parameters.
n1 = 400
n2 = 100
p11 = 0.1
p22 = 0.1
x_vec <- 10^seq(0, log10(n1*n2), 0.01)

# Plot R.
r_vm_vec <- sapply(x_vec, r_vm, n1 = n1, n2 = n2, p11 = p11, p22 = p22)
plot(x_vec, r_vm_vec, log = "x", ylim = c(0.3, 1.0), type = "l")
r_ip_vec <- sapply(x_vec, r_ip, n1 = n1, n2 = n2, p11 = p11, p22 = p22)
lines(x_vec, r_ip_vec, col = "red")
r_ld_vec <- sapply(x_vec, r_ld, n1 = n1, n2 = n2, p11 = p11, p22 = p22)
lines(x_vec, r_ld_vec, col = "blue")

# Plot T_dense.
t_dense_vm_vec <-
  sapply(x_vec, t_dense_vm, n1 = n1, n2 = n2, p11 = p11, p22 = p22)
plot(x_vec, t_dense_vm_vec, log = "x",  type = "l")
t_dense_ip_vec <-
  sapply(x_vec, t_dense_ip, n1 = n1, n2 = n2, p11 = p11, p22 = p22)
lines(x_vec, t_dense_ip_vec, col = "red")
t_dense_ld_vec <-
  sapply(x_vec, t_dense_ld, n1 = n1, n2 = n2, p11 = p11, p22 = p22)
lines(x_vec, t_dense_ld_vec, col = "blue")
