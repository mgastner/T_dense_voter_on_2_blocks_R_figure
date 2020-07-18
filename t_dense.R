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
n1 = 600
n2 = 400
p11 = 0.3
p22 = 0.1
x_vec <- 10^seq(0, log10(n1*n2), 0.01)
r_vm_vec <- sapply(x_vec, r_vm, n1 = n1, n2 = n2, p11 = p11, p22 = p22)
plot(x_vec, r_vm_vec, log = "x", ylim = c(0.3, 1.0), type = "l")
r_ip_vec <- sapply(x_vec, r_ip, n1 = n1, n2 = n2, p11 = p11, p22 = p22)
lines(x_vec, r_ip_vec, log = "x", col = "red")
r_ld_vec <- sapply(x_vec, r_ld, n1 = n1, n2 = n2, p11 = p11, p22 = p22)
lines(x_vec, r_ld_vec, log = "x", col = "blue")