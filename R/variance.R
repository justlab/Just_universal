#' @export
varn = function(v, na.rm = F)
# Variance with n instead of (n - 1) in the denominator.
    mean((v - mean(v, na.rm = na.rm))^2, na.rm = na.rm)

#' @export
sdn = function(v, na.rm = F)
# Standard deviation with n instead of (n - 1) in the denominator.
    sqrt(varn(v, na.rm))
