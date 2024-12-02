is_conv <- function(x) performance::check_convergence(x) & !performance::check_singularity(x)
