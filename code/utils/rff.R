# Required libraries
library(MASS)     # For mvrnorm to generate random numbers
library(stats)    # For kernel functions
library(pracma)   # For cosine function
library(momentchi2)  # For advanced p-value calculation using Lancaster or HBE methods


# Multinomial Kernel function for categorical variables
multinomial_kernel <- function(x, y, theta = 1) {
  if (x == y) {
    return(exp(theta))  # Higher similarity for matching categories
  } else {
    return(1)
  }
}

# Random Fourier Feature approximation for mixed data
rff_mixed <- function(x, num_f = 10) {
  # If x is a single column, reshape it into a matrix
  if (is.vector(x)) {
    x <- matrix(x, ncol = 1)
  }
  
  # Ensure that x is a data frame or matrix
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop("Input x must be a matrix or data frame.")
  }
  
  # Identify categorical and continuous columns
  cat_idx <- which(sapply(1:ncol(x), function(i) {
    length(unique(x[, i])) <= 10
  })) # Categorical columns (<= 10 unique values)
  
  cont_idx <- setdiff(1:ncol(x), cat_idx) # Continuous columns
  
  # Check if any continuous or categorical variables exist
  if (length(cat_idx) > 0 && length(cat_idx) <= ncol(x)) {
    x_disc <- x[, cat_idx, drop = FALSE]
  } else {
    x_disc <- NULL
  }
  
  if (length(cont_idx) > 0 && length(cont_idx) <= ncol(x)) {
    x_cont <- x[, cont_idx, drop = FALSE]
  } else {
    x_cont <- NULL
  }
  
  # Calculate number of RFFs for categorical and continuous
  n_disc <- if (!is.null(x_disc)) ncol(x_disc) else 0
  n_cont <- if (!is.null(x_cont)) ncol(x_cont) else 0
  
  if (n_cont > 0) {
    num_f_cont <- floor(n_cont / (n_disc + n_cont) * num_f)
  } else {
    num_f_cont <- 0  # If no continuous features, set to 0
  }
  
  num_f_disc <- num_f - num_f_cont
  
  # Random Fourier Features for continuous variables (Gaussian RBF kernel)
  if (!is.null(x_cont)) {
    r <- nrow(x_cont)  # number of rows (samples)
    sigma <- median(dist(x_cont))  # Gaussian width
    if (sigma == 0 || is.na(sigma)) sigma <- 1
    
    w <- matrix(rnorm(num_f_cont * n_cont, mean = 0, sd = 1 / sigma), nrow = num_f_cont, ncol = n_cont)
    b <- matrix(runif(num_f_cont * r, 0, 2 * pi), nrow = num_f_cont, ncol = r)
    
    cont_feat <- sqrt(2) * cos(w %*% t(x_cont) + b)
    cont_feat <- t(cont_feat)  # Transpose back to (r, num_f_cont)
    
  } else {
    cont_feat <- NULL
  }
  
  # Multinomial Kernel Features for categorical variables
  if (!is.null(x_disc)) {
    r <- nrow(x_disc)
    disc_feat <- matrix(0, nrow = r, ncol = num_f_disc)
    for (i in 1:r) {
      for (j in 1:num_f_disc) {
        for (k in 1:ncol(x_disc)) {
          disc_feat[i, j] <- disc_feat[i, j] + multinomial_kernel(x_disc[i, k], x_disc[sample(1:r, 1), k], theta = 1)
        }
      }
    }
  } else {
    disc_feat <- NULL
  }
  
  # Combine continuous and categorical features
  if (!is.null(cont_feat) & !is.null(disc_feat)) {
    return(cbind(cont_feat, disc_feat))
  } else if (!is.null(cont_feat)) {
    return(cont_feat)
  } else {
    return(disc_feat)
  }
}

# Kernel Ridge Regression using Cholesky and matrix inversion
kernel_ridge_regression <- function(x_rff, z_rff, lambda = 1e-10) {
  Lzz <- chol(t(z_rff) %*% z_rff + lambda * diag(ncol(z_rff)))  # Cholesky decomposition
  A <- backsolve(Lzz, t(z_rff) %*% x_rff, transpose = TRUE)
  fitted_values <- z_rff %*% backsolve(Lzz, A)
  return(fitted_values)
}

# CI Test with Random Fourier Features and momentchi2
ci_test_rff <- function(x, y, z = NULL, suffStat, num_f = 10, alpha = 0.05) {
  # Extract data from suffStat
  data <- suffStat$data
  
  # # Print debugging information
  # cat("Testing variables x =", x, " y =", y, " z =", z, "\n")
  # cat("Number of features for RFF:", num_f, "\n")
  # 
  # Generate random Fourier features for x and y
  x_rff <- rff_mixed(data[, x], num_f = num_f)
  y_rff <- rff_mixed(data[, y], num_f = num_f)
  
  # If conditional on z, generate features for z and perform Kernel Ridge Regression
  if (!is.null(z) && length(z) > 0L) {
    z_rff <- rff_mixed(data[, z], num_f = num_f)
    # Perform Kernel Ridge Regression to get residuals
    res_x_z <- as.matrix(x_rff) - kernel_ridge_regression(as.matrix(x_rff), as.matrix(z_rff))
    res_y_z <- as.matrix(y_rff) - kernel_ridge_regression(as.matrix(y_rff), as.matrix(z_rff))
  } else {
    res_x_z <- x_rff
    res_y_z <- y_rff
  }
  
  # Compute covariance matrix for residuals
  cov_xy <- cov(res_x_z, res_y_z)
  
  # Eigenvalue decomposition for the covariance matrix
  eigen_cov <- eigen(cov_xy)
  
  # Extract real parts of the eigenvalues
  real_eigenvalues <- Re(eigen_cov$values)
  real_eigenvalues <- real_eigenvalues[real_eigenvalues > 0] # Filter positive real eigenvalues
  
  # Compute test statistic
  test_stat <- sum(real_eigenvalues^2)
  
  # Compute p-value using momentchi2 package
  if (length(real_eigenvalues) > 0) {
    pval <- momentchi2::hbe(real_eigenvalues, test_stat)
  } else {
    pval <- NA
  }
  
  # Return p-value and test statistic
  return(pval)
}

