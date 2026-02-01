\name{FeatureSRHT}
\alias{FeatureSRHT}
\title{Subsampled Randomized Hadamard Transform for OLS Feature Selection}
\description{
  Performs efficient feature selection for Ordinary Least Squares (OLS) regression using the Subsampled Randomized Hadamard Transform (SRHT). 

  This function implements a highly optimized, parallelized Fast Walsh-Hadamard Transform (FWHT) to rotate the input data in \eqn{O(ND \log D)} time. [cite_start]It supports multiple sampling strategies, including a novel "Supervised" method adapted from classification literature[cite: 147, 163], which uses robust Quantile Binning and a Laplacian-based scoring metric (\eqn{S_b - \alpha S_w}) to select features that maximize separation between target values.
}
\usage{
FeatureSRHT(X, y, r, method = "uniform", bins = 10, alpha = 0.0)
}
\arguments{
  \item{X}{
    A numeric matrix of dimensions \eqn{N \times D}. The feature matrix.
  }
  \item{y}{
    A numeric vector of length \eqn{N}. The continuous target variable.
  }
  \item{r}{
    Integer. The target reduced dimension (number of features to select). Must be less than \eqn{D}.
  }
  \item{method}{
    Character string specifying the sampling strategy. Options are:
    \itemize{
      [cite_start]\item \code{"uniform"} (Default): Uniformly samples \eqn{r} features from the rotated matrix[cite: 9].
      [cite_start]\item \code{"top-r"}: Selects the \eqn{r} rotated features with the largest Euclidean norms (deterministic)[cite: 150].
      [cite_start]\item \code{"leverage"}: Selects features based on importance sampling (probability proportional to column norms), re-weighted by \eqn{\sqrt{1/(r p_i)}} to maintain an unbiased estimator[cite: 138].
      [cite_start]\item \code{"supervised"}: Uses Quantile Binning on \eqn{y} to create pseudo-classes, then selects features maximizing the "Variance Between Bins" penalized by "Variance Within Bins"[cite: 167, 171].
      \item \code{"all"}: Runs all methods and returns a comparative list.
    }
  }
  \item{bins}{
    Integer. The number of quantile bins (Equal-Depth) used to discretize the continuous target \eqn{y} for the "supervised" method. Defaults to 10. Robust to outliers.
  }
  \item{alpha}{
    Numeric. The penalty weight for intra-class variance in the supervised scoring metric. 
    [cite_start]Formula: \eqn{\text{Score} = \text{VarianceBetween} - \alpha \times \text{VarianceWithin}}[cite: 171].
    Defaults to 0.0 (maximizing separability only). Setting \eqn{\alpha > 0} (e.g., 1.0) enforces tighter clustering of similar target values.
  }
}
\details{
  The algorithm performs the following steps:
  \enumerate{
    [cite_start]\item \strong{Rotation:} The input matrix \eqn{X} is zero-padded to the next power of 2 and rotated using a parallelized Fast Walsh-Hadamard Transform (FWHT)[cite: 9, 81]. This mixes information across all features.
    \item \strong{Scoring:} Features are scored based on the selected \code{method}. 
      \itemize{
        \item For \code{"supervised"}, the target \eqn{y} is discretized into \code{bins} groups using quantile binning. [cite_start]A score is calculated for every rotated feature \eqn{j}: \eqn{b_j = \text{Var}_{\text{between}}(j) - \alpha \cdot \text{Var}_{\text{within}}(j)}[cite: 147].
      }
    \item \strong{Selection:} The top \eqn{r} features are selected (or sampled) to form the reduced matrix \eqn{\tilde{X}}.
    \item \strong{Regression:} An OLS regression is performed on \eqn{\tilde{X}} to obtain coefficients.
  }
  The rotation step uses OpenMP to parallelize across rows, ensuring minimal memory overhead via zero-allocation buffers.
}
\value{
  A list containing the results for the selected method(s). Each element is a list with components:
  \item{Coefficients}{A vector of length \eqn{r} containing the OLS coefficients for the selected features.}
  \item{Indices}{Integer vector of the indices (0-based) of the selected features from the rotated matrix.}
  \item{Time_Rot}{Time (in seconds) taken for the FWHT rotation.}
  \item{Time_Sample}{Time (in seconds) taken for scoring and sampling.}
  \item{Time_OLS}{Time (in seconds) taken to solve the linear system.}
}
\author{
  Anirudh
}
\references{
  Lei, Z., & Lan, L. (2020). Improved Subsampled Randomized Hadamard Transform for Linear SVM. \emph{AAAI Conference on Artificial Intelligence}. (Adapted for OLS Regression)[cite_start]. [cite: 1]
}
\examples{
\dontrun{
# This example demonstrates how to use the function on synthetic data.
# 1. Simulate data (N=1000 rows, D=512 columns)
set.seed(42)
N <- 1000
D <- 512
X <- matrix(rnorm(N * D), N, D)

# Create a target y with some true signal
true_beta <- rep(0, D)
true_beta[1:10] <- 5.0
y <- as.vector(X \%*\% true_beta + rnorm(N))

# 2. Run SRHT Feature Selection
# Uses Supervised method with alpha=1.0 (Balanced Penalty)
res <- FeatureSRHT(X, y, r=50, method="supervised", bins=10, alpha=1.0)

# 3. Inspect Selected Features
print(res$Supervised$Indices)
}
}
\keyword{regression}
\keyword{feature selection}
\keyword{high-dimensional}
