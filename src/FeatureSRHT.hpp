#ifndef FEATURESRHT_HPP
#define FEATURESRHT_HPP

#include <RcppEigen.h>
#include <vector>
#include <random>

using namespace Eigen;

struct BenchmarkResult {
    std::string method;
    double train_r2;
    double time_sample;
    double time_ols;
    Eigen::VectorXd coeffs;
    std::vector<int> indices;
    int total_features;
    bool executed;
};

struct OLSResult { double r2; Eigen::VectorXd coeffs; };

// Declarations
double compute_scale(const MatrixXd& X);
MatrixXd apply_rotation(const MatrixXd& X, double scale, int seed);
OLSResult solve_ols(const MatrixXd& X, const VectorXd& y);
std::vector<int> bin_continuous_targets_quantile(const VectorXd& y, int bins);

class FeatureSRHT_Core {
public:
    static std::pair<MatrixXd, std::vector<int>> fit_transform_uniform(const MatrixXd& X_rot, int r, std::mt19937& rng);
    static std::pair<MatrixXd, std::vector<int>> fit_transform_top_r(const MatrixXd& X_rot, int r);
    static std::pair<MatrixXd, std::vector<int>> fit_transform_leverage(const MatrixXd& X_rot, int r, std::mt19937& rng);
    // Added alpha parameter for paper compliance
    static std::pair<MatrixXd, std::vector<int>> fit_transform_supervised(const MatrixXd& X_rot, const std::vector<int>& labels, int r, double alpha);
};
#endif
