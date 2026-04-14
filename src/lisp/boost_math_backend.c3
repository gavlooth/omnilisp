module lisp;

const int BOOST_MATH_STATUS_OK = 0;
const int BOOST_MATH_STATUS_INVALID_ARGUMENT = 1;
const int BOOST_MATH_STATUS_DOMAIN_ERROR = 2;
const int BOOST_MATH_STATUS_RANGE_ERROR = 3;
const int BOOST_MATH_STATUS_EVALUATION_ERROR = 4;

extern fn int omni_boost_math_lgamma(double value, double* out) @extern("omni_boost_math_lgamma");
extern fn int omni_boost_math_erf(double value, double* out) @extern("omni_boost_math_erf");
extern fn int omni_boost_math_erfc(double value, double* out) @extern("omni_boost_math_erfc");
extern fn int omni_boost_math_standard_normal_cdf(double value, double* out) @extern("omni_boost_math_standard_normal_cdf");
extern fn int omni_boost_math_standard_normal_quantile(double probability, double* out) @extern("omni_boost_math_standard_normal_quantile");
