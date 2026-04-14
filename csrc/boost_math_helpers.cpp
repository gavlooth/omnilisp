#include <boost/math/special_functions/erf.hpp>
#include <boost/math/special_functions/gamma.hpp>

#include <cmath>
#include <stdexcept>

namespace {

enum OmniBoostMathStatus {
    OMNI_BOOST_MATH_STATUS_OK = 0,
    OMNI_BOOST_MATH_STATUS_INVALID_ARGUMENT = 1,
    OMNI_BOOST_MATH_STATUS_DOMAIN_ERROR = 2,
    OMNI_BOOST_MATH_STATUS_RANGE_ERROR = 3,
    OMNI_BOOST_MATH_STATUS_EVALUATION_ERROR = 4,
};

bool is_gamma_pole(double value) {
    return value <= 0.0 && std::floor(value) == value;
}

template <typename Function>
int run_boost_math_unary(double value, double* out, Function function) {
    if (out == nullptr) return OMNI_BOOST_MATH_STATUS_INVALID_ARGUMENT;
    if (!std::isfinite(value)) return OMNI_BOOST_MATH_STATUS_INVALID_ARGUMENT;

    try {
        double result = function(value);
        if (!std::isfinite(result)) return OMNI_BOOST_MATH_STATUS_RANGE_ERROR;
        *out = result;
        return OMNI_BOOST_MATH_STATUS_OK;
    } catch (const std::domain_error&) {
        return OMNI_BOOST_MATH_STATUS_DOMAIN_ERROR;
    } catch (const std::overflow_error&) {
        return OMNI_BOOST_MATH_STATUS_RANGE_ERROR;
    } catch (const std::underflow_error&) {
        return OMNI_BOOST_MATH_STATUS_RANGE_ERROR;
    } catch (...) {
        return OMNI_BOOST_MATH_STATUS_EVALUATION_ERROR;
    }
}

} // namespace

extern "C" {

int omni_boost_math_lgamma(double value, double* out) {
    if (out == nullptr) return OMNI_BOOST_MATH_STATUS_INVALID_ARGUMENT;
    if (!std::isfinite(value)) return OMNI_BOOST_MATH_STATUS_INVALID_ARGUMENT;
    if (is_gamma_pole(value)) return OMNI_BOOST_MATH_STATUS_DOMAIN_ERROR;

    return run_boost_math_unary(value, out, [](double v) { return boost::math::lgamma(v); });
}

int omni_boost_math_erf(double value, double* out) {
    return run_boost_math_unary(value, out, [](double v) { return boost::math::erf(v); });
}

int omni_boost_math_erfc(double value, double* out) {
    return run_boost_math_unary(value, out, [](double v) { return boost::math::erfc(v); });
}

}
