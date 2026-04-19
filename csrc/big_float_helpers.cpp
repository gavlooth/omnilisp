#include <boost/multiprecision/cpp_dec_float.hpp>
#include <boost/math/distributions/normal.hpp>
#include <boost/math/special_functions/erf.hpp>
#include <boost/math/special_functions/fpclassify.hpp>
#include <boost/math/special_functions/gamma.hpp>

#include <cmath>
#include <cstdlib>
#include <cstring>
#include <ios>
#include <limits>
#include <string>

using boost::multiprecision::cpp_dec_float_50;
constexpr long BIG_FLOAT_MAX_INTEGER_DIGITS = 315653;

namespace {

cpp_dec_float_50* as_big_float(void* value) {
    return static_cast<cpp_dec_float_50*>(value);
}

const cpp_dec_float_50* as_big_float_const(const void* value) {
    return static_cast<const cpp_dec_float_50*>(value);
}

bool parse_big_float(const char* chars, size_t len, cpp_dec_float_50& out) {
    if (chars == nullptr || len == 0) return false;
    std::string text(chars, len);
    cpp_dec_float_50 value(text);
    if (!boost::math::isfinite(value)) return false;
    out = value;
    return true;
}

cpp_dec_float_50 apply_binary(const cpp_dec_float_50& lhs, const cpp_dec_float_50& rhs, int op, bool& ok) {
    ok = true;
    switch (op) {
        case 1: return lhs + rhs;
        case 2: return lhs - rhs;
        case 3: return lhs * rhs;
        case 4:
            if (rhs == 0) {
                ok = false;
                return 0;
            }
            return lhs / rhs;
        default:
            ok = false;
            return 0;
    }
}

cpp_dec_float_50 apply_unary_math(const cpp_dec_float_50& value, int op, bool& ok) {
    ok = true;
    switch (op) {
        case 1: return sin(value);
        case 2: return cos(value);
        case 3: return tan(value);
        case 4: return asin(value);
        case 5: return acos(value);
        case 6: return atan(value);
        case 7: return exp(value);
        case 8: return log(value);
        case 9: return log10(value);
        case 10: return sqrt(value);
        case 11: return floor(value);
        case 12: return ceil(value);
        case 13: return boost::math::lgamma(value);
        case 14: return boost::math::erf(value);
        case 15: return boost::math::erfc(value);
        case 16: {
            boost::math::normal_distribution<cpp_dec_float_50> normal;
            return boost::math::cdf(normal, value);
        }
        case 17: {
            if (value <= 0 || value >= 1) {
                ok = false;
                return 0;
            }
            boost::math::normal_distribution<cpp_dec_float_50> normal;
            return boost::math::quantile(normal, value);
        }
        case 18: return sinh(value);
        case 19: return cosh(value);
        case 20: return tanh(value);
        default:
            ok = false;
            return 0;
    }
}

cpp_dec_float_50 apply_binary_math(const cpp_dec_float_50& lhs, const cpp_dec_float_50& rhs, int op, bool& ok) {
    ok = true;
    switch (op) {
        case 1: return pow(lhs, rhs);
        case 2: return atan2(lhs, rhs);
        default:
            ok = false;
            return 0;
    }
}

cpp_dec_float_50 apply_integer_rounding(const cpp_dec_float_50& value, int op, bool& ok) {
    ok = true;
    switch (op) {
        case 1: return floor(value);
        case 2: return ceil(value);
        case 3:
            if (value < 0) return ceil(value - cpp_dec_float_50("0.5"));
            return floor(value + cpp_dec_float_50("0.5"));
        case 4:
            if (value < 0) return ceil(value);
            return floor(value);
        default:
            ok = false;
            return 0;
    }
}

char* copy_string(const std::string& text, size_t* out_len) {
    char* out = static_cast<char*>(std::malloc(text.size() + 1));
    if (out == nullptr) return nullptr;
    std::memcpy(out, text.data(), text.size());
    out[text.size()] = '\0';
    if (out_len != nullptr) *out_len = text.size();
    return out;
}

} // namespace

extern "C" {

void* omni_big_float_from_i64(long value) {
    try {
        return new cpp_dec_float_50(value);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_float_from_double(double value) {
    try {
        if (!std::isfinite(value)) return nullptr;
        return new cpp_dec_float_50(value);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_float_from_chars(const char* chars, size_t len) {
    try {
        cpp_dec_float_50 value;
        if (!parse_big_float(chars, len, value)) return nullptr;
        return new cpp_dec_float_50(value);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_float_clone(const void* value) {
    try {
        const cpp_dec_float_50* src = as_big_float_const(value);
        if (src == nullptr) return nullptr;
        return new cpp_dec_float_50(*src);
    } catch (...) {
        return nullptr;
    }
}

void omni_big_float_free(void* value) {
    delete as_big_float(value);
}

char* omni_big_float_to_string(const void* value, size_t* out_len) {
    try {
        const cpp_dec_float_50* src = as_big_float_const(value);
        if (src == nullptr || !boost::math::isfinite(*src)) return nullptr;
        return copy_string(src->str(0, std::ios_base::fmtflags(0)), out_len);
    } catch (...) {
        return nullptr;
    }
}

void omni_big_float_string_free(char* value) {
    std::free(value);
}

int omni_big_float_to_double(const void* value, double* out) {
    try {
        const cpp_dec_float_50* src = as_big_float_const(value);
        if (src == nullptr || out == nullptr || !boost::math::isfinite(*src)) return 0;
        double converted = src->convert_to<double>();
        if (!std::isfinite(converted)) return 0;
        *out = converted;
        return 1;
    } catch (...) {
        return 0;
    }
}

int omni_big_float_compare(const void* lhs, const void* rhs) {
    const cpp_dec_float_50* left = as_big_float_const(lhs);
    const cpp_dec_float_50* right = as_big_float_const(rhs);
    if (left == nullptr || right == nullptr) return 0;
    if (*left < *right) return -1;
    if (*left > *right) return 1;
    return 0;
}

int omni_big_float_compare_i64(const void* lhs, long rhs) {
    const cpp_dec_float_50* left = as_big_float_const(lhs);
    if (left == nullptr) return 0;
    cpp_dec_float_50 right(rhs);
    if (*left < right) return -1;
    if (*left > right) return 1;
    return 0;
}

int omni_big_float_compare_double(const void* lhs, double rhs) {
    const cpp_dec_float_50* left = as_big_float_const(lhs);
    if (left == nullptr || !std::isfinite(rhs)) return 0;
    cpp_dec_float_50 right(rhs);
    if (*left < right) return -1;
    if (*left > right) return 1;
    return 0;
}

void* omni_big_float_binary(const void* lhs, const void* rhs, int op) {
    try {
        const cpp_dec_float_50* left = as_big_float_const(lhs);
        const cpp_dec_float_50* right = as_big_float_const(rhs);
        if (left == nullptr || right == nullptr) return nullptr;
        bool ok = false;
        cpp_dec_float_50 result = apply_binary(*left, *right, op, ok);
        if (!ok || !boost::math::isfinite(result)) return nullptr;
        return new cpp_dec_float_50(result);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_float_neg(const void* value) {
    try {
        const cpp_dec_float_50* src = as_big_float_const(value);
        if (src == nullptr) return nullptr;
        return new cpp_dec_float_50(-*src);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_float_abs(const void* value) {
    try {
        const cpp_dec_float_50* src = as_big_float_const(value);
        if (src == nullptr) return nullptr;
        return new cpp_dec_float_50(*src < 0 ? -*src : *src);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_float_unary_math(const void* value, int op) {
    try {
        const cpp_dec_float_50* src = as_big_float_const(value);
        if (src == nullptr || !boost::math::isfinite(*src)) return nullptr;
        bool ok = false;
        cpp_dec_float_50 result = apply_unary_math(*src, op, ok);
        if (!ok || !boost::math::isfinite(result)) return nullptr;
        return new cpp_dec_float_50(result);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_float_binary_math(const void* lhs, const void* rhs, int op) {
    try {
        const cpp_dec_float_50* left = as_big_float_const(lhs);
        const cpp_dec_float_50* right = as_big_float_const(rhs);
        if (left == nullptr || right == nullptr || !boost::math::isfinite(*left) || !boost::math::isfinite(*right)) {
            return nullptr;
        }
        bool ok = false;
        cpp_dec_float_50 result = apply_binary_math(*left, *right, op, ok);
        if (!ok || !boost::math::isfinite(result)) return nullptr;
        return new cpp_dec_float_50(result);
    } catch (...) {
        return nullptr;
    }
}

char* omni_big_float_round_to_integer_string(const void* value, int op, size_t* out_len) {
    try {
        const cpp_dec_float_50* src = as_big_float_const(value);
        if (src == nullptr || !boost::math::isfinite(*src)) return nullptr;

        if (*src != 0) {
            cpp_dec_float_50 magnitude = *src < 0 ? -*src : *src;
            cpp_dec_float_50 digits = floor(log10(magnitude)) + 1;
            if (digits > BIG_FLOAT_MAX_INTEGER_DIGITS) return nullptr;
        }

        bool ok = false;
        cpp_dec_float_50 rounded = apply_integer_rounding(*src, op, ok);
        if (!ok || !boost::math::isfinite(rounded)) return nullptr;

        return copy_string(rounded.str(0, std::ios_base::fixed), out_len);
    } catch (...) {
        return nullptr;
    }
}

} // extern "C"
