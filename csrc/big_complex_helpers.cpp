#include <boost/multiprecision/cpp_dec_float.hpp>
#include <boost/math/special_functions/fpclassify.hpp>

#include <complex>
#include <cstdlib>
#include <cstring>
#include <ios>
#include <string>

using boost::multiprecision::cpp_dec_float_50;
using omni_big_complex = std::complex<cpp_dec_float_50>;

namespace {

omni_big_complex* as_big_complex(void* value) {
    return static_cast<omni_big_complex*>(value);
}

const omni_big_complex* as_big_complex_const(const void* value) {
    return static_cast<const omni_big_complex*>(value);
}

const cpp_dec_float_50* as_big_float_const(const void* value) {
    return static_cast<const cpp_dec_float_50*>(value);
}

bool finite_complex(const omni_big_complex& value) {
    return boost::math::isfinite(value.real()) && boost::math::isfinite(value.imag());
}

bool parse_big_float_part(const char* chars, size_t len, cpp_dec_float_50& out) {
    if (chars == nullptr || len == 0) return false;
    std::string text(chars, len);
    cpp_dec_float_50 value(text);
    if (!boost::math::isfinite(value)) return false;
    out = value;
    return true;
}

char* copy_string(const std::string& text, size_t* out_len) {
    char* out = static_cast<char*>(std::malloc(text.size() + 1));
    if (out == nullptr) return nullptr;
    std::memcpy(out, text.data(), text.size());
    out[text.size()] = '\0';
    if (out_len != nullptr) *out_len = text.size();
    return out;
}

omni_big_complex apply_binary(const omni_big_complex& lhs, const omni_big_complex& rhs, int op, bool& ok) {
    ok = true;
    switch (op) {
        case 1: return lhs + rhs;
        case 2: return lhs - rhs;
        case 3: return lhs * rhs;
        case 4:
            if (rhs.real() == 0 && rhs.imag() == 0) {
                ok = false;
                return omni_big_complex(0, 0);
            }
            return lhs / rhs;
        default:
            ok = false;
            return omni_big_complex(0, 0);
    }
}

} // namespace

extern "C" {

void* omni_big_complex_from_i64(long real) {
    try {
        return new omni_big_complex(cpp_dec_float_50(real), cpp_dec_float_50(0));
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_complex_from_double(double real) {
    try {
        cpp_dec_float_50 part(real);
        if (!boost::math::isfinite(part)) return nullptr;
        return new omni_big_complex(part, cpp_dec_float_50(0));
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_complex_from_big_float_parts(const void* real, const void* imag) {
    try {
        const cpp_dec_float_50* real_part = as_big_float_const(real);
        const cpp_dec_float_50* imag_part = imag == nullptr ? nullptr : as_big_float_const(imag);
        if (real_part == nullptr || !boost::math::isfinite(*real_part)) return nullptr;
        cpp_dec_float_50 im = imag_part == nullptr ? cpp_dec_float_50(0) : *imag_part;
        if (!boost::math::isfinite(im)) return nullptr;
        return new omni_big_complex(*real_part, im);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_complex_from_chars(const char* real_chars, size_t real_len, const char* imag_chars, size_t imag_len) {
    try {
        cpp_dec_float_50 real;
        cpp_dec_float_50 imag;
        if (!parse_big_float_part(real_chars, real_len, real)) return nullptr;
        if (!parse_big_float_part(imag_chars, imag_len, imag)) return nullptr;
        return new omni_big_complex(real, imag);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_complex_clone(const void* value) {
    try {
        const omni_big_complex* src = as_big_complex_const(value);
        if (src == nullptr || !finite_complex(*src)) return nullptr;
        return new omni_big_complex(*src);
    } catch (...) {
        return nullptr;
    }
}

void omni_big_complex_free(void* value) {
    delete as_big_complex(value);
}

char* omni_big_complex_to_string(const void* value, size_t* out_len) {
    try {
        const omni_big_complex* src = as_big_complex_const(value);
        if (src == nullptr || !finite_complex(*src)) return nullptr;

        cpp_dec_float_50 imag = src->imag();
        bool negative_imag = imag < 0;
        if (negative_imag) imag = -imag;

        std::string text = src->real().str(0, std::ios_base::fmtflags(0));
        text += negative_imag ? "-" : "+";
        text += imag.str(0, std::ios_base::fmtflags(0));
        text += "i";
        return copy_string(text, out_len);
    } catch (...) {
        return nullptr;
    }
}

void omni_big_complex_string_free(char* value) {
    std::free(value);
}

void* omni_big_complex_binary(const void* lhs, const void* rhs, int op) {
    try {
        const omni_big_complex* left = as_big_complex_const(lhs);
        const omni_big_complex* right = as_big_complex_const(rhs);
        if (left == nullptr || right == nullptr || !finite_complex(*left) || !finite_complex(*right)) return nullptr;
        bool ok = false;
        omni_big_complex result = apply_binary(*left, *right, op, ok);
        if (!ok || !finite_complex(result)) return nullptr;
        return new omni_big_complex(result);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_complex_neg(const void* value) {
    try {
        const omni_big_complex* src = as_big_complex_const(value);
        if (src == nullptr || !finite_complex(*src)) return nullptr;
        return new omni_big_complex(-(*src));
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_complex_abs(const void* value) {
    try {
        const omni_big_complex* src = as_big_complex_const(value);
        if (src == nullptr || !finite_complex(*src)) return nullptr;
        cpp_dec_float_50 result = std::abs(*src);
        if (!boost::math::isfinite(result)) return nullptr;
        return new cpp_dec_float_50(result);
    } catch (...) {
        return nullptr;
    }
}

int omni_big_complex_equal(const void* lhs, const void* rhs) {
    const omni_big_complex* left = as_big_complex_const(lhs);
    const omni_big_complex* right = as_big_complex_const(rhs);
    if (left == nullptr || right == nullptr || !finite_complex(*left) || !finite_complex(*right)) return 0;
    return *left == *right ? 1 : 0;
}

int omni_big_complex_is_zero(const void* value) {
    const omni_big_complex* src = as_big_complex_const(value);
    if (src == nullptr || !finite_complex(*src)) return 0;
    return src->real() == 0 && src->imag() == 0 ? 1 : 0;
}

} // extern "C"
