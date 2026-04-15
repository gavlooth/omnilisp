#include <boost/multiprecision/cpp_int.hpp>

#include <cmath>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <string>

using boost::multiprecision::cpp_int;

namespace {

cpp_int* as_big_integer(void* value) {
    return static_cast<cpp_int*>(value);
}

const cpp_int* as_big_integer_const(const void* value) {
    return static_cast<const cpp_int*>(value);
}

bool parse_decimal(const char* chars, size_t len, cpp_int& out) {
    if (chars == nullptr || len == 0) return false;

    size_t pos = 0;
    bool negative = false;
    if (chars[pos] == '-' || chars[pos] == '+') {
        negative = chars[pos] == '-';
        pos++;
    }
    if (pos >= len) return false;

    cpp_int value = 0;
    for (; pos < len; pos++) {
        char ch = chars[pos];
        if (ch < '0' || ch > '9') return false;
        value *= 10;
        value += (int)(ch - '0');
    }

    out = negative ? -value : value;
    return true;
}

cpp_int abs_value(const cpp_int& value) {
    return value < 0 ? -value : value;
}

cpp_int gcd_value(cpp_int lhs, cpp_int rhs) {
    lhs = abs_value(lhs);
    rhs = abs_value(rhs);
    while (rhs != 0) {
        cpp_int next = lhs % rhs;
        lhs = rhs;
        rhs = next;
    }
    return lhs;
}

cpp_int lcm_value(const cpp_int& lhs, const cpp_int& rhs) {
    if (lhs == 0 || rhs == 0) return 0;
    cpp_int gcd = gcd_value(lhs, rhs);
    return abs_value((lhs / gcd) * rhs);
}

cpp_int apply_binary(const cpp_int& lhs, const cpp_int& rhs, int op) {
    switch (op) {
        case 1: return lhs + rhs;
        case 2: return lhs - rhs;
        case 3: return lhs * rhs;
        case 4: return lhs / rhs;
        case 5: return lhs % rhs;
        case 6: return gcd_value(lhs, rhs);
        case 7: return lcm_value(lhs, rhs);
        case 8: return lhs & rhs;
        case 9: return lhs | rhs;
        case 10: return lhs ^ rhs;
        default: return 0;
    }
}

bool valid_binary_op(int op) {
    return op >= 1 && op <= 10;
}

} // namespace

extern "C" {

void* omni_big_integer_from_i64(long value) {
    try {
        return new cpp_int(value);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_from_chars(const char* chars, size_t len) {
    try {
        cpp_int value;
        if (!parse_decimal(chars, len, value)) return nullptr;
        return new cpp_int(value);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_clone(const void* value) {
    try {
        const cpp_int* src = as_big_integer_const(value);
        if (src == nullptr) return nullptr;
        return new cpp_int(*src);
    } catch (...) {
        return nullptr;
    }
}

void omni_big_integer_free(void* value) {
    delete as_big_integer(value);
}

char* omni_big_integer_to_string(const void* value, size_t* out_len) {
    try {
        const cpp_int* src = as_big_integer_const(value);
        if (src == nullptr) return nullptr;
        std::string text = src->convert_to<std::string>();
        char* out = static_cast<char*>(std::malloc(text.size() + 1));
        if (out == nullptr) return nullptr;
        std::memcpy(out, text.data(), text.size());
        out[text.size()] = '\0';
        if (out_len != nullptr) *out_len = text.size();
        return out;
    } catch (...) {
        return nullptr;
    }
}

void omni_big_integer_string_free(char* value) {
    std::free(value);
}

int omni_big_integer_fits_i64(const void* value) {
    try {
        const cpp_int* src = as_big_integer_const(value);
        if (src == nullptr) return 0;
        cpp_int min = std::numeric_limits<long>::min();
        cpp_int max = std::numeric_limits<long>::max();
        return (*src >= min && *src <= max) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

int omni_big_integer_to_i64(const void* value, long* out) {
    try {
        const cpp_int* src = as_big_integer_const(value);
        if (src == nullptr || out == nullptr || !omni_big_integer_fits_i64(value)) return 0;
        *out = static_cast<long>(*src);
        return 1;
    } catch (...) {
        return 0;
    }
}

int omni_big_integer_to_double(const void* value, double* out) {
    try {
        const cpp_int* src = as_big_integer_const(value);
        if (src == nullptr || out == nullptr) return 0;
        double converted = src->convert_to<double>();
        if (!std::isfinite(converted)) return 0;
        *out = converted;
        return 1;
    } catch (...) {
        return 0;
    }
}

int omni_big_integer_compare(const void* lhs, const void* rhs) {
    const cpp_int* left = as_big_integer_const(lhs);
    const cpp_int* right = as_big_integer_const(rhs);
    if (left == nullptr || right == nullptr) return 0;
    if (*left < *right) return -1;
    if (*left > *right) return 1;
    return 0;
}

int omni_big_integer_compare_i64(const void* lhs, long rhs) {
    const cpp_int* left = as_big_integer_const(lhs);
    if (left == nullptr) return 0;
    cpp_int right(rhs);
    if (*left < right) return -1;
    if (*left > right) return 1;
    return 0;
}

void* omni_big_integer_binary(const void* lhs, const void* rhs, int op) {
    try {
        const cpp_int* left = as_big_integer_const(lhs);
        const cpp_int* right = as_big_integer_const(rhs);
        if (left == nullptr || right == nullptr || !valid_binary_op(op)) return nullptr;
        return new cpp_int(apply_binary(*left, *right, op));
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_binary_i64(long lhs, long rhs, int op) {
    try {
        if (!valid_binary_op(op)) return nullptr;
        cpp_int left(lhs);
        cpp_int right(rhs);
        return new cpp_int(apply_binary(left, right, op));
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_binary_left_i64(long lhs, const void* rhs, int op) {
    try {
        const cpp_int* right = as_big_integer_const(rhs);
        if (right == nullptr || !valid_binary_op(op)) return nullptr;
        cpp_int left(lhs);
        return new cpp_int(apply_binary(left, *right, op));
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_binary_right_i64(const void* lhs, long rhs, int op) {
    try {
        const cpp_int* left = as_big_integer_const(lhs);
        if (left == nullptr || !valid_binary_op(op)) return nullptr;
        cpp_int right(rhs);
        return new cpp_int(apply_binary(*left, right, op));
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_neg(const void* value) {
    try {
        const cpp_int* src = as_big_integer_const(value);
        if (src == nullptr) return nullptr;
        return new cpp_int(-(*src));
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_neg_i64(long value) {
    try {
        cpp_int src(value);
        return new cpp_int(-src);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_bitwise_not(const void* value) {
    try {
        const cpp_int* src = as_big_integer_const(value);
        if (src == nullptr) return nullptr;
        return new cpp_int(~(*src));
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_bitwise_not_i64(long value) {
    try {
        cpp_int src(value);
        return new cpp_int(~src);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_shift_left(const void* value, unsigned long shift) {
    try {
        const cpp_int* src = as_big_integer_const(value);
        if (src == nullptr) return nullptr;
        return new cpp_int((*src) << shift);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_shift_left_i64(long value, unsigned long shift) {
    try {
        cpp_int src(value);
        return new cpp_int(src << shift);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_shift_right(const void* value, unsigned long shift) {
    try {
        const cpp_int* src = as_big_integer_const(value);
        if (src == nullptr) return nullptr;
        return new cpp_int((*src) >> shift);
    } catch (...) {
        return nullptr;
    }
}

void* omni_big_integer_shift_right_i64(long value, unsigned long shift) {
    try {
        cpp_int src(value);
        return new cpp_int(src >> shift);
    } catch (...) {
        return nullptr;
    }
}

}
