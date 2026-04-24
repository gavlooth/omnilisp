#include <math.h>
#include <stddef.h>

#define OMNI_CUDA_ML_ADAM_MIN_CORRECTION 1.17549435e-38f

extern "C" __global__ void omni_cuda_ml_sgd_f32(
    const float* params,
    const float* grads,
    const float* velocity,
    float learning_rate,
    float momentum,
    float weight_decay,
    unsigned int has_velocity,
    size_t element_count,
    float* out_params,
    float* out_velocity
) {
    size_t idx = (size_t)blockIdx.x * (size_t)blockDim.x + (size_t)threadIdx.x;
    if (idx >= element_count) return;

    float param_value = params[idx];
    float effective_grad = grads[idx] + weight_decay * param_value;
    if (momentum > 0.0f) {
        float next_velocity = effective_grad;
        if (has_velocity != 0u) next_velocity += momentum * velocity[idx];
        if (out_velocity != NULL) out_velocity[idx] = next_velocity;
        out_params[idx] = param_value - learning_rate * next_velocity;
        return;
    }
    out_params[idx] = param_value - learning_rate * effective_grad;
}

extern "C" __global__ void omni_cuda_ml_adam_f32(
    const float* params,
    const float* grads,
    const float* first,
    const float* second,
    float learning_rate,
    float beta1,
    float beta2,
    float epsilon,
    float weight_decay,
    float first_correction,
    float second_correction,
    unsigned int decoupled_weight_decay,
    unsigned int has_moments,
    size_t element_count,
    float* out_params,
    float* out_first,
    float* out_second
) {
    size_t idx = (size_t)blockIdx.x * (size_t)blockDim.x + (size_t)threadIdx.x;
    if (idx >= element_count) return;

    float param_value = params[idx];
    float grad_value = grads[idx];
    float moment_grad = decoupled_weight_decay != 0u ? grad_value : grad_value + weight_decay * param_value;
    float old_first = has_moments != 0u ? first[idx] : 0.0f;
    float old_second = has_moments != 0u ? second[idx] : 0.0f;
    float next_first = beta1 * old_first + (1.0f - beta1) * moment_grad;
    float next_second = beta2 * old_second + (1.0f - beta2) * moment_grad * moment_grad;
    if (!isfinite(first_correction) || !isfinite(second_correction) ||
        first_correction < OMNI_CUDA_ML_ADAM_MIN_CORRECTION ||
        second_correction < OMNI_CUDA_ML_ADAM_MIN_CORRECTION) {
        out_params[idx] = param_value;
        out_first[idx] = next_first;
        out_second[idx] = next_second;
        return;
    }
    float first_hat = next_first / first_correction;
    float second_hat = next_second / second_correction;
    float decay_update = decoupled_weight_decay != 0u ? learning_rate * weight_decay * param_value : 0.0f;

    out_params[idx] = param_value - decay_update - (learning_rate * first_hat / (sqrtf(second_hat) + epsilon));
    out_first[idx] = next_first;
    out_second[idx] = next_second;
}

extern "C" __global__ void omni_cuda_ml_rmsprop_f32(
    const float* params,
    const float* grads,
    const float* square,
    const float* velocity,
    float learning_rate,
    float alpha,
    float epsilon,
    float momentum,
    float weight_decay,
    unsigned int has_square,
    unsigned int has_velocity,
    size_t element_count,
    float* out_params,
    float* out_square,
    float* out_velocity
) {
    size_t idx = (size_t)blockIdx.x * (size_t)blockDim.x + (size_t)threadIdx.x;
    if (idx >= element_count) return;

    float param_value = params[idx];
    float effective_grad = grads[idx] + weight_decay * param_value;
    float old_square = has_square != 0u ? square[idx] : 0.0f;
    float next_square = alpha * old_square + (1.0f - alpha) * effective_grad * effective_grad;
    float scaled_update = learning_rate * effective_grad / (sqrtf(next_square) + epsilon);
    out_square[idx] = next_square;

    if (momentum > 0.0f) {
        float old_velocity = has_velocity != 0u ? velocity[idx] : 0.0f;
        float next_velocity = momentum * old_velocity + scaled_update;
        if (out_velocity != NULL) out_velocity[idx] = next_velocity;
        out_params[idx] = param_value - next_velocity;
        return;
    }
    out_params[idx] = param_value - scaled_update;
}
