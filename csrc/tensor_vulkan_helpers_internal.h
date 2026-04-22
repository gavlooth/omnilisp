#ifndef OMNI_TENSOR_VULKAN_HELPERS_INTERNAL_H
#define OMNI_TENSOR_VULKAN_HELPERS_INTERNAL_H

#include <dlfcn.h>
#include <float.h>
#include <fcntl.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define OMNI_TENSOR_VULKAN_UNAVAILABLE 0
#define OMNI_TENSOR_VULKAN_SUCCESS 1
#define OMNI_TENSOR_VULKAN_INVALID -2
#define OMNI_TENSOR_VULKAN_ALLOCATION_FAILED -3
#define OMNI_TENSOR_VULKAN_COPY_FAILED -4
#define OMNI_TENSOR_VULKAN_UNSUPPORTED -5
#define OMNI_TENSOR_VULKAN_EXECUTION_FAILED -6
#define OMNI_TENSOR_VULKAN_SINGULAR -7
#define OMNI_TENSOR_VULKAN_NO_CONVERGENCE -8
#define OMNI_TENSOR_VULKAN_NOT_SYMMETRIC -9
#define OMNI_TENSOR_VULKAN_DOMAIN_ERROR -10
#define OMNI_TENSOR_VULKAN_INVALID_ARGUMENT -11

#define OMNI_VULKAN_QUEUE_COMPUTE_BIT 0x00000002u
#define OMNI_VULKAN_BUFFER_USAGE_TRANSFER_SRC_BIT 0x00000001u
#define OMNI_VULKAN_BUFFER_USAGE_TRANSFER_DST_BIT 0x00000002u
#define OMNI_VULKAN_BUFFER_USAGE_STORAGE_BUFFER_BIT 0x00000020u
#define OMNI_VULKAN_MEMORY_PROPERTY_HOST_VISIBLE_BIT 0x00000001u
#define OMNI_VULKAN_MEMORY_PROPERTY_HOST_COHERENT_BIT 0x00000002u
#define OMNI_VULKAN_STRUCTURE_TYPE_APPLICATION_INFO 0u
#define OMNI_VULKAN_STRUCTURE_TYPE_INSTANCE_CREATE_INFO 1u
#define OMNI_VULKAN_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO 2u
#define OMNI_VULKAN_STRUCTURE_TYPE_DEVICE_CREATE_INFO 3u
#define OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO 4u
#define OMNI_VULKAN_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO 5u
#define OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_CREATE_INFO 12u
#define OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO 16u
#define OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO 18u
#define OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO 29u
#define OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO 30u
#define OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO 32u
#define OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO 33u
#define OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO 34u
#define OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET 35u
#define OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO 39u
#define OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO 40u
#define OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO 42u
#define OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER 44u
#define OMNI_VULKAN_STRUCTURE_TYPE_MEMORY_BARRIER 46u
#define OMNI_VULKAN_SHARING_MODE_EXCLUSIVE 0u
#define OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER 7u
#define OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT 0x00000020u
#define OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE 1u
#define OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY 0u
#define OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT 0x00000001u
#define OMNI_VULKAN_ACCESS_SHADER_READ_BIT 0x00000020u
#define OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT 0x00000040u
#define OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT 0x00000800u
#define OMNI_VULKAN_QUEUE_FAMILY_IGNORED (~0u)
#define OMNI_VULKAN_WHOLE_SIZE (~0ull)
#define OMNI_VULKAN_SUCCESS 0
#define OMNI_VULKAN_API_VERSION_1_0 ((1u << 22) | (0u << 12) | 0u)
#define OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE 64u
#define OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE 64u
#define OMNI_TENSOR_VULKAN_ML_REDUCTION_LOCAL_SIZE 64u
#define OMNI_TENSOR_VULKAN_TRANSPOSE_LOCAL_SIZE 64u
#define OMNI_TENSOR_VULKAN_DIAGONAL_LOCAL_SIZE 64u
#define OMNI_TENSOR_VULKAN_DIAGONAL_MATRIX_LOCAL_SIZE 64u
#define OMNI_TENSOR_VULKAN_TRACE_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_NORM_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_RANK_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_LU_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_DETERMINANT_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_SOLVE_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE 64u
#define OMNI_TENSOR_VULKAN_INVERSE_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_CHOLESKY_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_QR_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_SINGULAR_VALUES_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_SVD_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_SYMMETRIC_EIGEN_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_GENERAL_EIGEN_LOCAL_SIZE 1u
#define OMNI_TENSOR_VULKAN_ROUND_I64_LOCAL_SIZE 64u
#define OMNI_TENSOR_VULKAN_MAX_STORAGE_DESCRIPTORS 8u

typedef uint32_t OmniVulkanFlags;
typedef uint32_t OmniVulkanBool32;
typedef uint64_t OmniVulkanDeviceSize;
typedef int32_t OmniVulkanResult;
typedef struct OmniVulkanInstance_T* OmniVulkanInstance;
typedef struct OmniVulkanPhysicalDevice_T* OmniVulkanPhysicalDevice;
typedef struct OmniVulkanDevice_T* OmniVulkanDevice;
typedef struct OmniVulkanQueue_T* OmniVulkanQueue;
typedef struct OmniVulkanBuffer_T* OmniVulkanBuffer;
typedef struct OmniVulkanDeviceMemory_T* OmniVulkanDeviceMemory;
typedef struct OmniVulkanShaderModule_T* OmniVulkanShaderModule;
typedef struct OmniVulkanDescriptorSetLayout_T* OmniVulkanDescriptorSetLayout;
typedef struct OmniVulkanDescriptorPool_T* OmniVulkanDescriptorPool;
typedef struct OmniVulkanDescriptorSet_T* OmniVulkanDescriptorSet;
typedef struct OmniVulkanPipelineLayout_T* OmniVulkanPipelineLayout;
typedef struct OmniVulkanPipeline_T* OmniVulkanPipeline;
typedef struct OmniVulkanPipelineCache_T* OmniVulkanPipelineCache;
typedef struct OmniVulkanCommandPool_T* OmniVulkanCommandPool;
typedef struct OmniVulkanCommandBuffer_T* OmniVulkanCommandBuffer;
typedef struct OmniVulkanFence_T* OmniVulkanFence;
typedef struct OmniVulkanSemaphore_T* OmniVulkanSemaphore;

typedef struct OmniVulkanApplicationInfo {
    uint32_t sType;
    const void* pNext;
    const char* pApplicationName;
    uint32_t applicationVersion;
    const char* pEngineName;
    uint32_t engineVersion;
    uint32_t apiVersion;
} OmniVulkanApplicationInfo;

typedef struct OmniVulkanInstanceCreateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags flags;
    const OmniVulkanApplicationInfo* pApplicationInfo;
    uint32_t enabledLayerCount;
    const char* const* ppEnabledLayerNames;
    uint32_t enabledExtensionCount;
    const char* const* ppEnabledExtensionNames;
} OmniVulkanInstanceCreateInfo;

typedef struct OmniVulkanExtent3D {
    uint32_t width;
    uint32_t height;
    uint32_t depth;
} OmniVulkanExtent3D;

typedef struct OmniVulkanQueueFamilyProperties {
    OmniVulkanFlags queueFlags;
    uint32_t queueCount;
    uint32_t timestampValidBits;
    OmniVulkanExtent3D minImageTransferGranularity;
} OmniVulkanQueueFamilyProperties;

typedef struct OmniVulkanPhysicalDeviceFeatures {
    OmniVulkanBool32 robustBufferAccess;
    OmniVulkanBool32 fullDrawIndexUint32;
    OmniVulkanBool32 imageCubeArray;
    OmniVulkanBool32 independentBlend;
    OmniVulkanBool32 geometryShader;
    OmniVulkanBool32 tessellationShader;
    OmniVulkanBool32 sampleRateShading;
    OmniVulkanBool32 dualSrcBlend;
    OmniVulkanBool32 logicOp;
    OmniVulkanBool32 multiDrawIndirect;
    OmniVulkanBool32 drawIndirectFirstInstance;
    OmniVulkanBool32 depthClamp;
    OmniVulkanBool32 depthBiasClamp;
    OmniVulkanBool32 fillModeNonSolid;
    OmniVulkanBool32 depthBounds;
    OmniVulkanBool32 wideLines;
    OmniVulkanBool32 largePoints;
    OmniVulkanBool32 alphaToOne;
    OmniVulkanBool32 multiViewport;
    OmniVulkanBool32 samplerAnisotropy;
    OmniVulkanBool32 textureCompressionETC2;
    OmniVulkanBool32 textureCompressionASTC_LDR;
    OmniVulkanBool32 textureCompressionBC;
    OmniVulkanBool32 occlusionQueryPrecise;
    OmniVulkanBool32 pipelineStatisticsQuery;
    OmniVulkanBool32 vertexPipelineStoresAndAtomics;
    OmniVulkanBool32 fragmentStoresAndAtomics;
    OmniVulkanBool32 shaderTessellationAndGeometryPointSize;
    OmniVulkanBool32 shaderImageGatherExtended;
    OmniVulkanBool32 shaderStorageImageExtendedFormats;
    OmniVulkanBool32 shaderStorageImageMultisample;
    OmniVulkanBool32 shaderStorageImageReadWithoutFormat;
    OmniVulkanBool32 shaderStorageImageWriteWithoutFormat;
    OmniVulkanBool32 shaderUniformBufferArrayDynamicIndexing;
    OmniVulkanBool32 shaderSampledImageArrayDynamicIndexing;
    OmniVulkanBool32 shaderStorageBufferArrayDynamicIndexing;
    OmniVulkanBool32 shaderStorageImageArrayDynamicIndexing;
    OmniVulkanBool32 shaderClipDistance;
    OmniVulkanBool32 shaderCullDistance;
    OmniVulkanBool32 shaderFloat64;
    OmniVulkanBool32 shaderInt64;
    OmniVulkanBool32 shaderInt16;
    OmniVulkanBool32 shaderResourceResidency;
    OmniVulkanBool32 shaderResourceMinLod;
    OmniVulkanBool32 sparseBinding;
    OmniVulkanBool32 sparseResidencyBuffer;
    OmniVulkanBool32 sparseResidencyImage2D;
    OmniVulkanBool32 sparseResidencyImage3D;
    OmniVulkanBool32 sparseResidency2Samples;
    OmniVulkanBool32 sparseResidency4Samples;
    OmniVulkanBool32 sparseResidency8Samples;
    OmniVulkanBool32 sparseResidency16Samples;
    OmniVulkanBool32 sparseResidencyAliased;
    OmniVulkanBool32 variableMultisampleRate;
    OmniVulkanBool32 inheritedQueries;
} OmniVulkanPhysicalDeviceFeatures;

typedef struct OmniVulkanDeviceQueueCreateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags flags;
    uint32_t queueFamilyIndex;
    uint32_t queueCount;
    const float* pQueuePriorities;
} OmniVulkanDeviceQueueCreateInfo;

typedef struct OmniVulkanDeviceCreateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags flags;
    uint32_t queueCreateInfoCount;
    const OmniVulkanDeviceQueueCreateInfo* pQueueCreateInfos;
    uint32_t enabledLayerCount;
    const char* const* ppEnabledLayerNames;
    uint32_t enabledExtensionCount;
    const char* const* ppEnabledExtensionNames;
    const OmniVulkanPhysicalDeviceFeatures* pEnabledFeatures;
} OmniVulkanDeviceCreateInfo;

typedef struct OmniVulkanBufferCreateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags flags;
    OmniVulkanDeviceSize size;
    OmniVulkanFlags usage;
    uint32_t sharingMode;
    uint32_t queueFamilyIndexCount;
    const uint32_t* pQueueFamilyIndices;
} OmniVulkanBufferCreateInfo;

typedef struct OmniVulkanMemoryRequirements {
    OmniVulkanDeviceSize size;
    OmniVulkanDeviceSize alignment;
    uint32_t memoryTypeBits;
} OmniVulkanMemoryRequirements;

typedef struct OmniVulkanMemoryAllocateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanDeviceSize allocationSize;
    uint32_t memoryTypeIndex;
} OmniVulkanMemoryAllocateInfo;

typedef struct OmniVulkanShaderModuleCreateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags flags;
    size_t codeSize;
    const uint32_t* pCode;
} OmniVulkanShaderModuleCreateInfo;

typedef struct OmniVulkanDescriptorSetLayoutBinding {
    uint32_t binding;
    uint32_t descriptorType;
    uint32_t descriptorCount;
    OmniVulkanFlags stageFlags;
    const void* pImmutableSamplers;
} OmniVulkanDescriptorSetLayoutBinding;

typedef struct OmniVulkanDescriptorSetLayoutCreateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags flags;
    uint32_t bindingCount;
    const OmniVulkanDescriptorSetLayoutBinding* pBindings;
} OmniVulkanDescriptorSetLayoutCreateInfo;

typedef struct OmniVulkanDescriptorPoolSize {
    uint32_t type;
    uint32_t descriptorCount;
} OmniVulkanDescriptorPoolSize;

typedef struct OmniVulkanDescriptorPoolCreateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags flags;
    uint32_t maxSets;
    uint32_t poolSizeCount;
    const OmniVulkanDescriptorPoolSize* pPoolSizes;
} OmniVulkanDescriptorPoolCreateInfo;

typedef struct OmniVulkanDescriptorSetAllocateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanDescriptorPool descriptorPool;
    uint32_t descriptorSetCount;
    const OmniVulkanDescriptorSetLayout* pSetLayouts;
} OmniVulkanDescriptorSetAllocateInfo;

typedef struct OmniVulkanDescriptorBufferInfo {
    OmniVulkanBuffer buffer;
    OmniVulkanDeviceSize offset;
    OmniVulkanDeviceSize range;
} OmniVulkanDescriptorBufferInfo;

typedef struct OmniVulkanWriteDescriptorSet {
    uint32_t sType;
    const void* pNext;
    OmniVulkanDescriptorSet dstSet;
    uint32_t dstBinding;
    uint32_t dstArrayElement;
    uint32_t descriptorCount;
    uint32_t descriptorType;
    const void* pImageInfo;
    const OmniVulkanDescriptorBufferInfo* pBufferInfo;
    const void* pTexelBufferView;
} OmniVulkanWriteDescriptorSet;

typedef struct OmniVulkanPushConstantRange {
    OmniVulkanFlags stageFlags;
    uint32_t offset;
    uint32_t size;
} OmniVulkanPushConstantRange;

typedef struct OmniVulkanPipelineLayoutCreateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags flags;
    uint32_t setLayoutCount;
    const OmniVulkanDescriptorSetLayout* pSetLayouts;
    uint32_t pushConstantRangeCount;
    const OmniVulkanPushConstantRange* pPushConstantRanges;
} OmniVulkanPipelineLayoutCreateInfo;

typedef struct OmniVulkanPipelineShaderStageCreateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags flags;
    OmniVulkanFlags stage;
    OmniVulkanShaderModule module;
    const char* pName;
    const void* pSpecializationInfo;
} OmniVulkanPipelineShaderStageCreateInfo;

typedef struct OmniVulkanComputePipelineCreateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags flags;
    OmniVulkanPipelineShaderStageCreateInfo stage;
    OmniVulkanPipelineLayout layout;
    OmniVulkanPipeline basePipelineHandle;
    int32_t basePipelineIndex;
} OmniVulkanComputePipelineCreateInfo;

typedef struct OmniVulkanCommandPoolCreateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags flags;
    uint32_t queueFamilyIndex;
} OmniVulkanCommandPoolCreateInfo;

typedef struct OmniVulkanCommandBufferAllocateInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanCommandPool commandPool;
    uint32_t level;
    uint32_t commandBufferCount;
} OmniVulkanCommandBufferAllocateInfo;

typedef struct OmniVulkanCommandBufferBeginInfo {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags flags;
    const void* pInheritanceInfo;
} OmniVulkanCommandBufferBeginInfo;

typedef struct OmniVulkanMemoryBarrier {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags srcAccessMask;
    OmniVulkanFlags dstAccessMask;
} OmniVulkanMemoryBarrier;

typedef struct OmniVulkanBufferMemoryBarrier {
    uint32_t sType;
    const void* pNext;
    OmniVulkanFlags srcAccessMask;
    OmniVulkanFlags dstAccessMask;
    uint32_t srcQueueFamilyIndex;
    uint32_t dstQueueFamilyIndex;
    OmniVulkanBuffer buffer;
    OmniVulkanDeviceSize offset;
    OmniVulkanDeviceSize size;
} OmniVulkanBufferMemoryBarrier;

typedef struct OmniVulkanSubmitInfo {
    uint32_t sType;
    const void* pNext;
    uint32_t waitSemaphoreCount;
    const OmniVulkanSemaphore* pWaitSemaphores;
    const OmniVulkanFlags* pWaitDstStageMask;
    uint32_t commandBufferCount;
    const OmniVulkanCommandBuffer* pCommandBuffers;
    uint32_t signalSemaphoreCount;
    const OmniVulkanSemaphore* pSignalSemaphores;
} OmniVulkanSubmitInfo;

typedef struct OmniTensorVulkanAddScalarPushConstants {
    double scalar;
    uint32_t count;
    uint32_t padding;
} OmniTensorVulkanAddScalarPushConstants;

typedef struct OmniTensorVulkanMapPushConstants {
    double scalar;
    uint32_t count;
    uint32_t op;
    uint32_t mode;
    uint32_t out_rank;
    uint32_t left_rank;
    uint32_t right_rank;
    uint32_t padding;
} OmniTensorVulkanMapPushConstants;

typedef struct OmniTensorVulkanMapF32PushConstants {
    float scalar;
    uint32_t count;
    uint32_t op;
    uint32_t mode;
    uint32_t out_rank;
    uint32_t left_rank;
    uint32_t right_rank;
    uint32_t padding;
} OmniTensorVulkanMapF32PushConstants;

typedef struct OmniTensorVulkanMapComplex128PushConstants {
    double scalar_real;
    double scalar_imag;
    uint32_t count;
    uint32_t op;
    uint32_t mode;
    uint32_t out_rank;
    uint32_t left_rank;
    uint32_t right_rank;
} OmniTensorVulkanMapComplex128PushConstants;

typedef struct OmniTensorVulkanMapComplex64PushConstants {
    float scalar_real;
    float scalar_imag;
    uint32_t count;
    uint32_t op;
    uint32_t mode;
    uint32_t out_rank;
    uint32_t left_rank;
    uint32_t right_rank;
    uint32_t padding;
} OmniTensorVulkanMapComplex64PushConstants;

typedef struct OmniTensorVulkanMapUnaryPushConstants {
    uint32_t count;
    uint32_t op;
    uint32_t padding0;
    uint32_t padding1;
} OmniTensorVulkanMapUnaryPushConstants;

typedef struct OmniTensorVulkanDotPushConstants {
    uint32_t count;
} OmniTensorVulkanDotPushConstants;

typedef struct OmniTensorVulkanContractPushConstants {
    uint32_t left_rank;
    uint32_t right_rank;
    uint32_t axis_count;
    uint32_t out_count;
    uint32_t out_rank;
} OmniTensorVulkanContractPushConstants;

typedef struct OmniTensorVulkanMlReductionPushConstants {
    uint32_t input_rank;
    uint32_t axis_count;
    uint32_t out_count;
    uint32_t out_rank;
    uint32_t op;
    uint32_t reduction_count;
} OmniTensorVulkanMlReductionPushConstants;

typedef struct OmniTensorVulkanMlLayerNormPushConstants {
    uint32_t input_rank;
    uint32_t out_count;
    uint32_t axis;
    uint32_t reduction_count;
    float epsilon;
} OmniTensorVulkanMlLayerNormPushConstants;

typedef struct OmniTensorVulkanMlLayerNormPushConstantsF64 {
    uint32_t input_rank;
    uint32_t out_count;
    uint32_t axis;
    uint32_t reduction_count;
    double epsilon;
} OmniTensorVulkanMlLayerNormPushConstantsF64;

typedef struct OmniTensorVulkanMlBatchNormPushConstants {
    uint32_t input_rank;
    uint32_t out_count;
    uint32_t channel_axis;
    uint32_t channel_dim;
    float epsilon;
} OmniTensorVulkanMlBatchNormPushConstants;

typedef struct OmniTensorVulkanMlBatchNormPushConstantsF64 {
    uint32_t input_rank;
    uint32_t out_count;
    uint32_t channel_axis;
    uint32_t channel_dim;
    double epsilon;
} OmniTensorVulkanMlBatchNormPushConstantsF64;

typedef struct OmniTensorVulkanMlAttentionPushConstants {
    uint32_t batch_count;
    uint32_t query_len;
    uint32_t key_len;
    uint32_t head_dim;
    uint32_t value_dim;
    uint32_t out_count;
    uint32_t mask_kind;
    float scale;
} OmniTensorVulkanMlAttentionPushConstants;

typedef struct OmniTensorVulkanTransposePushConstants {
    uint32_t rows;
    uint32_t cols;
    uint32_t count;
    uint32_t padding;
} OmniTensorVulkanTransposePushConstants;

typedef struct OmniTensorVulkanDiagonalPushConstants {
    uint32_t rows;
    uint32_t cols;
    uint32_t count;
    uint32_t padding;
} OmniTensorVulkanDiagonalPushConstants;

typedef struct OmniTensorVulkanDiagonalMatrixPushConstants {
    uint32_t n;
    uint32_t count;
    uint32_t padding0;
    uint32_t padding1;
} OmniTensorVulkanDiagonalMatrixPushConstants;

typedef struct OmniTensorVulkanTracePushConstants {
    uint32_t n;
    uint32_t padding0;
    uint32_t padding1;
    uint32_t padding2;
} OmniTensorVulkanTracePushConstants;

typedef struct OmniTensorVulkanNormPushConstants {
    uint32_t rows;
    uint32_t cols;
    uint32_t mode;
    uint32_t padding;
} OmniTensorVulkanNormPushConstants;

typedef struct OmniTensorVulkanRankPushConstants {
    double tolerance;
    uint32_t rows;
    uint32_t cols;
} OmniTensorVulkanRankPushConstants;

typedef struct OmniTensorVulkanRankF32PushConstants {
    float tolerance;
    uint32_t rows;
    uint32_t cols;
    uint32_t padding;
} OmniTensorVulkanRankF32PushConstants;

typedef struct OmniTensorVulkanDeterminantPushConstants {
    uint32_t n;
    uint32_t padding0;
    uint32_t padding1;
    uint32_t padding2;
} OmniTensorVulkanDeterminantPushConstants;

typedef struct OmniTensorVulkanLuPushConstants {
    uint32_t n;
    uint32_t padding0;
    uint32_t padding1;
    uint32_t padding2;
} OmniTensorVulkanLuPushConstants;

typedef struct OmniTensorVulkanInversePushConstants {
    uint32_t n;
    uint32_t padding0;
    uint32_t padding1;
    uint32_t padding2;
} OmniTensorVulkanInversePushConstants;

typedef struct OmniTensorVulkanSolvePushConstants {
    uint32_t n;
    uint32_t rhs_cols;
    uint32_t result_count;
    uint32_t padding;
} OmniTensorVulkanSolvePushConstants;

typedef struct OmniTensorVulkanSolveMultiPushConstants {
    uint32_t n;
    uint32_t rhs_cols;
    uint32_t result_count;
    uint32_t pivot;
    uint32_t work_count;
    uint32_t scratch_src;
    uint32_t scratch_dst;
    uint32_t scratch_count;
} OmniTensorVulkanSolveMultiPushConstants;

typedef struct OmniTensorVulkanCholeskyPushConstants {
    uint32_t n;
    uint32_t padding0;
    uint32_t padding1;
    uint32_t padding2;
} OmniTensorVulkanCholeskyPushConstants;

typedef struct OmniTensorVulkanQrPushConstants {
    uint32_t rows;
    uint32_t cols;
    uint32_t q_count;
    uint32_t r_count;
} OmniTensorVulkanQrPushConstants;

typedef struct OmniTensorVulkanSingularValuesPushConstants {
    uint32_t rows;
    uint32_t cols;
    uint32_t k;
    uint32_t output_count;
} OmniTensorVulkanSingularValuesPushConstants;

typedef struct OmniTensorVulkanSvdPushConstants {
    uint32_t rows;
    uint32_t cols;
    uint32_t k;
    uint32_t u_count;
    uint32_t s_count;
    uint32_t v_count;
} OmniTensorVulkanSvdPushConstants;

typedef struct OmniTensorVulkanSymmetricEigenPushConstants {
    uint32_t n;
    uint32_t value_count;
    uint32_t vector_count;
    uint32_t reserved;
} OmniTensorVulkanSymmetricEigenPushConstants;

typedef struct OmniTensorVulkanGeneralEigenPushConstants {
    uint32_t n;
    uint32_t value_count;
    uint32_t vector_count;
    uint32_t vector_storage_count;
    uint32_t max_iterations;
    uint32_t reserved;
} OmniTensorVulkanGeneralEigenPushConstants;

typedef struct OmniVulkanMemoryType {
    OmniVulkanFlags propertyFlags;
    uint32_t heapIndex;
} OmniVulkanMemoryType;

typedef struct OmniVulkanMemoryHeap {
    OmniVulkanDeviceSize size;
    OmniVulkanFlags flags;
} OmniVulkanMemoryHeap;

typedef struct OmniVulkanPhysicalDeviceMemoryProperties {
    uint32_t memoryTypeCount;
    OmniVulkanMemoryType memoryTypes[32];
    uint32_t memoryHeapCount;
    OmniVulkanMemoryHeap memoryHeaps[16];
} OmniVulkanPhysicalDeviceMemoryProperties;

typedef struct OmniTensorVulkanContext {
    OmniVulkanInstance instance;
    OmniVulkanPhysicalDevice physical_device;
    OmniVulkanDevice device;
    uint32_t queue_family_index;
    uint32_t ref_count;
} OmniTensorVulkanContext;

typedef struct OmniTensorVulkanBuffer {
    OmniTensorVulkanContext* context;
    OmniVulkanBuffer buffer;
    OmniVulkanDeviceMemory memory;
    size_t byte_len;
    uint32_t ref_count;
} OmniTensorVulkanBuffer;

typedef struct OmniTensorVulkanStorageBufferDescriptor {
    OmniVulkanBuffer buffer;
    OmniVulkanDeviceSize range;
} OmniTensorVulkanStorageBufferDescriptor;

typedef struct OmniTensorVulkanDescriptorResources {
    OmniVulkanDescriptorSetLayout layout;
    OmniVulkanDescriptorPool pool;
    OmniVulkanDescriptorSet set;
} OmniTensorVulkanDescriptorResources;

typedef int (*OmniTensorVulkanStatusMapperF64)(double status_payload);
typedef int (*OmniTensorVulkanStatusMapperF32)(float status_payload);


#include "tensor_vulkan_helpers_spv_decls.h"
#include "tensor_vulkan_helpers_runtime_decls.h"
#include "tensor_vulkan_helpers_api_decls.h"

#endif
