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

extern const uint32_t omni_tensor_vulkan_map_add_scalar_f64_spv[];
extern const size_t omni_tensor_vulkan_map_add_scalar_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_map_f32_spv[];
extern const size_t omni_tensor_vulkan_map_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_map_f64_spv[];
extern const size_t omni_tensor_vulkan_map_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_map_complex128_spv[];
extern const size_t omni_tensor_vulkan_map_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_map_complex64_spv[];
extern const size_t omni_tensor_vulkan_map_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_map_complex128_unary_spv[];
extern const size_t omni_tensor_vulkan_map_complex128_unary_spv_size;
extern const uint32_t omni_tensor_vulkan_map_complex64_unary_spv[];
extern const size_t omni_tensor_vulkan_map_complex64_unary_spv_size;
extern const uint32_t omni_tensor_vulkan_map_complex128_to_real_spv[];
extern const size_t omni_tensor_vulkan_map_complex128_to_real_spv_size;
extern const uint32_t omni_tensor_vulkan_map_complex64_to_real_spv[];
extern const size_t omni_tensor_vulkan_map_complex64_to_real_spv_size;
extern const uint32_t omni_tensor_vulkan_map_unary_f32_spv[];
extern const size_t omni_tensor_vulkan_map_unary_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_map_unary_f64_spv[];
extern const size_t omni_tensor_vulkan_map_unary_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_normal_quantile_f32_spv[];
extern const size_t omni_tensor_vulkan_normal_quantile_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_normal_quantile_f64_spv[];
extern const size_t omni_tensor_vulkan_normal_quantile_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_round_i64_f32_spv[];
extern const size_t omni_tensor_vulkan_round_i64_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_round_i64_f64_spv[];
extern const size_t omni_tensor_vulkan_round_i64_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_contract_dot_f64_spv[];
extern const size_t omni_tensor_vulkan_contract_dot_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_contract_f32_spv[];
extern const size_t omni_tensor_vulkan_contract_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_contract_f64_spv[];
extern const size_t omni_tensor_vulkan_contract_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_contract_complex128_spv[];
extern const size_t omni_tensor_vulkan_contract_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_contract_complex64_spv[];
extern const size_t omni_tensor_vulkan_contract_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_transpose_f32_spv[];
extern const size_t omni_tensor_vulkan_transpose_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_transpose_f64_spv[];
extern const size_t omni_tensor_vulkan_transpose_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_transpose_complex128_spv[];
extern const size_t omni_tensor_vulkan_transpose_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_transpose_complex64_spv[];
extern const size_t omni_tensor_vulkan_transpose_complex64_spv_size;

static int omni_tensor_vulkan_create_compute_pipeline_for_shader(
    OmniVulkanDevice device,
    OmniVulkanPipelineLayout pipeline_layout,
    const uint32_t* shader_words,
    size_t shader_size,
    OmniVulkanShaderModule* out_shader_module,
    OmniVulkanPipeline* out_pipeline
);

static int omni_tensor_vulkan_create_storage_descriptor_set_layout(
    OmniVulkanDevice device,
    uint32_t binding_count,
    OmniTensorVulkanDescriptorResources* resources
);

static int omni_tensor_vulkan_allocate_storage_descriptor_set(
    OmniVulkanDevice device,
    const OmniTensorVulkanStorageBufferDescriptor* descriptors,
    uint32_t descriptor_count,
    OmniTensorVulkanDescriptorResources* resources
);

static void omni_tensor_vulkan_destroy_storage_descriptor_resources(
    OmniVulkanDevice device,
    OmniTensorVulkanDescriptorResources* resources
);

static int omni_tensor_vulkan_record_submit_single_dispatch(
    OmniVulkanDevice device,
    OmniVulkanQueue queue,
    uint32_t queue_family_index,
    OmniVulkanPipeline pipeline,
    OmniVulkanPipelineLayout pipeline_layout,
    OmniVulkanDescriptorSet descriptor_set,
    const void* push_data,
    uint32_t push_size,
    uint32_t group_count
);

static int omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
);

static int omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
);
extern const uint32_t omni_tensor_vulkan_diagonal_f64_spv[];
extern const size_t omni_tensor_vulkan_diagonal_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_diagonal_f32_spv[];
extern const size_t omni_tensor_vulkan_diagonal_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_diagonal_complex128_spv[];
extern const size_t omni_tensor_vulkan_diagonal_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_diagonal_complex64_spv[];
extern const size_t omni_tensor_vulkan_diagonal_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_diagonal_matrix_f64_spv[];
extern const size_t omni_tensor_vulkan_diagonal_matrix_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_diagonal_matrix_f32_spv[];
extern const size_t omni_tensor_vulkan_diagonal_matrix_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_diagonal_matrix_complex128_spv[];
extern const size_t omni_tensor_vulkan_diagonal_matrix_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_diagonal_matrix_complex64_spv[];
extern const size_t omni_tensor_vulkan_diagonal_matrix_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_trace_f64_spv[];
extern const size_t omni_tensor_vulkan_trace_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_trace_f32_spv[];
extern const size_t omni_tensor_vulkan_trace_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_trace_complex128_spv[];
extern const size_t omni_tensor_vulkan_trace_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_trace_complex64_spv[];
extern const size_t omni_tensor_vulkan_trace_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_norm_f64_spv[];
extern const size_t omni_tensor_vulkan_norm_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_norm_f32_spv[];
extern const size_t omni_tensor_vulkan_norm_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_norm_complex128_spv[];
extern const size_t omni_tensor_vulkan_norm_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_norm_complex64_spv[];
extern const size_t omni_tensor_vulkan_norm_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_rank_f64_spv[];
extern const size_t omni_tensor_vulkan_rank_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_rank_f32_spv[];
extern const size_t omni_tensor_vulkan_rank_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_rank_complex128_spv[];
extern const size_t omni_tensor_vulkan_rank_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_rank_complex64_spv[];
extern const size_t omni_tensor_vulkan_rank_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_lu_f64_spv[];
extern const size_t omni_tensor_vulkan_lu_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_determinant_f64_spv[];
extern const size_t omni_tensor_vulkan_determinant_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_lu_f32_spv[];
extern const size_t omni_tensor_vulkan_lu_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_determinant_f32_spv[];
extern const size_t omni_tensor_vulkan_determinant_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_lu_complex128_spv[];
extern const size_t omni_tensor_vulkan_lu_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_determinant_complex128_spv[];
extern const size_t omni_tensor_vulkan_determinant_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_lu_complex64_spv[];
extern const size_t omni_tensor_vulkan_lu_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_determinant_complex64_spv[];
extern const size_t omni_tensor_vulkan_determinant_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_f64_spv[];
extern const size_t omni_tensor_vulkan_solve_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_f32_spv[];
extern const size_t omni_tensor_vulkan_solve_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_complex128_spv[];
extern const size_t omni_tensor_vulkan_solve_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_complex64_spv[];
extern const size_t omni_tensor_vulkan_solve_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_inverse_complex128_spv[];
extern const size_t omni_tensor_vulkan_inverse_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_inverse_complex64_spv[];
extern const size_t omni_tensor_vulkan_inverse_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_cholesky_complex128_spv[];
extern const size_t omni_tensor_vulkan_cholesky_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_cholesky_complex64_spv[];
extern const size_t omni_tensor_vulkan_cholesky_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_qr_complex128_spv[];
extern const size_t omni_tensor_vulkan_qr_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_qr_complex64_spv[];
extern const size_t omni_tensor_vulkan_qr_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_parallel_init_f32_spv[];
extern const size_t omni_tensor_vulkan_solve_parallel_init_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_parallel_init_f64_spv[];
extern const size_t omni_tensor_vulkan_solve_parallel_init_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_parallel_f32_spv[];
extern const size_t omni_tensor_vulkan_solve_parallel_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_parallel_f64_spv[];
extern const size_t omni_tensor_vulkan_solve_parallel_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_pivot_f32_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_pivot_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_pivot_f64_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_pivot_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_pivot_reduce_f32_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_pivot_reduce_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_pivot_reduce_f64_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_pivot_reduce_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_pivot_commit_f32_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_pivot_commit_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_pivot_commit_f64_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_pivot_commit_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_row_swap_f32_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_row_swap_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_row_swap_f64_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_row_swap_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_factor_f32_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_factor_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_factor_f64_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_factor_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_eliminate_f32_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_eliminate_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_eliminate_f64_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_eliminate_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_backsolve_f32_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_backsolve_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_solve_multi_backsolve_f64_spv[];
extern const size_t omni_tensor_vulkan_solve_multi_backsolve_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_inverse_f64_spv[];
extern const size_t omni_tensor_vulkan_inverse_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_cholesky_f64_spv[];
extern const size_t omni_tensor_vulkan_cholesky_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_qr_f64_spv[];
extern const size_t omni_tensor_vulkan_qr_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_inverse_f32_spv[];
extern const size_t omni_tensor_vulkan_inverse_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_cholesky_f32_spv[];
extern const size_t omni_tensor_vulkan_cholesky_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_qr_f32_spv[];
extern const size_t omni_tensor_vulkan_qr_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_singular_values_f64_spv[];
extern const size_t omni_tensor_vulkan_singular_values_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_singular_values_f32_spv[];
extern const size_t omni_tensor_vulkan_singular_values_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_singular_values_complex128_spv[];
extern const size_t omni_tensor_vulkan_singular_values_complex128_spv_size;
extern const uint32_t omni_tensor_vulkan_singular_values_complex64_spv[];
extern const size_t omni_tensor_vulkan_singular_values_complex64_spv_size;
extern const uint32_t omni_tensor_vulkan_svd_f64_spv[];
extern const size_t omni_tensor_vulkan_svd_f64_spv_size;
extern const uint32_t omni_tensor_vulkan_svd_f32_spv[];
extern const size_t omni_tensor_vulkan_svd_f32_spv_size;
extern const uint32_t omni_tensor_vulkan_symmetric_eigen_f64_spv[];
extern const size_t omni_tensor_vulkan_symmetric_eigen_f64_spv_size;

static long g_omni_tensor_vulkan_solve_parallel_call_count = 0;
static long g_omni_tensor_vulkan_solve_multi_dispatch_call_count = 0;
static long g_omni_tensor_vulkan_solve_pivot_reduce_call_count = 0;
static long g_omni_tensor_vulkan_solve_factor_stage_call_count = 0;
static long g_omni_tensor_vulkan_solve_serial_call_count = 0;
static long g_omni_tensor_vulkan_solve_parallel_f32_call_count = 0;
static long g_omni_tensor_vulkan_solve_multi_dispatch_f32_call_count = 0;
static long g_omni_tensor_vulkan_solve_pivot_reduce_f32_call_count = 0;
static long g_omni_tensor_vulkan_solve_factor_stage_f32_call_count = 0;
static long g_omni_tensor_vulkan_solve_serial_f32_call_count = 0;
static long g_omni_tensor_vulkan_solve_parallel_f64_call_count = 0;
static long g_omni_tensor_vulkan_solve_multi_dispatch_f64_call_count = 0;
static long g_omni_tensor_vulkan_solve_pivot_reduce_f64_call_count = 0;
static long g_omni_tensor_vulkan_solve_factor_stage_f64_call_count = 0;
static long g_omni_tensor_vulkan_solve_serial_f64_call_count = 0;

typedef OmniVulkanResult (*omni_vulkan_create_instance_fn)(
    const OmniVulkanInstanceCreateInfo* create_info,
    const void* allocator,
    OmniVulkanInstance* instance
);
typedef void (*omni_vulkan_destroy_instance_fn)(OmniVulkanInstance instance, const void* allocator);
typedef OmniVulkanResult (*omni_vulkan_enumerate_physical_devices_fn)(
    OmniVulkanInstance instance,
    uint32_t* physical_device_count,
    OmniVulkanPhysicalDevice* physical_devices
);
typedef void (*omni_vulkan_get_queue_family_properties_fn)(
    OmniVulkanPhysicalDevice physical_device,
    uint32_t* queue_family_property_count,
    OmniVulkanQueueFamilyProperties* queue_family_properties
);
typedef void (*omni_vulkan_get_physical_device_features_fn)(
    OmniVulkanPhysicalDevice physical_device,
    OmniVulkanPhysicalDeviceFeatures* features
);
typedef void (*omni_vulkan_get_physical_device_memory_properties_fn)(
    OmniVulkanPhysicalDevice physical_device,
    OmniVulkanPhysicalDeviceMemoryProperties* memory_properties
);
typedef OmniVulkanResult (*omni_vulkan_create_device_fn)(
    OmniVulkanPhysicalDevice physical_device,
    const OmniVulkanDeviceCreateInfo* create_info,
    const void* allocator,
    OmniVulkanDevice* device
);
typedef void (*omni_vulkan_destroy_device_fn)(OmniVulkanDevice device, const void* allocator);
typedef OmniVulkanResult (*omni_vulkan_create_buffer_fn)(
    OmniVulkanDevice device,
    const OmniVulkanBufferCreateInfo* create_info,
    const void* allocator,
    OmniVulkanBuffer* buffer
);
typedef void (*omni_vulkan_destroy_buffer_fn)(OmniVulkanDevice device, OmniVulkanBuffer buffer, const void* allocator);
typedef void (*omni_vulkan_get_buffer_memory_requirements_fn)(
    OmniVulkanDevice device,
    OmniVulkanBuffer buffer,
    OmniVulkanMemoryRequirements* memory_requirements
);
typedef OmniVulkanResult (*omni_vulkan_allocate_memory_fn)(
    OmniVulkanDevice device,
    const OmniVulkanMemoryAllocateInfo* allocate_info,
    const void* allocator,
    OmniVulkanDeviceMemory* memory
);
typedef void (*omni_vulkan_free_memory_fn)(OmniVulkanDevice device, OmniVulkanDeviceMemory memory, const void* allocator);
typedef OmniVulkanResult (*omni_vulkan_bind_buffer_memory_fn)(
    OmniVulkanDevice device,
    OmniVulkanBuffer buffer,
    OmniVulkanDeviceMemory memory,
    OmniVulkanDeviceSize memory_offset
);
typedef OmniVulkanResult (*omni_vulkan_map_memory_fn)(
    OmniVulkanDevice device,
    OmniVulkanDeviceMemory memory,
    OmniVulkanDeviceSize offset,
    OmniVulkanDeviceSize size,
    OmniVulkanFlags flags,
    void** data
);
typedef void (*omni_vulkan_unmap_memory_fn)(OmniVulkanDevice device, OmniVulkanDeviceMemory memory);
typedef void (*omni_vulkan_get_device_queue_fn)(
    OmniVulkanDevice device,
    uint32_t queue_family_index,
    uint32_t queue_index,
    OmniVulkanQueue* queue
);
typedef OmniVulkanResult (*omni_vulkan_create_shader_module_fn)(
    OmniVulkanDevice device,
    const OmniVulkanShaderModuleCreateInfo* create_info,
    const void* allocator,
    OmniVulkanShaderModule* shader_module
);
typedef void (*omni_vulkan_destroy_shader_module_fn)(
    OmniVulkanDevice device,
    OmniVulkanShaderModule shader_module,
    const void* allocator
);
typedef OmniVulkanResult (*omni_vulkan_create_descriptor_set_layout_fn)(
    OmniVulkanDevice device,
    const OmniVulkanDescriptorSetLayoutCreateInfo* create_info,
    const void* allocator,
    OmniVulkanDescriptorSetLayout* set_layout
);
typedef void (*omni_vulkan_destroy_descriptor_set_layout_fn)(
    OmniVulkanDevice device,
    OmniVulkanDescriptorSetLayout set_layout,
    const void* allocator
);
typedef OmniVulkanResult (*omni_vulkan_create_descriptor_pool_fn)(
    OmniVulkanDevice device,
    const OmniVulkanDescriptorPoolCreateInfo* create_info,
    const void* allocator,
    OmniVulkanDescriptorPool* descriptor_pool
);
typedef void (*omni_vulkan_destroy_descriptor_pool_fn)(
    OmniVulkanDevice device,
    OmniVulkanDescriptorPool descriptor_pool,
    const void* allocator
);
typedef OmniVulkanResult (*omni_vulkan_allocate_descriptor_sets_fn)(
    OmniVulkanDevice device,
    const OmniVulkanDescriptorSetAllocateInfo* allocate_info,
    OmniVulkanDescriptorSet* descriptor_sets
);
typedef void (*omni_vulkan_update_descriptor_sets_fn)(
    OmniVulkanDevice device,
    uint32_t descriptor_write_count,
    const OmniVulkanWriteDescriptorSet* descriptor_writes,
    uint32_t descriptor_copy_count,
    const void* descriptor_copies
);
typedef OmniVulkanResult (*omni_vulkan_create_pipeline_layout_fn)(
    OmniVulkanDevice device,
    const OmniVulkanPipelineLayoutCreateInfo* create_info,
    const void* allocator,
    OmniVulkanPipelineLayout* pipeline_layout
);
typedef void (*omni_vulkan_destroy_pipeline_layout_fn)(
    OmniVulkanDevice device,
    OmniVulkanPipelineLayout pipeline_layout,
    const void* allocator
);
typedef OmniVulkanResult (*omni_vulkan_create_compute_pipelines_fn)(
    OmniVulkanDevice device,
    OmniVulkanPipelineCache pipeline_cache,
    uint32_t create_info_count,
    const OmniVulkanComputePipelineCreateInfo* create_infos,
    const void* allocator,
    OmniVulkanPipeline* pipelines
);
typedef void (*omni_vulkan_destroy_pipeline_fn)(
    OmniVulkanDevice device,
    OmniVulkanPipeline pipeline,
    const void* allocator
);
typedef OmniVulkanResult (*omni_vulkan_create_command_pool_fn)(
    OmniVulkanDevice device,
    const OmniVulkanCommandPoolCreateInfo* create_info,
    const void* allocator,
    OmniVulkanCommandPool* command_pool
);
typedef void (*omni_vulkan_destroy_command_pool_fn)(
    OmniVulkanDevice device,
    OmniVulkanCommandPool command_pool,
    const void* allocator
);
typedef OmniVulkanResult (*omni_vulkan_allocate_command_buffers_fn)(
    OmniVulkanDevice device,
    const OmniVulkanCommandBufferAllocateInfo* allocate_info,
    OmniVulkanCommandBuffer* command_buffers
);
typedef OmniVulkanResult (*omni_vulkan_begin_command_buffer_fn)(
    OmniVulkanCommandBuffer command_buffer,
    const OmniVulkanCommandBufferBeginInfo* begin_info
);
typedef OmniVulkanResult (*omni_vulkan_end_command_buffer_fn)(OmniVulkanCommandBuffer command_buffer);
typedef void (*omni_vulkan_cmd_bind_pipeline_fn)(
    OmniVulkanCommandBuffer command_buffer,
    uint32_t pipeline_bind_point,
    OmniVulkanPipeline pipeline
);
typedef void (*omni_vulkan_cmd_bind_descriptor_sets_fn)(
    OmniVulkanCommandBuffer command_buffer,
    uint32_t pipeline_bind_point,
    OmniVulkanPipelineLayout layout,
    uint32_t first_set,
    uint32_t descriptor_set_count,
    const OmniVulkanDescriptorSet* descriptor_sets,
    uint32_t dynamic_offset_count,
    const uint32_t* dynamic_offsets
);
typedef void (*omni_vulkan_cmd_push_constants_fn)(
    OmniVulkanCommandBuffer command_buffer,
    OmniVulkanPipelineLayout layout,
    OmniVulkanFlags stage_flags,
    uint32_t offset,
    uint32_t size,
    const void* values
);
typedef void (*omni_vulkan_cmd_dispatch_fn)(
    OmniVulkanCommandBuffer command_buffer,
    uint32_t group_count_x,
    uint32_t group_count_y,
    uint32_t group_count_z
);
typedef void (*omni_vulkan_cmd_pipeline_barrier_fn)(
    OmniVulkanCommandBuffer command_buffer,
    OmniVulkanFlags src_stage_mask,
    OmniVulkanFlags dst_stage_mask,
    OmniVulkanFlags dependency_flags,
    uint32_t memory_barrier_count,
    const OmniVulkanMemoryBarrier* memory_barriers,
    uint32_t buffer_memory_barrier_count,
    const OmniVulkanBufferMemoryBarrier* buffer_memory_barriers,
    uint32_t image_memory_barrier_count,
    const void* image_memory_barriers
);
typedef OmniVulkanResult (*omni_vulkan_queue_submit_fn)(
    OmniVulkanQueue queue,
    uint32_t submit_count,
    const OmniVulkanSubmitInfo* submits,
    OmniVulkanFence fence
);
typedef OmniVulkanResult (*omni_vulkan_queue_wait_idle_fn)(OmniVulkanQueue queue);

static void* omni_tensor_vulkan_handle = NULL;
static omni_vulkan_create_instance_fn omni_vulkan_create_instance = NULL;
static omni_vulkan_destroy_instance_fn omni_vulkan_destroy_instance = NULL;
static omni_vulkan_enumerate_physical_devices_fn omni_vulkan_enumerate_physical_devices = NULL;
static omni_vulkan_get_queue_family_properties_fn omni_vulkan_get_queue_family_properties = NULL;
static omni_vulkan_get_physical_device_features_fn omni_vulkan_get_physical_device_features = NULL;
static omni_vulkan_get_physical_device_memory_properties_fn omni_vulkan_get_physical_device_memory_properties = NULL;
static omni_vulkan_create_device_fn omni_vulkan_create_device = NULL;
static omni_vulkan_destroy_device_fn omni_vulkan_destroy_device = NULL;
static omni_vulkan_create_buffer_fn omni_vulkan_create_buffer = NULL;
static omni_vulkan_destroy_buffer_fn omni_vulkan_destroy_buffer = NULL;
static omni_vulkan_get_buffer_memory_requirements_fn omni_vulkan_get_buffer_memory_requirements = NULL;
static omni_vulkan_allocate_memory_fn omni_vulkan_allocate_memory = NULL;
static omni_vulkan_free_memory_fn omni_vulkan_free_memory = NULL;
static omni_vulkan_bind_buffer_memory_fn omni_vulkan_bind_buffer_memory = NULL;
static omni_vulkan_map_memory_fn omni_vulkan_map_memory = NULL;
static omni_vulkan_unmap_memory_fn omni_vulkan_unmap_memory = NULL;
static omni_vulkan_get_device_queue_fn omni_vulkan_get_device_queue = NULL;
static omni_vulkan_create_shader_module_fn omni_vulkan_create_shader_module = NULL;
static omni_vulkan_destroy_shader_module_fn omni_vulkan_destroy_shader_module = NULL;
static omni_vulkan_create_descriptor_set_layout_fn omni_vulkan_create_descriptor_set_layout = NULL;
static omni_vulkan_destroy_descriptor_set_layout_fn omni_vulkan_destroy_descriptor_set_layout = NULL;
static omni_vulkan_create_descriptor_pool_fn omni_vulkan_create_descriptor_pool = NULL;
static omni_vulkan_destroy_descriptor_pool_fn omni_vulkan_destroy_descriptor_pool = NULL;
static omni_vulkan_allocate_descriptor_sets_fn omni_vulkan_allocate_descriptor_sets = NULL;
static omni_vulkan_update_descriptor_sets_fn omni_vulkan_update_descriptor_sets = NULL;
static omni_vulkan_create_pipeline_layout_fn omni_vulkan_create_pipeline_layout = NULL;
static omni_vulkan_destroy_pipeline_layout_fn omni_vulkan_destroy_pipeline_layout = NULL;
static omni_vulkan_create_compute_pipelines_fn omni_vulkan_create_compute_pipelines = NULL;
static omni_vulkan_destroy_pipeline_fn omni_vulkan_destroy_pipeline = NULL;
static omni_vulkan_create_command_pool_fn omni_vulkan_create_command_pool = NULL;
static omni_vulkan_destroy_command_pool_fn omni_vulkan_destroy_command_pool = NULL;
static omni_vulkan_allocate_command_buffers_fn omni_vulkan_allocate_command_buffers = NULL;
static omni_vulkan_begin_command_buffer_fn omni_vulkan_begin_command_buffer = NULL;
static omni_vulkan_end_command_buffer_fn omni_vulkan_end_command_buffer = NULL;
static omni_vulkan_cmd_bind_pipeline_fn omni_vulkan_cmd_bind_pipeline = NULL;
static omni_vulkan_cmd_bind_descriptor_sets_fn omni_vulkan_cmd_bind_descriptor_sets = NULL;
static omni_vulkan_cmd_push_constants_fn omni_vulkan_cmd_push_constants = NULL;
static omni_vulkan_cmd_dispatch_fn omni_vulkan_cmd_dispatch = NULL;
static omni_vulkan_cmd_pipeline_barrier_fn omni_vulkan_cmd_pipeline_barrier = NULL;
static omni_vulkan_queue_submit_fn omni_vulkan_queue_submit = NULL;
static omni_vulkan_queue_wait_idle_fn omni_vulkan_queue_wait_idle = NULL;
static int omni_tensor_vulkan_resolution_attempted = 0;
static int omni_tensor_vulkan_probe_attempted = 0;
static int omni_tensor_vulkan_available_cached = 0;
static int omni_tensor_vulkan_float64_cached = 0;
static int omni_tensor_vulkan_int64_cached = 0;
// Process-lifetime backend context so independently placed buffers can bind
// together in binary Vulkan kernels.
static OmniTensorVulkanContext* omni_tensor_vulkan_shared_context = NULL;

static int omni_tensor_vulkan_resolve(void);

static int omni_tensor_vulkan_silence_stderr_begin(int* saved_stderr) {
    if (saved_stderr == NULL) return 0;
    *saved_stderr = -1;

    int devnull = open("/dev/null", O_WRONLY);
    if (devnull < 0) return 0;

    int saved = dup(STDERR_FILENO);
    if (saved < 0) {
        close(devnull);
        return 0;
    }

    if (dup2(devnull, STDERR_FILENO) < 0) {
        close(saved);
        close(devnull);
        return 0;
    }

    close(devnull);
    *saved_stderr = saved;
    return 1;
}

static void omni_tensor_vulkan_silence_stderr_end(int saved_stderr) {
    if (saved_stderr < 0) return;
    dup2(saved_stderr, STDERR_FILENO);
    close(saved_stderr);
}

static int omni_tensor_vulkan_create_instance_silent(OmniVulkanInstance* out_instance) {
    if (out_instance == NULL) return 0;
    *out_instance = NULL;
    if (!omni_tensor_vulkan_resolve()) return 0;

    OmniVulkanApplicationInfo app_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_APPLICATION_INFO,
        NULL,
        "Omni",
        1,
        "Omni",
        1,
        OMNI_VULKAN_API_VERSION_1_0
    };
    OmniVulkanInstanceCreateInfo create_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
        NULL,
        0,
        &app_info,
        0,
        NULL,
        0,
        NULL
    };

    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult result = omni_vulkan_create_instance(&create_info, NULL, out_instance);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    return result == OMNI_VULKAN_SUCCESS && *out_instance != NULL;
}

static int omni_tensor_vulkan_select_physical_device(
    OmniVulkanInstance instance,
    OmniVulkanPhysicalDevice* out_physical_device,
    uint32_t* out_queue_family_index
) {
    if (instance == NULL || out_physical_device == NULL || out_queue_family_index == NULL) return 0;
    *out_physical_device = NULL;
    *out_queue_family_index = 0;

    uint32_t device_count = 0;
    if (omni_vulkan_enumerate_physical_devices(instance, &device_count, NULL) != OMNI_VULKAN_SUCCESS || device_count == 0) return 0;

    OmniVulkanPhysicalDevice* devices = (OmniVulkanPhysicalDevice*)calloc(device_count, sizeof(OmniVulkanPhysicalDevice));
    if (devices == NULL) return 0;
    if (omni_vulkan_enumerate_physical_devices(instance, &device_count, devices) != OMNI_VULKAN_SUCCESS) {
        free(devices);
        return 0;
    }

    int found = 0;
    for (uint32_t i = 0; i < device_count && !found; i++) {
        uint32_t queue_count = 0;
        omni_vulkan_get_queue_family_properties(devices[i], &queue_count, NULL);
        if (queue_count == 0) continue;

        OmniVulkanQueueFamilyProperties* queues = (OmniVulkanQueueFamilyProperties*)calloc(queue_count, sizeof(OmniVulkanQueueFamilyProperties));
        if (queues == NULL) continue;
        omni_vulkan_get_queue_family_properties(devices[i], &queue_count, queues);

        for (uint32_t q = 0; q < queue_count; q++) {
            if ((queues[q].queueFlags & OMNI_VULKAN_QUEUE_COMPUTE_BIT) != 0 && queues[q].queueCount > 0) {
                *out_physical_device = devices[i];
                *out_queue_family_index = q;
                found = 1;
                break;
            }
        }
        free(queues);
    }

    free(devices);
    return found;
}

static int omni_tensor_vulkan_find_host_visible_memory_type(
    OmniVulkanPhysicalDevice physical_device,
    uint32_t memory_type_bits,
    uint32_t* out_memory_type_index
) {
    if (physical_device == NULL || out_memory_type_index == NULL) return 0;
    *out_memory_type_index = 0;

    OmniVulkanPhysicalDeviceMemoryProperties props;
    memset(&props, 0, sizeof(props));
    omni_vulkan_get_physical_device_memory_properties(physical_device, &props);

    for (uint32_t i = 0; i < props.memoryTypeCount && i < 32; i++) {
        uint32_t bit = 1u << i;
        uint32_t required = OMNI_VULKAN_MEMORY_PROPERTY_HOST_VISIBLE_BIT | OMNI_VULKAN_MEMORY_PROPERTY_HOST_COHERENT_BIT;
        if ((memory_type_bits & bit) != 0 && (props.memoryTypes[i].propertyFlags & required) == required) {
            *out_memory_type_index = i;
            return 1;
        }
    }

    return 0;
}

static int omni_tensor_vulkan_resolve(void) {
    if (omni_vulkan_create_instance != NULL &&
        omni_vulkan_destroy_instance != NULL &&
        omni_vulkan_enumerate_physical_devices != NULL &&
        omni_vulkan_get_queue_family_properties != NULL &&
        omni_vulkan_get_physical_device_features != NULL &&
        omni_vulkan_get_physical_device_memory_properties != NULL &&
        omni_vulkan_create_device != NULL &&
        omni_vulkan_destroy_device != NULL &&
        omni_vulkan_create_buffer != NULL &&
        omni_vulkan_destroy_buffer != NULL &&
        omni_vulkan_get_buffer_memory_requirements != NULL &&
        omni_vulkan_allocate_memory != NULL &&
        omni_vulkan_free_memory != NULL &&
        omni_vulkan_bind_buffer_memory != NULL &&
        omni_vulkan_map_memory != NULL &&
        omni_vulkan_unmap_memory != NULL &&
        omni_vulkan_get_device_queue != NULL &&
        omni_vulkan_create_shader_module != NULL &&
        omni_vulkan_destroy_shader_module != NULL &&
        omni_vulkan_create_descriptor_set_layout != NULL &&
        omni_vulkan_destroy_descriptor_set_layout != NULL &&
        omni_vulkan_create_descriptor_pool != NULL &&
        omni_vulkan_destroy_descriptor_pool != NULL &&
        omni_vulkan_allocate_descriptor_sets != NULL &&
        omni_vulkan_update_descriptor_sets != NULL &&
        omni_vulkan_create_pipeline_layout != NULL &&
        omni_vulkan_destroy_pipeline_layout != NULL &&
        omni_vulkan_create_compute_pipelines != NULL &&
        omni_vulkan_destroy_pipeline != NULL &&
        omni_vulkan_create_command_pool != NULL &&
        omni_vulkan_destroy_command_pool != NULL &&
        omni_vulkan_allocate_command_buffers != NULL &&
        omni_vulkan_begin_command_buffer != NULL &&
        omni_vulkan_end_command_buffer != NULL &&
        omni_vulkan_cmd_bind_pipeline != NULL &&
        omni_vulkan_cmd_bind_descriptor_sets != NULL &&
        omni_vulkan_cmd_push_constants != NULL &&
        omni_vulkan_cmd_dispatch != NULL &&
        omni_vulkan_cmd_pipeline_barrier != NULL &&
        omni_vulkan_queue_submit != NULL &&
        omni_vulkan_queue_wait_idle != NULL) {
        return 1;
    }
    if (omni_tensor_vulkan_resolution_attempted) return 0;
    omni_tensor_vulkan_resolution_attempted = 1;

    const char* candidates[] = {
        "libvulkan.so.1",
        "libvulkan.so",
        NULL
    };

    for (int i = 0; candidates[i] != NULL; i++) {
        void* handle = dlopen(candidates[i], RTLD_LAZY | RTLD_LOCAL);
        if (handle == NULL) continue;

        void* create_instance_symbol = dlsym(handle, "vkCreateInstance");
        void* destroy_instance_symbol = dlsym(handle, "vkDestroyInstance");
        void* enumerate_physical_devices_symbol = dlsym(handle, "vkEnumeratePhysicalDevices");
        void* get_queue_family_properties_symbol = dlsym(handle, "vkGetPhysicalDeviceQueueFamilyProperties");
        void* get_physical_device_features_symbol = dlsym(handle, "vkGetPhysicalDeviceFeatures");
        void* get_physical_device_memory_properties_symbol = dlsym(handle, "vkGetPhysicalDeviceMemoryProperties");
        void* create_device_symbol = dlsym(handle, "vkCreateDevice");
        void* destroy_device_symbol = dlsym(handle, "vkDestroyDevice");
        void* create_buffer_symbol = dlsym(handle, "vkCreateBuffer");
        void* destroy_buffer_symbol = dlsym(handle, "vkDestroyBuffer");
        void* get_buffer_memory_requirements_symbol = dlsym(handle, "vkGetBufferMemoryRequirements");
        void* allocate_memory_symbol = dlsym(handle, "vkAllocateMemory");
        void* free_memory_symbol = dlsym(handle, "vkFreeMemory");
        void* bind_buffer_memory_symbol = dlsym(handle, "vkBindBufferMemory");
        void* map_memory_symbol = dlsym(handle, "vkMapMemory");
        void* unmap_memory_symbol = dlsym(handle, "vkUnmapMemory");
        void* get_device_queue_symbol = dlsym(handle, "vkGetDeviceQueue");
        void* create_shader_module_symbol = dlsym(handle, "vkCreateShaderModule");
        void* destroy_shader_module_symbol = dlsym(handle, "vkDestroyShaderModule");
        void* create_descriptor_set_layout_symbol = dlsym(handle, "vkCreateDescriptorSetLayout");
        void* destroy_descriptor_set_layout_symbol = dlsym(handle, "vkDestroyDescriptorSetLayout");
        void* create_descriptor_pool_symbol = dlsym(handle, "vkCreateDescriptorPool");
        void* destroy_descriptor_pool_symbol = dlsym(handle, "vkDestroyDescriptorPool");
        void* allocate_descriptor_sets_symbol = dlsym(handle, "vkAllocateDescriptorSets");
        void* update_descriptor_sets_symbol = dlsym(handle, "vkUpdateDescriptorSets");
        void* create_pipeline_layout_symbol = dlsym(handle, "vkCreatePipelineLayout");
        void* destroy_pipeline_layout_symbol = dlsym(handle, "vkDestroyPipelineLayout");
        void* create_compute_pipelines_symbol = dlsym(handle, "vkCreateComputePipelines");
        void* destroy_pipeline_symbol = dlsym(handle, "vkDestroyPipeline");
        void* create_command_pool_symbol = dlsym(handle, "vkCreateCommandPool");
        void* destroy_command_pool_symbol = dlsym(handle, "vkDestroyCommandPool");
        void* allocate_command_buffers_symbol = dlsym(handle, "vkAllocateCommandBuffers");
        void* begin_command_buffer_symbol = dlsym(handle, "vkBeginCommandBuffer");
        void* end_command_buffer_symbol = dlsym(handle, "vkEndCommandBuffer");
        void* cmd_bind_pipeline_symbol = dlsym(handle, "vkCmdBindPipeline");
        void* cmd_bind_descriptor_sets_symbol = dlsym(handle, "vkCmdBindDescriptorSets");
        void* cmd_push_constants_symbol = dlsym(handle, "vkCmdPushConstants");
        void* cmd_dispatch_symbol = dlsym(handle, "vkCmdDispatch");
        void* cmd_pipeline_barrier_symbol = dlsym(handle, "vkCmdPipelineBarrier");
        void* queue_submit_symbol = dlsym(handle, "vkQueueSubmit");
        void* queue_wait_idle_symbol = dlsym(handle, "vkQueueWaitIdle");
        if (create_instance_symbol != NULL &&
            destroy_instance_symbol != NULL &&
            enumerate_physical_devices_symbol != NULL &&
            get_queue_family_properties_symbol != NULL &&
            get_physical_device_features_symbol != NULL &&
            get_physical_device_memory_properties_symbol != NULL &&
            create_device_symbol != NULL &&
            destroy_device_symbol != NULL &&
            create_buffer_symbol != NULL &&
            destroy_buffer_symbol != NULL &&
            get_buffer_memory_requirements_symbol != NULL &&
            allocate_memory_symbol != NULL &&
            free_memory_symbol != NULL &&
            bind_buffer_memory_symbol != NULL &&
            map_memory_symbol != NULL &&
            unmap_memory_symbol != NULL &&
            get_device_queue_symbol != NULL &&
            create_shader_module_symbol != NULL &&
            destroy_shader_module_symbol != NULL &&
            create_descriptor_set_layout_symbol != NULL &&
            destroy_descriptor_set_layout_symbol != NULL &&
            create_descriptor_pool_symbol != NULL &&
            destroy_descriptor_pool_symbol != NULL &&
            allocate_descriptor_sets_symbol != NULL &&
            update_descriptor_sets_symbol != NULL &&
            create_pipeline_layout_symbol != NULL &&
            destroy_pipeline_layout_symbol != NULL &&
            create_compute_pipelines_symbol != NULL &&
            destroy_pipeline_symbol != NULL &&
            create_command_pool_symbol != NULL &&
            destroy_command_pool_symbol != NULL &&
            allocate_command_buffers_symbol != NULL &&
            begin_command_buffer_symbol != NULL &&
            end_command_buffer_symbol != NULL &&
            cmd_bind_pipeline_symbol != NULL &&
            cmd_bind_descriptor_sets_symbol != NULL &&
            cmd_push_constants_symbol != NULL &&
            cmd_dispatch_symbol != NULL &&
            cmd_pipeline_barrier_symbol != NULL &&
            queue_submit_symbol != NULL &&
            queue_wait_idle_symbol != NULL) {
            omni_tensor_vulkan_handle = handle;
            omni_vulkan_create_instance = (omni_vulkan_create_instance_fn)create_instance_symbol;
            omni_vulkan_destroy_instance = (omni_vulkan_destroy_instance_fn)destroy_instance_symbol;
            omni_vulkan_enumerate_physical_devices = (omni_vulkan_enumerate_physical_devices_fn)enumerate_physical_devices_symbol;
            omni_vulkan_get_queue_family_properties = (omni_vulkan_get_queue_family_properties_fn)get_queue_family_properties_symbol;
            omni_vulkan_get_physical_device_features = (omni_vulkan_get_physical_device_features_fn)get_physical_device_features_symbol;
            omni_vulkan_get_physical_device_memory_properties = (omni_vulkan_get_physical_device_memory_properties_fn)get_physical_device_memory_properties_symbol;
            omni_vulkan_create_device = (omni_vulkan_create_device_fn)create_device_symbol;
            omni_vulkan_destroy_device = (omni_vulkan_destroy_device_fn)destroy_device_symbol;
            omni_vulkan_create_buffer = (omni_vulkan_create_buffer_fn)create_buffer_symbol;
            omni_vulkan_destroy_buffer = (omni_vulkan_destroy_buffer_fn)destroy_buffer_symbol;
            omni_vulkan_get_buffer_memory_requirements = (omni_vulkan_get_buffer_memory_requirements_fn)get_buffer_memory_requirements_symbol;
            omni_vulkan_allocate_memory = (omni_vulkan_allocate_memory_fn)allocate_memory_symbol;
            omni_vulkan_free_memory = (omni_vulkan_free_memory_fn)free_memory_symbol;
            omni_vulkan_bind_buffer_memory = (omni_vulkan_bind_buffer_memory_fn)bind_buffer_memory_symbol;
            omni_vulkan_map_memory = (omni_vulkan_map_memory_fn)map_memory_symbol;
            omni_vulkan_unmap_memory = (omni_vulkan_unmap_memory_fn)unmap_memory_symbol;
            omni_vulkan_get_device_queue = (omni_vulkan_get_device_queue_fn)get_device_queue_symbol;
            omni_vulkan_create_shader_module = (omni_vulkan_create_shader_module_fn)create_shader_module_symbol;
            omni_vulkan_destroy_shader_module = (omni_vulkan_destroy_shader_module_fn)destroy_shader_module_symbol;
            omni_vulkan_create_descriptor_set_layout = (omni_vulkan_create_descriptor_set_layout_fn)create_descriptor_set_layout_symbol;
            omni_vulkan_destroy_descriptor_set_layout = (omni_vulkan_destroy_descriptor_set_layout_fn)destroy_descriptor_set_layout_symbol;
            omni_vulkan_create_descriptor_pool = (omni_vulkan_create_descriptor_pool_fn)create_descriptor_pool_symbol;
            omni_vulkan_destroy_descriptor_pool = (omni_vulkan_destroy_descriptor_pool_fn)destroy_descriptor_pool_symbol;
            omni_vulkan_allocate_descriptor_sets = (omni_vulkan_allocate_descriptor_sets_fn)allocate_descriptor_sets_symbol;
            omni_vulkan_update_descriptor_sets = (omni_vulkan_update_descriptor_sets_fn)update_descriptor_sets_symbol;
            omni_vulkan_create_pipeline_layout = (omni_vulkan_create_pipeline_layout_fn)create_pipeline_layout_symbol;
            omni_vulkan_destroy_pipeline_layout = (omni_vulkan_destroy_pipeline_layout_fn)destroy_pipeline_layout_symbol;
            omni_vulkan_create_compute_pipelines = (omni_vulkan_create_compute_pipelines_fn)create_compute_pipelines_symbol;
            omni_vulkan_destroy_pipeline = (omni_vulkan_destroy_pipeline_fn)destroy_pipeline_symbol;
            omni_vulkan_create_command_pool = (omni_vulkan_create_command_pool_fn)create_command_pool_symbol;
            omni_vulkan_destroy_command_pool = (omni_vulkan_destroy_command_pool_fn)destroy_command_pool_symbol;
            omni_vulkan_allocate_command_buffers = (omni_vulkan_allocate_command_buffers_fn)allocate_command_buffers_symbol;
            omni_vulkan_begin_command_buffer = (omni_vulkan_begin_command_buffer_fn)begin_command_buffer_symbol;
            omni_vulkan_end_command_buffer = (omni_vulkan_end_command_buffer_fn)end_command_buffer_symbol;
            omni_vulkan_cmd_bind_pipeline = (omni_vulkan_cmd_bind_pipeline_fn)cmd_bind_pipeline_symbol;
            omni_vulkan_cmd_bind_descriptor_sets = (omni_vulkan_cmd_bind_descriptor_sets_fn)cmd_bind_descriptor_sets_symbol;
            omni_vulkan_cmd_push_constants = (omni_vulkan_cmd_push_constants_fn)cmd_push_constants_symbol;
            omni_vulkan_cmd_dispatch = (omni_vulkan_cmd_dispatch_fn)cmd_dispatch_symbol;
            omni_vulkan_cmd_pipeline_barrier = (omni_vulkan_cmd_pipeline_barrier_fn)cmd_pipeline_barrier_symbol;
            omni_vulkan_queue_submit = (omni_vulkan_queue_submit_fn)queue_submit_symbol;
            omni_vulkan_queue_wait_idle = (omni_vulkan_queue_wait_idle_fn)queue_wait_idle_symbol;
            return 1;
        }

        dlclose(handle);
    }

    return 0;
}

static void omni_tensor_vulkan_probe(void) {
    if (omni_tensor_vulkan_probe_attempted) return;
    omni_tensor_vulkan_probe_attempted = 1;
    omni_tensor_vulkan_available_cached = 0;
    omni_tensor_vulkan_float64_cached = 0;
    omni_tensor_vulkan_int64_cached = 0;
    omni_tensor_vulkan_int64_cached = 0;

    OmniVulkanInstance instance = NULL;
    if (!omni_tensor_vulkan_create_instance_silent(&instance)) return;

    OmniVulkanPhysicalDevice physical_device = NULL;
    uint32_t queue_family_index = 0;
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    int selected = omni_tensor_vulkan_select_physical_device(instance, &physical_device, &queue_family_index);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (selected) {
        (void)queue_family_index;
        omni_tensor_vulkan_available_cached = 1;
        OmniVulkanPhysicalDeviceFeatures features = {0};
        omni_vulkan_get_physical_device_features(physical_device, &features);
        if (features.shaderFloat64 != 0) omni_tensor_vulkan_float64_cached = 1;
        if (features.shaderInt64 != 0) omni_tensor_vulkan_int64_cached = 1;
    }

    omni_vulkan_destroy_instance(instance, NULL);
}

int omni_tensor_backend_vulkan_available(void) {
    omni_tensor_vulkan_probe();
    return omni_tensor_vulkan_available_cached;
}

int omni_tensor_backend_vulkan_float64_available(void) {
    omni_tensor_vulkan_probe();
    return omni_tensor_vulkan_float64_cached;
}

int omni_tensor_backend_vulkan_int64_available(void) {
    omni_tensor_vulkan_probe();
    return omni_tensor_vulkan_int64_cached;
}

int omni_tensor_backend_vulkan_float32_available(void) {
    return omni_tensor_backend_vulkan_available();
}

int omni_tensor_backend_vulkan_rounding_i64_available(void) {
    return omni_tensor_backend_vulkan_int64_available();
}

static void omni_tensor_backend_vulkan_context_retain(OmniTensorVulkanContext* context) {
    if (context == NULL) return;
    if (context->ref_count < UINT32_MAX) context->ref_count++;
}

static void omni_tensor_backend_vulkan_context_release(OmniTensorVulkanContext* context) {
    if (context == NULL) return;
    if (context->ref_count > 1) {
        context->ref_count--;
        return;
    }
    if (context->device != NULL) omni_vulkan_destroy_device(context->device, NULL);
    if (context->instance != NULL) omni_vulkan_destroy_instance(context->instance, NULL);
    free(context);
}

static void omni_tensor_backend_vulkan_destroy_buffer_handle(OmniTensorVulkanBuffer* handle) {
    if (handle == NULL) return;
    if (handle->ref_count > 1) {
        handle->ref_count--;
        return;
    }
    OmniTensorVulkanContext* context = handle->context;
    if (context != NULL && context->device != NULL) {
        if (handle->buffer != NULL) omni_vulkan_destroy_buffer(context->device, handle->buffer, NULL);
        if (handle->memory != NULL) omni_vulkan_free_memory(context->device, handle->memory, NULL);
    }
    free(handle);
    omni_tensor_backend_vulkan_context_release(context);
}

static int omni_tensor_backend_vulkan_create_context(OmniTensorVulkanContext** out_context) {
    if (out_context == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_context = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;

    OmniTensorVulkanContext* context = (OmniTensorVulkanContext*)calloc(1, sizeof(OmniTensorVulkanContext));
    if (context == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    context->ref_count = 1;

    if (!omni_tensor_vulkan_create_instance_silent(&context->instance)) {
        omni_tensor_backend_vulkan_context_release(context);
        return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    }

    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    int selected = omni_tensor_vulkan_select_physical_device(context->instance, &context->physical_device, &context->queue_family_index);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (!selected) {
        omni_tensor_backend_vulkan_context_release(context);
        return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    }

    OmniVulkanPhysicalDeviceFeatures available_features = {0};
    omni_vulkan_get_physical_device_features(context->physical_device, &available_features);
    OmniVulkanPhysicalDeviceFeatures enabled_features = {0};
    const OmniVulkanPhysicalDeviceFeatures* enabled_feature_ptr = NULL;
    if (available_features.shaderFloat64 != 0) {
        enabled_features.shaderFloat64 = 1;
        enabled_feature_ptr = &enabled_features;
    }
    if (available_features.shaderInt64 != 0) {
        enabled_features.shaderInt64 = 1;
        enabled_feature_ptr = &enabled_features;
    }

    float queue_priority = 1.0f;
    OmniVulkanDeviceQueueCreateInfo queue_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index,
        1,
        &queue_priority
    };
    OmniVulkanDeviceCreateInfo device_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
        NULL,
        0,
        1,
        &queue_info,
        0,
        NULL,
        0,
        NULL,
        enabled_feature_ptr
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult device_result = omni_vulkan_create_device(context->physical_device, &device_info, NULL, &context->device);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (device_result != OMNI_VULKAN_SUCCESS || context->device == NULL) {
        omni_tensor_backend_vulkan_context_release(context);
        return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    }

    *out_context = context;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_get_shared_context(OmniTensorVulkanContext** out_context) {
    if (out_context == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_context = NULL;
    if (omni_tensor_vulkan_shared_context != NULL) {
        omni_tensor_backend_vulkan_context_retain(omni_tensor_vulkan_shared_context);
        *out_context = omni_tensor_vulkan_shared_context;
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    OmniTensorVulkanContext* context = NULL;
    int status = omni_tensor_backend_vulkan_create_context(&context);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    omni_tensor_vulkan_shared_context = context;
    omni_tensor_backend_vulkan_context_retain(context);
    *out_context = context;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_create_buffer_on_context(
    OmniTensorVulkanContext* context,
    size_t byte_len,
    OmniTensorVulkanBuffer** out_handle
) {
    if (out_handle == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_handle = NULL;
    if (context == NULL || context->device == NULL || context->physical_device == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)calloc(1, sizeof(OmniTensorVulkanBuffer));
    if (handle == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    handle->context = context;
    handle->byte_len = byte_len;
    handle->ref_count = 1;

    OmniVulkanBufferCreateInfo buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
        NULL,
        0,
        (OmniVulkanDeviceSize)byte_len,
        OMNI_VULKAN_BUFFER_USAGE_STORAGE_BUFFER_BIT | OMNI_VULKAN_BUFFER_USAGE_TRANSFER_SRC_BIT | OMNI_VULKAN_BUFFER_USAGE_TRANSFER_DST_BIT,
        OMNI_VULKAN_SHARING_MODE_EXCLUSIVE,
        0,
        NULL
    };
    if (omni_vulkan_create_buffer(context->device, &buffer_info, NULL, &handle->buffer) != OMNI_VULKAN_SUCCESS || handle->buffer == NULL) {
        free(handle);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }

    OmniVulkanMemoryRequirements requirements = {0};
    omni_vulkan_get_buffer_memory_requirements(context->device, handle->buffer, &requirements);
    uint32_t memory_type_index = 0;
    if (!omni_tensor_vulkan_find_host_visible_memory_type(context->physical_device, requirements.memoryTypeBits, &memory_type_index)) {
        omni_vulkan_destroy_buffer(context->device, handle->buffer, NULL);
        free(handle);
        return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    }

    OmniVulkanMemoryAllocateInfo alloc_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
        NULL,
        requirements.size,
        memory_type_index
    };
    if (omni_vulkan_allocate_memory(context->device, &alloc_info, NULL, &handle->memory) != OMNI_VULKAN_SUCCESS || handle->memory == NULL) {
        omni_vulkan_destroy_buffer(context->device, handle->buffer, NULL);
        free(handle);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }

    if (omni_vulkan_bind_buffer_memory(context->device, handle->buffer, handle->memory, 0) != OMNI_VULKAN_SUCCESS) {
        omni_vulkan_free_memory(context->device, handle->memory, NULL);
        omni_vulkan_destroy_buffer(context->device, handle->buffer, NULL);
        free(handle);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }

    omni_tensor_backend_vulkan_context_retain(context);
    *out_handle = handle;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_create_buffer(size_t byte_len, OmniTensorVulkanBuffer** out_handle) {
    if (out_handle == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_handle = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanContext* context = NULL;
    int status = omni_tensor_backend_vulkan_get_shared_context(&context);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, out_handle);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_context_release(context);
        return status;
    }
    omni_tensor_backend_vulkan_context_release(context);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_copy_to_device(const void* host_ptr, size_t byte_len, void** out_device_ptr) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (host_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* handle = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer(byte_len, &handle);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    void* mapped = NULL;
    OmniVulkanDevice device = handle->context != NULL ? handle->context->device : NULL;
    if (device == NULL || omni_vulkan_map_memory(device, handle->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(handle);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped, host_ptr, byte_len);
    omni_vulkan_unmap_memory(device, handle->memory);

    *out_device_ptr = handle;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_copy_to_host(const void* device_ptr, size_t byte_len, void* host_ptr) {
    if (byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (device_ptr == NULL || host_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)device_ptr;
    OmniVulkanDevice device = handle->context != NULL ? handle->context->device : NULL;
    if (device == NULL || handle->memory == NULL || handle->byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, handle->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(host_ptr, mapped, byte_len);
    omni_vulkan_unmap_memory(device, handle->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_copy_to_existing_device(const void* host_ptr, size_t byte_len, void* device_ptr) {
    if (byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (host_ptr == NULL || device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)device_ptr;
    OmniVulkanDevice device = handle->context != NULL ? handle->context->device : NULL;
    if (device == NULL || handle->memory == NULL || handle->byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, handle->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped, host_ptr, byte_len);
    omni_vulkan_unmap_memory(device, handle->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_copy_device_to_existing_device(const void* source_device_ptr, size_t byte_len, void* dest_device_ptr) {
    if (byte_len == 0 || source_device_ptr == dest_device_ptr) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (source_device_ptr == NULL || dest_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* source = (OmniTensorVulkanBuffer*)source_device_ptr;
    OmniTensorVulkanBuffer* dest = (OmniTensorVulkanBuffer*)dest_device_ptr;
    OmniVulkanDevice source_device = source->context != NULL ? source->context->device : NULL;
    OmniVulkanDevice dest_device = dest->context != NULL ? dest->context->device : NULL;
    if (source_device == NULL || dest_device == NULL ||
        source->memory == NULL || dest->memory == NULL ||
        source->byte_len < byte_len || dest->byte_len < byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (source->context != dest->context) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    void* source_mapped = NULL;
    if (omni_vulkan_map_memory(source_device, source->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &source_mapped) != OMNI_VULKAN_SUCCESS || source_mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }

    void* dest_mapped = NULL;
    if (omni_vulkan_map_memory(dest_device, dest->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &dest_mapped) != OMNI_VULKAN_SUCCESS || dest_mapped == NULL) {
        omni_vulkan_unmap_memory(source_device, source->memory);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }

    memcpy(dest_mapped, source_mapped, byte_len);
    omni_vulkan_unmap_memory(dest_device, dest->memory);
    omni_vulkan_unmap_memory(source_device, source->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_fill_f64(void* device_ptr, size_t element_count, double value) {
    if (element_count == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (device_ptr == NULL || element_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_INVALID;

    size_t byte_len = element_count * sizeof(double);
    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)device_ptr;
    OmniVulkanDevice device = handle->context != NULL ? handle->context->device : NULL;
    if (device == NULL || handle->memory == NULL || handle->byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, handle->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }

    double* values = (double*)mapped;
    for (size_t i = 0; i < element_count; i++) {
        values[i] = value;
    }
    omni_vulkan_unmap_memory(device, handle->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_fill_f32(void* device_ptr, size_t element_count, float value) {
    if (element_count == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (device_ptr == NULL || element_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_INVALID;

    size_t byte_len = element_count * sizeof(float);
    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)device_ptr;
    OmniVulkanDevice device = handle->context != NULL ? handle->context->device : NULL;
    if (device == NULL || handle->memory == NULL || handle->byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;

    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, handle->memory, 0, (OmniVulkanDeviceSize)byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }

    float* values = (float*)mapped;
    for (size_t i = 0; i < element_count; i++) {
        values[i] = value;
    }
    omni_vulkan_unmap_memory(device, handle->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_copy_range_to_host(
    const void* device_ptr,
    size_t offset,
    size_t byte_len,
    void* host_ptr
) {
    if (byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (device_ptr == NULL || host_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)device_ptr;
    OmniVulkanDevice device = handle->context != NULL ? handle->context->device : NULL;
    if (device == NULL || handle->memory == NULL || offset > handle->byte_len || byte_len > handle->byte_len - offset) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    void* mapped = NULL;
    if (omni_vulkan_map_memory(device, handle->memory, 0, (OmniVulkanDeviceSize)handle->byte_len, 0, &mapped) != OMNI_VULKAN_SUCCESS || mapped == NULL) {
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(host_ptr, (const unsigned char*)mapped + offset, byte_len);
    omni_vulkan_unmap_memory(device, handle->memory);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_map_add_scalar_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    double scalar,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (element_count > UINT32_MAX || byte_len != element_count * sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return status;
    }

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[2] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        2,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(OmniTensorVulkanAddScalarPushConstants)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        omni_tensor_vulkan_map_add_scalar_f64_spv_size,
        omni_tensor_vulkan_map_add_scalar_f64_spv
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &shader_info, NULL, &shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, &pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 2 };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo input_info = { input->buffer, 0, (OmniVulkanDeviceSize)byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)byte_len };
    OmniVulkanWriteDescriptorSet writes[2] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &input_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL }
    };
    omni_vulkan_update_descriptor_sets(device, 2, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniTensorVulkanAddScalarPushConstants push = { scalar, (uint32_t)element_count, 0 };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE;
    omni_vulkan_cmd_dispatch(command_buffer, group_count, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_dense_required_elements(
    size_t rank,
    const size_t* shape,
    const size_t* strides,
    size_t* out_required
) {
    if (out_required == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_required = 0;
    if (rank == 0) {
        *out_required = 1;
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }
    if (shape == NULL || strides == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    size_t max_offset = 0;
    for (size_t i = 0; i < rank; i++) {
        if (shape[i] == 0) {
            *out_required = 0;
            return OMNI_TENSOR_VULKAN_SUCCESS;
        }
        size_t axis_extent = shape[i] - 1;
        if (axis_extent != 0 && strides[i] > SIZE_MAX / axis_extent) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        size_t axis_offset = axis_extent * strides[i];
        if (axis_offset > SIZE_MAX - max_offset) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        max_offset += axis_offset;
    }
    if (max_offset == SIZE_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    *out_required = max_offset + 1;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_write_u32_metadata(
    uint32_t* metadata,
    size_t* cursor,
    const size_t* values,
    size_t count
) {
    if (cursor == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (count == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (metadata == NULL || values == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    for (size_t i = 0; i < count; i++) {
        if (values[i] > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        metadata[*cursor + i] = (uint32_t)values[i];
    }
    *cursor += count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_map_f64(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t byte_len,
    size_t element_count,
    double scalar,
    uint32_t mode,
    uint32_t op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (element_count > UINT32_MAX || byte_len != element_count * sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (mode > 2u || op > 5u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (out_rank > UINT32_MAX || left_rank > UINT32_MAX || right_rank > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (left_rank > 0 && (left_shape == NULL || left_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (right_rank > 0 && (right_shape == NULL || right_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;

    const void* primary_device_ptr = mode == 1u ? right_device_ptr : left_device_ptr;
    if (primary_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* primary = (OmniTensorVulkanBuffer*)primary_device_ptr;
    OmniTensorVulkanBuffer* left = left_device_ptr != NULL ? (OmniTensorVulkanBuffer*)left_device_ptr : primary;
    OmniTensorVulkanBuffer* right = right_device_ptr != NULL ? (OmniTensorVulkanBuffer*)right_device_ptr : primary;
    OmniTensorVulkanContext* context = primary->context;
    if (context == NULL || context->device == NULL ||
        left->context != context || right->context != context ||
        left->buffer == NULL || right->buffer == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    size_t left_required = 0;
    size_t right_required = 0;
    int shape_status = omni_tensor_vulkan_dense_required_elements(left_rank, left_shape, left_strides, &left_required);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    shape_status = omni_tensor_vulkan_dense_required_elements(right_rank, right_shape, right_strides, &right_required);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    if (left_required > SIZE_MAX / sizeof(double) || right_required > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (left->byte_len < left_required * sizeof(double) || right->byte_len < right_required * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return status;
    }

    if (out_rank > SIZE_MAX / 2u || left_rank > SIZE_MAX / 2u || right_rank > SIZE_MAX / 2u) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words_actual = (out_rank * 2u) + (left_rank * 2u) + (right_rank * 2u);
    if (metadata_words_actual > SIZE_MAX / sizeof(uint32_t)) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words = metadata_words_actual == 0 ? 1 : metadata_words_actual;
    uint32_t* metadata = (uint32_t*)calloc(metadata_words, sizeof(uint32_t));
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    size_t metadata_cursor = 0;
    status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_shape, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_strides, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_shape, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_strides, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_shape, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_strides, right_rank);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniTensorVulkanBuffer* metadata_buffer = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, metadata_words * sizeof(uint32_t), &metadata_buffer);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }
    void* mapped_metadata = NULL;
    if (omni_vulkan_map_memory(device, metadata_buffer->memory, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)), 0, &mapped_metadata) != OMNI_VULKAN_SUCCESS || mapped_metadata == NULL) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped_metadata, metadata, metadata_words * sizeof(uint32_t));
    omni_vulkan_unmap_memory(device, metadata_buffer->memory);
    free(metadata);

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[4] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {3, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        4,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(OmniTensorVulkanMapPushConstants)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        omni_tensor_vulkan_map_f64_spv_size,
        omni_tensor_vulkan_map_f64_spv
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &shader_info, NULL, &shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, &pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 4 };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo left_info = { left->buffer, 0, (OmniVulkanDeviceSize)left->byte_len };
    OmniVulkanDescriptorBufferInfo right_info = { right->buffer, 0, (OmniVulkanDeviceSize)right->byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)byte_len };
    OmniVulkanDescriptorBufferInfo metadata_info = { metadata_buffer->buffer, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)) };
    OmniVulkanWriteDescriptorSet writes[4] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &left_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &right_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 3, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &metadata_info, NULL }
    };
    omni_vulkan_update_descriptor_sets(device, 4, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniTensorVulkanMapPushConstants push = {
        scalar,
        (uint32_t)element_count,
        op,
        mode,
        (uint32_t)out_rank,
        (uint32_t)left_rank,
        (uint32_t)right_rank,
        0
    };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE;
    omni_vulkan_cmd_dispatch(command_buffer, group_count, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (metadata_buffer != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_map_f32(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t byte_len,
    size_t element_count,
    float scalar,
    uint32_t mode,
    uint32_t op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (element_count > UINT32_MAX || byte_len != element_count * sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (mode > 2u || op > 5u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (out_rank > UINT32_MAX || left_rank > UINT32_MAX || right_rank > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (left_rank > 0 && (left_shape == NULL || left_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (right_rank > 0 && (right_shape == NULL || right_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;

    const void* primary_device_ptr = mode == 1u ? right_device_ptr : left_device_ptr;
    if (primary_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* primary = (OmniTensorVulkanBuffer*)primary_device_ptr;
    OmniTensorVulkanBuffer* left = left_device_ptr != NULL ? (OmniTensorVulkanBuffer*)left_device_ptr : primary;
    OmniTensorVulkanBuffer* right = right_device_ptr != NULL ? (OmniTensorVulkanBuffer*)right_device_ptr : primary;
    OmniTensorVulkanContext* context = primary->context;
    if (context == NULL || context->device == NULL ||
        left->context != context || right->context != context ||
        left->buffer == NULL || right->buffer == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    size_t left_required = 0;
    size_t right_required = 0;
    int shape_status = omni_tensor_vulkan_dense_required_elements(left_rank, left_shape, left_strides, &left_required);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    shape_status = omni_tensor_vulkan_dense_required_elements(right_rank, right_shape, right_strides, &right_required);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    if (left_required > SIZE_MAX / sizeof(float) || right_required > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (left->byte_len < left_required * sizeof(float) || right->byte_len < right_required * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        return status;
    }

    if (out_rank > SIZE_MAX / 2u || left_rank > SIZE_MAX / 2u || right_rank > SIZE_MAX / 2u) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words_actual = (out_rank * 2u) + (left_rank * 2u) + (right_rank * 2u);
    if (metadata_words_actual > SIZE_MAX / sizeof(uint32_t)) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words = metadata_words_actual == 0 ? 1 : metadata_words_actual;
    uint32_t* metadata = (uint32_t*)calloc(metadata_words, sizeof(uint32_t));
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    size_t metadata_cursor = 0;
    status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_shape, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_strides, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_shape, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_strides, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_shape, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_strides, right_rank);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniTensorVulkanBuffer* metadata_buffer = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, metadata_words * sizeof(uint32_t), &metadata_buffer);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }
    void* mapped_metadata = NULL;
    if (omni_vulkan_map_memory(device, metadata_buffer->memory, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)), 0, &mapped_metadata) != OMNI_VULKAN_SUCCESS || mapped_metadata == NULL) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped_metadata, metadata, metadata_words * sizeof(uint32_t));
    omni_vulkan_unmap_memory(device, metadata_buffer->memory);
    free(metadata);

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[4] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {3, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        4,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(OmniTensorVulkanMapF32PushConstants)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        omni_tensor_vulkan_map_f32_spv_size,
        omni_tensor_vulkan_map_f32_spv
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &shader_info, NULL, &shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        stage_info,
        pipeline_layout,
        NULL,
        0
    };
    if (omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, &pipeline) != OMNI_VULKAN_SUCCESS || pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = {
        OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER,
        4
    };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo alloc_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &alloc_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo buffer_infos[4] = {
        { left->buffer, 0, left->byte_len },
        { right->buffer, 0, right->byte_len },
        { output->buffer, 0, byte_len },
        { metadata_buffer->buffer, 0, metadata_words * sizeof(uint32_t) }
    };
    OmniVulkanWriteDescriptorSet writes[4];
    for (uint32_t i = 0; i < 4; i++) {
        writes[i].sType = OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET;
        writes[i].pNext = NULL;
        writes[i].dstSet = descriptor_set;
        writes[i].dstBinding = i;
        writes[i].dstArrayElement = 0;
        writes[i].descriptorCount = 1;
        writes[i].descriptorType = OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER;
        writes[i].pImageInfo = NULL;
        writes[i].pBufferInfo = &buffer_infos[i];
        writes[i].pTexelBufferView = NULL;
    }
    omni_vulkan_update_descriptor_sets(device, 4, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniTensorVulkanMapF32PushConstants push = {
        scalar,
        (uint32_t)element_count,
        op,
        mode,
        (uint32_t)out_rank,
        (uint32_t)left_rank,
        (uint32_t)right_rank,
        0
    };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE;
    omni_vulkan_cmd_dispatch(command_buffer, group_count, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (metadata_buffer != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_map_complex_launch(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    uint32_t mode,
    uint32_t op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    int requires_float64,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (requires_float64 && !omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (element_size == 0 || element_count > UINT32_MAX || element_count > SIZE_MAX / element_size ||
        byte_len != element_count * element_size || mode > 2u || op > 3u || shader_words == NULL ||
        shader_size == 0 || push_data == NULL || push_size == 0 || push_size > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (out_rank > UINT32_MAX || left_rank > UINT32_MAX || right_rank > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (left_rank > 0 && (left_shape == NULL || left_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (right_rank > 0 && (right_shape == NULL || right_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;

    const void* primary_device_ptr = mode == 1u ? right_device_ptr : left_device_ptr;
    if (primary_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* primary = (OmniTensorVulkanBuffer*)primary_device_ptr;
    OmniTensorVulkanBuffer* left = left_device_ptr != NULL ? (OmniTensorVulkanBuffer*)left_device_ptr : primary;
    OmniTensorVulkanBuffer* right = right_device_ptr != NULL ? (OmniTensorVulkanBuffer*)right_device_ptr : primary;
    OmniTensorVulkanContext* context = primary->context;
    if (context == NULL || context->device == NULL ||
        left->context != context || right->context != context ||
        left->buffer == NULL || right->buffer == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    size_t left_required = 0;
    size_t right_required = 0;
    int shape_status = omni_tensor_vulkan_dense_required_elements(left_rank, left_shape, left_strides, &left_required);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    shape_status = omni_tensor_vulkan_dense_required_elements(right_rank, right_shape, right_strides, &right_required);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    if (left_required > SIZE_MAX / element_size || right_required > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (left->byte_len < left_required * element_size || right->byte_len < right_required * element_size) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniVulkanDevice device = context->device;
    OmniTensorVulkanBuffer* output = NULL;
    int result = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) return result;

    if (out_rank > SIZE_MAX / 2u || left_rank > SIZE_MAX / 2u || right_rank > SIZE_MAX / 2u) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words_actual = (out_rank * 2u) + (left_rank * 2u) + (right_rank * 2u);
    if (metadata_words_actual > SIZE_MAX / sizeof(uint32_t)) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words = metadata_words_actual == 0 ? 1 : metadata_words_actual;
    uint32_t* metadata = (uint32_t*)calloc(metadata_words, sizeof(uint32_t));
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    size_t metadata_cursor = 0;
    result = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_shape, out_rank);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) result = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_strides, out_rank);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) result = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_shape, left_rank);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) result = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_strides, left_rank);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) result = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_shape, right_rank);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) result = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_strides, right_rank);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    OmniTensorVulkanBuffer* metadata_buffer = NULL;
    result = omni_tensor_backend_vulkan_create_buffer_on_context(context, metadata_words * sizeof(uint32_t), &metadata_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }
    result = omni_tensor_backend_vulkan_copy_to_existing_device(metadata, metadata_words * sizeof(uint32_t), metadata_buffer);
    free(metadata);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    OmniTensorVulkanBuffer* status_buffer = NULL;
    result = omni_tensor_backend_vulkan_create_buffer_on_context(context, sizeof(uint32_t), &status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }
    uint32_t status_zero = 0u;
    result = omni_tensor_backend_vulkan_copy_to_existing_device(&status_zero, sizeof(status_zero), status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[5] = {
        { left->buffer, (OmniVulkanDeviceSize)left->byte_len },
        { right->buffer, (OmniVulkanDeviceSize)right->byte_len },
        { output->buffer, (OmniVulkanDeviceSize)byte_len },
        { metadata_buffer->buffer, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)) },
        { status_buffer->buffer, (OmniVulkanDeviceSize)sizeof(uint32_t) }
    };

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 5, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)push_size
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptors.layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS ||
        pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        shader_words,
        shader_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 5, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        push_data,
        (uint32_t)push_size,
        group_count
    );

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) {
        uint32_t status_host = 0u;
        int copy_status = omni_tensor_backend_vulkan_copy_to_host(status_buffer, sizeof(status_host), &status_host);
        if (copy_status != OMNI_TENSOR_VULKAN_SUCCESS) {
            result = copy_status;
        } else if (status_host == 1u) {
            result = OMNI_TENSOR_VULKAN_DOMAIN_ERROR;
        } else if (status_host == 2u) {
            result = OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
        } else if (status_host != 0u) {
            result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
    }
    omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
    omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_map_complex128(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t byte_len,
    size_t element_count,
    double scalar_real,
    double scalar_imag,
    uint32_t mode,
    uint32_t op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    OmniTensorVulkanMapComplex128PushConstants push = {
        scalar_real,
        scalar_imag,
        (uint32_t)element_count,
        op,
        mode,
        (uint32_t)out_rank,
        (uint32_t)left_rank,
        (uint32_t)right_rank
    };
    return omni_tensor_backend_vulkan_map_complex_launch(
        left_device_ptr,
        right_device_ptr,
        byte_len,
        element_count,
        sizeof(double) * 2u,
        mode,
        op,
        out_rank,
        left_rank,
        right_rank,
        out_shape,
        out_strides,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        omni_tensor_vulkan_map_complex128_spv,
        omni_tensor_vulkan_map_complex128_spv_size,
        &push,
        sizeof(push),
        1,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_complex64(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t byte_len,
    size_t element_count,
    float scalar_real,
    float scalar_imag,
    uint32_t mode,
    uint32_t op,
    size_t out_rank,
    size_t left_rank,
    size_t right_rank,
    const size_t* out_shape,
    const size_t* out_strides,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    void** out_device_ptr
) {
    OmniTensorVulkanMapComplex64PushConstants push = {
        scalar_real,
        scalar_imag,
        (uint32_t)element_count,
        op,
        mode,
        (uint32_t)out_rank,
        (uint32_t)left_rank,
        (uint32_t)right_rank,
        0
    };
    return omni_tensor_backend_vulkan_map_complex_launch(
        left_device_ptr,
        right_device_ptr,
        byte_len,
        element_count,
        sizeof(float) * 2u,
        mode,
        op,
        out_rank,
        left_rank,
        right_rank,
        out_shape,
        out_strides,
        left_shape,
        left_strides,
        right_shape,
        right_strides,
        omni_tensor_vulkan_map_complex64_spv,
        omni_tensor_vulkan_map_complex64_spv_size,
        &push,
        sizeof(push),
        0,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_contract_dot_f64(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t element_count,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (left_device_ptr == NULL || right_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    size_t byte_len = element_count * sizeof(double);
    if (byte_len / sizeof(double) != element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)left_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL ||
        right->context != context ||
        left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < byte_len || right->byte_len < byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, sizeof(double), &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[3] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        3,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(OmniTensorVulkanDotPushConstants)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        omni_tensor_vulkan_contract_dot_f64_spv_size,
        omni_tensor_vulkan_contract_dot_f64_spv
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &shader_info, NULL, &shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, &pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 4 };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo left_info = { left->buffer, 0, (OmniVulkanDeviceSize)byte_len };
    OmniVulkanDescriptorBufferInfo right_info = { right->buffer, 0, (OmniVulkanDeviceSize)byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)sizeof(double) };
    OmniVulkanWriteDescriptorSet writes[3] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &left_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &right_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL }
    };
    omni_vulkan_update_descriptor_sets(device, 3, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniTensorVulkanDotPushConstants push = { (uint32_t)element_count };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    omni_vulkan_cmd_dispatch(command_buffer, 1, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_contract_f64(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t left_rank,
    size_t right_rank,
    size_t axis_count,
    const size_t* left_axes,
    const size_t* right_axes,
    size_t out_element_count,
    size_t out_rank,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    const size_t* out_shape,
    const size_t* out_strides,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axis_count > left_rank || axis_count > right_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axis_count > 0 && (left_axes == NULL || right_axes == NULL)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if ((left_rank > 0 && (left_shape == NULL || left_strides == NULL)) ||
        (right_rank > 0 && (right_shape == NULL || right_strides == NULL))) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (left_rank > UINT32_MAX || right_rank > UINT32_MAX ||
        axis_count > UINT32_MAX ||
        out_element_count > UINT32_MAX || out_rank > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t left_count = 0;
    size_t right_count = 0;
    int shape_status = omni_tensor_vulkan_dense_required_elements(left_rank, left_shape, left_strides, &left_count);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    shape_status = omni_tensor_vulkan_dense_required_elements(right_rank, right_shape, right_strides, &right_count);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    if (left_count > SIZE_MAX / sizeof(double) || right_count > SIZE_MAX / sizeof(double) ||
        out_element_count > SIZE_MAX / sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t contracted_count = 1;
    for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
        size_t left_axis = left_axes[axis_pos];
        size_t right_axis = right_axes[axis_pos];
        if (left_axis >= left_rank || right_axis >= right_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        for (size_t previous = 0; previous < axis_pos; previous++) {
            if (left_axes[previous] == left_axis || right_axes[previous] == right_axis) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
        if (left_shape[left_axis] != right_shape[right_axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (left_shape[left_axis] != 0 && contracted_count > SIZE_MAX / left_shape[left_axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        contracted_count *= left_shape[left_axis];
    }
    if (contracted_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t expected_rank = (left_rank - axis_count) + (right_rank - axis_count);
    if (out_rank != expected_rank) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t result_axis = 0;
    size_t expected_count = 1;
    for (size_t axis = 0; axis < left_rank; axis++) {
        int contracted_axis = 0;
        for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
            if (left_axes[axis_pos] == axis) {
                contracted_axis = 1;
                break;
            }
        }
        if (contracted_axis) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != left_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (left_shape[axis] != 0 && expected_count > SIZE_MAX / left_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= left_shape[axis];
        result_axis++;
    }
    for (size_t axis = 0; axis < right_rank; axis++) {
        int contracted_axis = 0;
        for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
            if (right_axes[axis_pos] == axis) {
                contracted_axis = 1;
                break;
            }
        }
        if (contracted_axis) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != right_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (right_shape[axis] != 0 && expected_count > SIZE_MAX / right_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= right_shape[axis];
        result_axis++;
    }
    if (expected_rank == 0) expected_count = 1;
    if (expected_count != out_element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t left_byte_len = left_count * sizeof(double);
    size_t right_byte_len = right_count * sizeof(double);
    size_t out_byte_len = out_element_count * sizeof(double);

    if (out_element_count == 0) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    if (contracted_count == 0) {
        OmniTensorVulkanBuffer* output = NULL;
        int status = omni_tensor_backend_vulkan_create_buffer(out_byte_len, &output);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
        if (output == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        OmniVulkanDevice zero_device = output->context != NULL ? output->context->device : NULL;
        if (zero_device == NULL || output->memory == NULL || output->byte_len < out_byte_len) {
            omni_tensor_backend_vulkan_destroy_buffer_handle(output);
            return OMNI_TENSOR_VULKAN_INVALID;
        }
        void* mapped_zero = NULL;
        if (omni_vulkan_map_memory(zero_device, output->memory, 0, (OmniVulkanDeviceSize)out_byte_len, 0, &mapped_zero) != OMNI_VULKAN_SUCCESS || mapped_zero == NULL) {
            omni_tensor_backend_vulkan_destroy_buffer_handle(output);
            return OMNI_TENSOR_VULKAN_COPY_FAILED;
        }
        memset(mapped_zero, 0, out_byte_len);
        omni_vulkan_unmap_memory(zero_device, output->memory);
        *out_device_ptr = output;
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    if (left_device_ptr == NULL || right_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)left_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL ||
        right->context != context ||
        left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < left_byte_len || right->byte_len < right_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, out_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    if (out_rank > SIZE_MAX / 2u || left_rank > SIZE_MAX / 2u || right_rank > SIZE_MAX / 2u || axis_count > SIZE_MAX / 2u) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words_actual = (out_rank * 2u) + (left_rank * 2u) + (right_rank * 2u) + (axis_count * 2u);
    if (metadata_words_actual > SIZE_MAX / sizeof(uint32_t)) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words = metadata_words_actual == 0 ? 1 : metadata_words_actual;
    uint32_t* metadata = (uint32_t*)calloc(metadata_words, sizeof(uint32_t));
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    size_t metadata_cursor = 0;
    status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_shape, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_strides, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_shape, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_strides, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_shape, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_strides, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_axes, axis_count);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_axes, axis_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniTensorVulkanBuffer* metadata_buffer = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, metadata_words * sizeof(uint32_t), &metadata_buffer);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }
    void* mapped_metadata = NULL;
    if (omni_vulkan_map_memory(device, metadata_buffer->memory, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)), 0, &mapped_metadata) != OMNI_VULKAN_SUCCESS || mapped_metadata == NULL) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped_metadata, metadata, metadata_words * sizeof(uint32_t));
    omni_vulkan_unmap_memory(device, metadata_buffer->memory);
    free(metadata);

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[4] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {3, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        4,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(OmniTensorVulkanContractPushConstants)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        omni_tensor_vulkan_contract_f64_spv_size,
        omni_tensor_vulkan_contract_f64_spv
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &shader_info, NULL, &shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, &pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 4 };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo left_info = { left->buffer, 0, (OmniVulkanDeviceSize)left_byte_len };
    OmniVulkanDescriptorBufferInfo right_info = { right->buffer, 0, (OmniVulkanDeviceSize)right_byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)out_byte_len };
    OmniVulkanDescriptorBufferInfo metadata_info = { metadata_buffer->buffer, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)) };
    OmniVulkanWriteDescriptorSet writes[4] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &left_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &right_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 3, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &metadata_info, NULL }
    };
    omni_vulkan_update_descriptor_sets(device, 4, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniTensorVulkanContractPushConstants push = {
        (uint32_t)left_rank,
        (uint32_t)right_rank,
        (uint32_t)axis_count,
        (uint32_t)out_element_count,
        (uint32_t)out_rank
    };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    uint32_t group_count = ((uint32_t)out_element_count + OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE;
    omni_vulkan_cmd_dispatch(command_buffer, group_count, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (metadata_buffer != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_contract_f32(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t left_rank,
    size_t right_rank,
    size_t axis_count,
    const size_t* left_axes,
    const size_t* right_axes,
    size_t out_element_count,
    size_t out_rank,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    const size_t* out_shape,
    const size_t* out_strides,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axis_count > left_rank || axis_count > right_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axis_count > 0 && (left_axes == NULL || right_axes == NULL)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if ((left_rank > 0 && (left_shape == NULL || left_strides == NULL)) ||
        (right_rank > 0 && (right_shape == NULL || right_strides == NULL))) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (left_rank > UINT32_MAX || right_rank > UINT32_MAX ||
        axis_count > UINT32_MAX ||
        out_element_count > UINT32_MAX || out_rank > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t left_count = 0;
    size_t right_count = 0;
    int shape_status = omni_tensor_vulkan_dense_required_elements(left_rank, left_shape, left_strides, &left_count);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    shape_status = omni_tensor_vulkan_dense_required_elements(right_rank, right_shape, right_strides, &right_count);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    if (left_count > SIZE_MAX / sizeof(float) || right_count > SIZE_MAX / sizeof(float) ||
        out_element_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t contracted_count = 1;
    for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
        size_t left_axis = left_axes[axis_pos];
        size_t right_axis = right_axes[axis_pos];
        if (left_axis >= left_rank || right_axis >= right_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        for (size_t previous = 0; previous < axis_pos; previous++) {
            if (left_axes[previous] == left_axis || right_axes[previous] == right_axis) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
        if (left_shape[left_axis] != right_shape[right_axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (left_shape[left_axis] != 0 && contracted_count > SIZE_MAX / left_shape[left_axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        contracted_count *= left_shape[left_axis];
    }
    if (contracted_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t expected_rank = (left_rank - axis_count) + (right_rank - axis_count);
    if (out_rank != expected_rank) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t result_axis = 0;
    size_t expected_count = 1;
    for (size_t axis = 0; axis < left_rank; axis++) {
        int contracted_axis = 0;
        for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
            if (left_axes[axis_pos] == axis) {
                contracted_axis = 1;
                break;
            }
        }
        if (contracted_axis) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != left_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (left_shape[axis] != 0 && expected_count > SIZE_MAX / left_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= left_shape[axis];
        result_axis++;
    }
    for (size_t axis = 0; axis < right_rank; axis++) {
        int contracted_axis = 0;
        for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
            if (right_axes[axis_pos] == axis) {
                contracted_axis = 1;
                break;
            }
        }
        if (contracted_axis) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != right_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (right_shape[axis] != 0 && expected_count > SIZE_MAX / right_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= right_shape[axis];
        result_axis++;
    }
    if (expected_rank == 0) expected_count = 1;
    if (expected_count != out_element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t left_byte_len = left_count * sizeof(float);
    size_t right_byte_len = right_count * sizeof(float);
    size_t out_byte_len = out_element_count * sizeof(float);

    if (out_element_count == 0) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    if (contracted_count == 0) {
        OmniTensorVulkanBuffer* output = NULL;
        int status = omni_tensor_backend_vulkan_create_buffer(out_byte_len, &output);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
        if (output == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        OmniVulkanDevice zero_device = output->context != NULL ? output->context->device : NULL;
        if (zero_device == NULL || output->memory == NULL || output->byte_len < out_byte_len) {
            omni_tensor_backend_vulkan_destroy_buffer_handle(output);
            return OMNI_TENSOR_VULKAN_INVALID;
        }
        void* mapped_zero = NULL;
        if (omni_vulkan_map_memory(zero_device, output->memory, 0, (OmniVulkanDeviceSize)out_byte_len, 0, &mapped_zero) != OMNI_VULKAN_SUCCESS || mapped_zero == NULL) {
            omni_tensor_backend_vulkan_destroy_buffer_handle(output);
            return OMNI_TENSOR_VULKAN_COPY_FAILED;
        }
        memset(mapped_zero, 0, out_byte_len);
        omni_vulkan_unmap_memory(zero_device, output->memory);
        *out_device_ptr = output;
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    if (left_device_ptr == NULL || right_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)left_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL ||
        right->context != context ||
        left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < left_byte_len || right->byte_len < right_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, out_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    if (out_rank > SIZE_MAX / 2u || left_rank > SIZE_MAX / 2u || right_rank > SIZE_MAX / 2u || axis_count > SIZE_MAX / 2u) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words_actual = (out_rank * 2u) + (left_rank * 2u) + (right_rank * 2u) + (axis_count * 2u);
    if (metadata_words_actual > SIZE_MAX / sizeof(uint32_t)) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words = metadata_words_actual == 0 ? 1 : metadata_words_actual;
    uint32_t* metadata = (uint32_t*)calloc(metadata_words, sizeof(uint32_t));
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    size_t metadata_cursor = 0;
    status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_shape, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_strides, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_shape, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_strides, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_shape, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_strides, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_axes, axis_count);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_axes, axis_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniTensorVulkanBuffer* metadata_buffer = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, metadata_words * sizeof(uint32_t), &metadata_buffer);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }
    void* mapped_metadata = NULL;
    if (omni_vulkan_map_memory(device, metadata_buffer->memory, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)), 0, &mapped_metadata) != OMNI_VULKAN_SUCCESS || mapped_metadata == NULL) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped_metadata, metadata, metadata_words * sizeof(uint32_t));
    omni_vulkan_unmap_memory(device, metadata_buffer->memory);
    free(metadata);

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[4] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {3, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        4,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(OmniTensorVulkanContractPushConstants)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        omni_tensor_vulkan_contract_f32_spv_size,
        omni_tensor_vulkan_contract_f32_spv
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &shader_info, NULL, &shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, &pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 4 };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo left_info = { left->buffer, 0, (OmniVulkanDeviceSize)left_byte_len };
    OmniVulkanDescriptorBufferInfo right_info = { right->buffer, 0, (OmniVulkanDeviceSize)right_byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)out_byte_len };
    OmniVulkanDescriptorBufferInfo metadata_info = { metadata_buffer->buffer, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)) };
    OmniVulkanWriteDescriptorSet writes[4] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &left_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &right_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 3, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &metadata_info, NULL }
    };
    omni_vulkan_update_descriptor_sets(device, 4, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniTensorVulkanContractPushConstants push = {
        (uint32_t)left_rank,
        (uint32_t)right_rank,
        (uint32_t)axis_count,
        (uint32_t)out_element_count,
        (uint32_t)out_rank
    };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    uint32_t group_count = ((uint32_t)out_element_count + OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE;
    omni_vulkan_cmd_dispatch(command_buffer, group_count, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (metadata_buffer != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_contract_complex128(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t left_rank,
    size_t right_rank,
    size_t axis_count,
    const size_t* left_axes,
    const size_t* right_axes,
    size_t out_element_count,
    size_t out_rank,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    const size_t* out_shape,
    const size_t* out_strides,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axis_count > left_rank || axis_count > right_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axis_count > 0 && (left_axes == NULL || right_axes == NULL)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if ((left_rank > 0 && (left_shape == NULL || left_strides == NULL)) ||
        (right_rank > 0 && (right_shape == NULL || right_strides == NULL))) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (left_rank > UINT32_MAX || right_rank > UINT32_MAX ||
        axis_count > UINT32_MAX ||
        out_element_count > UINT32_MAX || out_rank > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t left_count = 0;
    size_t right_count = 0;
    int shape_status = omni_tensor_vulkan_dense_required_elements(left_rank, left_shape, left_strides, &left_count);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    shape_status = omni_tensor_vulkan_dense_required_elements(right_rank, right_shape, right_strides, &right_count);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    if (left_count > SIZE_MAX / (sizeof(double) * 2u) || right_count > SIZE_MAX / (sizeof(double) * 2u) ||
        out_element_count > SIZE_MAX / (sizeof(double) * 2u)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t contracted_count = 1;
    for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
        size_t left_axis = left_axes[axis_pos];
        size_t right_axis = right_axes[axis_pos];
        if (left_axis >= left_rank || right_axis >= right_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        for (size_t previous = 0; previous < axis_pos; previous++) {
            if (left_axes[previous] == left_axis || right_axes[previous] == right_axis) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
        if (left_shape[left_axis] != right_shape[right_axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (left_shape[left_axis] != 0 && contracted_count > SIZE_MAX / left_shape[left_axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        contracted_count *= left_shape[left_axis];
    }
    if (contracted_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t expected_rank = (left_rank - axis_count) + (right_rank - axis_count);
    if (out_rank != expected_rank) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t result_axis = 0;
    size_t expected_count = 1;
    for (size_t axis = 0; axis < left_rank; axis++) {
        int contracted_axis = 0;
        for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
            if (left_axes[axis_pos] == axis) {
                contracted_axis = 1;
                break;
            }
        }
        if (contracted_axis) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != left_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (left_shape[axis] != 0 && expected_count > SIZE_MAX / left_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= left_shape[axis];
        result_axis++;
    }
    for (size_t axis = 0; axis < right_rank; axis++) {
        int contracted_axis = 0;
        for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
            if (right_axes[axis_pos] == axis) {
                contracted_axis = 1;
                break;
            }
        }
        if (contracted_axis) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != right_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (right_shape[axis] != 0 && expected_count > SIZE_MAX / right_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= right_shape[axis];
        result_axis++;
    }
    if (expected_rank == 0) expected_count = 1;
    if (expected_count != out_element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t left_byte_len = left_count * (sizeof(double) * 2u);
    size_t right_byte_len = right_count * (sizeof(double) * 2u);
    size_t out_byte_len = out_element_count * (sizeof(double) * 2u);

    if (out_element_count == 0) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    if (contracted_count == 0) {
        OmniTensorVulkanBuffer* output = NULL;
        int status = omni_tensor_backend_vulkan_create_buffer(out_byte_len, &output);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
        if (output == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        OmniVulkanDevice zero_device = output->context != NULL ? output->context->device : NULL;
        if (zero_device == NULL || output->memory == NULL || output->byte_len < out_byte_len) {
            omni_tensor_backend_vulkan_destroy_buffer_handle(output);
            return OMNI_TENSOR_VULKAN_INVALID;
        }
        void* mapped_zero = NULL;
        if (omni_vulkan_map_memory(zero_device, output->memory, 0, (OmniVulkanDeviceSize)out_byte_len, 0, &mapped_zero) != OMNI_VULKAN_SUCCESS || mapped_zero == NULL) {
            omni_tensor_backend_vulkan_destroy_buffer_handle(output);
            return OMNI_TENSOR_VULKAN_COPY_FAILED;
        }
        memset(mapped_zero, 0, out_byte_len);
        omni_vulkan_unmap_memory(zero_device, output->memory);
        *out_device_ptr = output;
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    if (left_device_ptr == NULL || right_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)left_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL ||
        right->context != context ||
        left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < left_byte_len || right->byte_len < right_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, out_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    if (out_rank > SIZE_MAX / 2u || left_rank > SIZE_MAX / 2u || right_rank > SIZE_MAX / 2u || axis_count > SIZE_MAX / 2u) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words_actual = (out_rank * 2u) + (left_rank * 2u) + (right_rank * 2u) + (axis_count * 2u);
    if (metadata_words_actual > SIZE_MAX / sizeof(uint32_t)) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words = metadata_words_actual == 0 ? 1 : metadata_words_actual;
    uint32_t* metadata = (uint32_t*)calloc(metadata_words, sizeof(uint32_t));
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    size_t metadata_cursor = 0;
    status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_shape, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_strides, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_shape, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_strides, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_shape, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_strides, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_axes, axis_count);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_axes, axis_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniTensorVulkanBuffer* metadata_buffer = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, metadata_words * sizeof(uint32_t), &metadata_buffer);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }
    void* mapped_metadata = NULL;
    if (omni_vulkan_map_memory(device, metadata_buffer->memory, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)), 0, &mapped_metadata) != OMNI_VULKAN_SUCCESS || mapped_metadata == NULL) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped_metadata, metadata, metadata_words * sizeof(uint32_t));
    omni_vulkan_unmap_memory(device, metadata_buffer->memory);
    free(metadata);

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[4] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {3, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        4,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(OmniTensorVulkanContractPushConstants)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        omni_tensor_vulkan_contract_complex128_spv_size,
        omni_tensor_vulkan_contract_complex128_spv
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &shader_info, NULL, &shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, &pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 4 };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo left_info = { left->buffer, 0, (OmniVulkanDeviceSize)left_byte_len };
    OmniVulkanDescriptorBufferInfo right_info = { right->buffer, 0, (OmniVulkanDeviceSize)right_byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)out_byte_len };
    OmniVulkanDescriptorBufferInfo metadata_info = { metadata_buffer->buffer, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)) };
    OmniVulkanWriteDescriptorSet writes[4] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &left_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &right_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 3, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &metadata_info, NULL }
    };
    omni_vulkan_update_descriptor_sets(device, 4, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniTensorVulkanContractPushConstants push = {
        (uint32_t)left_rank,
        (uint32_t)right_rank,
        (uint32_t)axis_count,
        (uint32_t)out_element_count,
        (uint32_t)out_rank
    };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    uint32_t group_count = ((uint32_t)out_element_count + OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE;
    omni_vulkan_cmd_dispatch(command_buffer, group_count, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (metadata_buffer != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_contract_complex64(
    const void* left_device_ptr,
    const void* right_device_ptr,
    size_t left_rank,
    size_t right_rank,
    size_t axis_count,
    const size_t* left_axes,
    const size_t* right_axes,
    size_t out_element_count,
    size_t out_rank,
    const size_t* left_shape,
    const size_t* left_strides,
    const size_t* right_shape,
    const size_t* right_strides,
    const size_t* out_shape,
    const size_t* out_strides,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axis_count > left_rank || axis_count > right_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (axis_count > 0 && (left_axes == NULL || right_axes == NULL)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if ((left_rank > 0 && (left_shape == NULL || left_strides == NULL)) ||
        (right_rank > 0 && (right_shape == NULL || right_strides == NULL))) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (out_rank > 0 && (out_shape == NULL || out_strides == NULL)) return OMNI_TENSOR_VULKAN_INVALID;
    if (left_rank > UINT32_MAX || right_rank > UINT32_MAX ||
        axis_count > UINT32_MAX ||
        out_element_count > UINT32_MAX || out_rank > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t left_count = 0;
    size_t right_count = 0;
    int shape_status = omni_tensor_vulkan_dense_required_elements(left_rank, left_shape, left_strides, &left_count);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    shape_status = omni_tensor_vulkan_dense_required_elements(right_rank, right_shape, right_strides, &right_count);
    if (shape_status != OMNI_TENSOR_VULKAN_SUCCESS) return shape_status;
    if (left_count > SIZE_MAX / (sizeof(float) * 2u) || right_count > SIZE_MAX / (sizeof(float) * 2u) ||
        out_element_count > SIZE_MAX / (sizeof(float) * 2u)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    size_t contracted_count = 1;
    for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
        size_t left_axis = left_axes[axis_pos];
        size_t right_axis = right_axes[axis_pos];
        if (left_axis >= left_rank || right_axis >= right_rank) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        for (size_t previous = 0; previous < axis_pos; previous++) {
            if (left_axes[previous] == left_axis || right_axes[previous] == right_axis) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        }
        if (left_shape[left_axis] != right_shape[right_axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (left_shape[left_axis] != 0 && contracted_count > SIZE_MAX / left_shape[left_axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        contracted_count *= left_shape[left_axis];
    }
    if (contracted_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t expected_rank = (left_rank - axis_count) + (right_rank - axis_count);
    if (out_rank != expected_rank) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t result_axis = 0;
    size_t expected_count = 1;
    for (size_t axis = 0; axis < left_rank; axis++) {
        int contracted_axis = 0;
        for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
            if (left_axes[axis_pos] == axis) {
                contracted_axis = 1;
                break;
            }
        }
        if (contracted_axis) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != left_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (left_shape[axis] != 0 && expected_count > SIZE_MAX / left_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= left_shape[axis];
        result_axis++;
    }
    for (size_t axis = 0; axis < right_rank; axis++) {
        int contracted_axis = 0;
        for (size_t axis_pos = 0; axis_pos < axis_count; axis_pos++) {
            if (right_axes[axis_pos] == axis) {
                contracted_axis = 1;
                break;
            }
        }
        if (contracted_axis) continue;
        if (result_axis >= out_rank || out_shape[result_axis] != right_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        if (right_shape[axis] != 0 && expected_count > SIZE_MAX / right_shape[axis]) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
        expected_count *= right_shape[axis];
        result_axis++;
    }
    if (expected_rank == 0) expected_count = 1;
    if (expected_count != out_element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    size_t left_byte_len = left_count * (sizeof(float) * 2u);
    size_t right_byte_len = right_count * (sizeof(float) * 2u);
    size_t out_byte_len = out_element_count * (sizeof(float) * 2u);

    if (out_element_count == 0) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    if (contracted_count == 0) {
        OmniTensorVulkanBuffer* output = NULL;
        int status = omni_tensor_backend_vulkan_create_buffer(out_byte_len, &output);
        if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
        if (output == NULL) return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        OmniVulkanDevice zero_device = output->context != NULL ? output->context->device : NULL;
        if (zero_device == NULL || output->memory == NULL || output->byte_len < out_byte_len) {
            omni_tensor_backend_vulkan_destroy_buffer_handle(output);
            return OMNI_TENSOR_VULKAN_INVALID;
        }
        void* mapped_zero = NULL;
        if (omni_vulkan_map_memory(zero_device, output->memory, 0, (OmniVulkanDeviceSize)out_byte_len, 0, &mapped_zero) != OMNI_VULKAN_SUCCESS || mapped_zero == NULL) {
            omni_tensor_backend_vulkan_destroy_buffer_handle(output);
            return OMNI_TENSOR_VULKAN_COPY_FAILED;
        }
        memset(mapped_zero, 0, out_byte_len);
        omni_vulkan_unmap_memory(zero_device, output->memory);
        *out_device_ptr = output;
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    if (left_device_ptr == NULL || right_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)left_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL ||
        right->context != context ||
        left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < left_byte_len || right->byte_len < right_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, out_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    if (out_rank > SIZE_MAX / 2u || left_rank > SIZE_MAX / 2u || right_rank > SIZE_MAX / 2u || axis_count > SIZE_MAX / 2u) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words_actual = (out_rank * 2u) + (left_rank * 2u) + (right_rank * 2u) + (axis_count * 2u);
    if (metadata_words_actual > SIZE_MAX / sizeof(uint32_t)) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t metadata_words = metadata_words_actual == 0 ? 1 : metadata_words_actual;
    uint32_t* metadata = (uint32_t*)calloc(metadata_words, sizeof(uint32_t));
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    size_t metadata_cursor = 0;
    status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_shape, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, out_strides, out_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_shape, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_strides, left_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_shape, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_strides, right_rank);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, left_axes, axis_count);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) status = omni_tensor_vulkan_write_u32_metadata(metadata, &metadata_cursor, right_axes, axis_count);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniTensorVulkanBuffer* metadata_buffer = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, metadata_words * sizeof(uint32_t), &metadata_buffer);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }
    void* mapped_metadata = NULL;
    if (omni_vulkan_map_memory(device, metadata_buffer->memory, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)), 0, &mapped_metadata) != OMNI_VULKAN_SUCCESS || mapped_metadata == NULL) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return OMNI_TENSOR_VULKAN_COPY_FAILED;
    }
    memcpy(mapped_metadata, metadata, metadata_words * sizeof(uint32_t));
    omni_vulkan_unmap_memory(device, metadata_buffer->memory);
    free(metadata);

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[4] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {3, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        4,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(OmniTensorVulkanContractPushConstants)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        omni_tensor_vulkan_contract_complex64_spv_size,
        omni_tensor_vulkan_contract_complex64_spv
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &shader_info, NULL, &shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, &pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 4 };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo left_info = { left->buffer, 0, (OmniVulkanDeviceSize)left_byte_len };
    OmniVulkanDescriptorBufferInfo right_info = { right->buffer, 0, (OmniVulkanDeviceSize)right_byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)out_byte_len };
    OmniVulkanDescriptorBufferInfo metadata_info = { metadata_buffer->buffer, 0, (OmniVulkanDeviceSize)(metadata_words * sizeof(uint32_t)) };
    OmniVulkanWriteDescriptorSet writes[4] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &left_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &right_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 3, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &metadata_info, NULL }
    };
    omni_vulkan_update_descriptor_sets(device, 4, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniTensorVulkanContractPushConstants push = {
        (uint32_t)left_rank,
        (uint32_t)right_rank,
        (uint32_t)axis_count,
        (uint32_t)out_element_count,
        (uint32_t)out_rank
    };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    uint32_t group_count = ((uint32_t)out_element_count + OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_CONTRACT_LOCAL_SIZE;
    omni_vulkan_cmd_dispatch(command_buffer, group_count, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (metadata_buffer != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(metadata_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_map_unary_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    uint32_t op,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (op > 4 && op != 19u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        op,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_map_unary_f64_spv,
        omni_tensor_vulkan_map_unary_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_unary_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    uint32_t op,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (op > 16 && op != 19u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        op,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_map_unary_f32_spv,
        omni_tensor_vulkan_map_unary_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_complex128_unary(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    uint32_t op,
    void** out_device_ptr
) {
    if (op > 5u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / (sizeof(double) * 2u) || byte_len != element_count * sizeof(double) * 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        op,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_map_complex128_unary_spv,
        omni_tensor_vulkan_map_complex128_unary_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_complex64_unary(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    uint32_t op,
    void** out_device_ptr
) {
    if (op > 5u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / (sizeof(float) * 2u) || byte_len != element_count * sizeof(float) * 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        op,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_map_complex64_unary_spv,
        omni_tensor_vulkan_map_complex64_unary_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_complex128_to_real(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t element_count,
    uint32_t op,
    void** out_device_ptr
) {
    if (op != 0u && op != 3u && op != 4u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / (sizeof(double) * 2u) ||
        input_byte_len != element_count * sizeof(double) * 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        op,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        input_byte_len,
        element_count * sizeof(double),
        element_count,
        omni_tensor_vulkan_map_complex128_to_real_spv,
        omni_tensor_vulkan_map_complex128_to_real_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_complex64_to_real(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t element_count,
    uint32_t op,
    void** out_device_ptr
) {
    if (op != 0u && op != 3u && op != 4u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / (sizeof(float) * 2u) ||
        input_byte_len != element_count * sizeof(float) * 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        op,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        input_byte_len,
        element_count * sizeof(float),
        element_count,
        omni_tensor_vulkan_map_complex64_to_real_spv,
        omni_tensor_vulkan_map_complex64_to_real_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_map_normal_quantile_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int result = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) return result;

    OmniTensorVulkanBuffer* status_buffer = NULL;
    result = omni_tensor_backend_vulkan_create_buffer_on_context(context, sizeof(uint32_t), &status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }
    uint32_t status_zero = 0u;
    result = omni_tensor_backend_vulkan_copy_to_existing_device(&status_zero, sizeof(status_zero), status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[3] = {
        { input->buffer, (OmniVulkanDeviceSize)byte_len },
        { output->buffer, (OmniVulkanDeviceSize)byte_len },
        { status_buffer->buffer, (OmniVulkanDeviceSize)sizeof(uint32_t) }
    };

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        0,
        0,
        0
    };
    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(push)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptors.layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS ||
        pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        omni_tensor_vulkan_normal_quantile_f32_spv,
        omni_tensor_vulkan_normal_quantile_f32_spv_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        &push,
        (uint32_t)sizeof(push),
        group_count
    );

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) {
        uint32_t status_host = 0u;
        int copy_status = omni_tensor_backend_vulkan_copy_to_host(status_buffer, sizeof(status_host), &status_host);
        if (copy_status != OMNI_TENSOR_VULKAN_SUCCESS) {
            result = copy_status;
        } else if (status_host == 1u) {
            result = OMNI_TENSOR_VULKAN_DOMAIN_ERROR;
        } else if (status_host == 2u) {
            result = OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
        } else if (status_host != 0u) {
            result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
    }
    omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_map_normal_quantile_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int result = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) return result;

    OmniTensorVulkanBuffer* status_buffer = NULL;
    result = omni_tensor_backend_vulkan_create_buffer_on_context(context, sizeof(uint32_t), &status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }
    uint32_t status_zero = 0u;
    result = omni_tensor_backend_vulkan_copy_to_existing_device(&status_zero, sizeof(status_zero), status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[3] = {
        { input->buffer, (OmniVulkanDeviceSize)byte_len },
        { output->buffer, (OmniVulkanDeviceSize)byte_len },
        { status_buffer->buffer, (OmniVulkanDeviceSize)sizeof(uint32_t) }
    };

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniTensorVulkanMapUnaryPushConstants push = {
        (uint32_t)element_count,
        0,
        0,
        0
    };
    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(push)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptors.layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS ||
        pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        omni_tensor_vulkan_normal_quantile_f64_spv,
        omni_tensor_vulkan_normal_quantile_f64_spv_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        &push,
        (uint32_t)sizeof(push),
        group_count
    );

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) {
        uint32_t status_host = 0u;
        int copy_status = omni_tensor_backend_vulkan_copy_to_host(status_buffer, sizeof(status_host), &status_host);
        if (copy_status != OMNI_TENSOR_VULKAN_SUCCESS) {
            result = copy_status;
        } else if (status_host == 1u) {
            result = OMNI_TENSOR_VULKAN_DOMAIN_ERROR;
        } else if (status_host == 2u) {
            result = OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
        } else if (status_host != 0u) {
            result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
    }
    omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_round_i64_launch(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    size_t element_size,
    unsigned int op,
    const uint32_t* shader_words,
    size_t shader_size,
    int require_float64,
    int64_t* host_out
) {
    if (op < 1 || op > 4) return OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_int64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (require_float64 && !omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count == 0) return byte_len == 0 ? OMNI_TENSOR_VULKAN_SUCCESS : OMNI_TENSOR_VULKAN_INVALID;
    if (input_device_ptr == NULL || host_out == NULL || shader_words == NULL || shader_size == 0) return OMNI_TENSOR_VULKAN_INVALID;
    if (element_size == 0 || element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX / sizeof(int64_t)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < byte_len) return OMNI_TENSOR_VULKAN_INVALID;
    OmniVulkanDevice device = context->device;
    size_t out_byte_len = element_count * sizeof(int64_t);

    OmniTensorVulkanBuffer* output = NULL;
    int result = omni_tensor_backend_vulkan_create_buffer_on_context(context, out_byte_len, &output);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) return result;

    OmniTensorVulkanBuffer* status_buffer = NULL;
    result = omni_tensor_backend_vulkan_create_buffer_on_context(context, sizeof(uint32_t), &status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }
    uint32_t status_zero = 0u;
    result = omni_tensor_backend_vulkan_copy_to_existing_device(&status_zero, sizeof(status_zero), status_buffer);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[3] = {
        { input->buffer, (OmniVulkanDeviceSize)byte_len },
        { output->buffer, (OmniVulkanDeviceSize)out_byte_len },
        { status_buffer->buffer, (OmniVulkanDeviceSize)sizeof(uint32_t) }
    };

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniTensorVulkanMapUnaryPushConstants push = { (uint32_t)element_count, op, 0, 0 };
    OmniVulkanPushConstantRange push_range = { OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push) };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptors.layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(device, pipeline_layout, shader_words, shader_size, &shader_module, &pipeline);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_MAP_LOCAL_SIZE;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        &push,
        (uint32_t)sizeof(push),
        group_count
    );

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) {
        uint32_t status_host = 0u;
        int copy_status = omni_tensor_backend_vulkan_copy_to_host(status_buffer, sizeof(status_host), &status_host);
        if (copy_status != OMNI_TENSOR_VULKAN_SUCCESS) {
            result = copy_status;
        } else if (status_host == 2u) {
            result = OMNI_TENSOR_VULKAN_INVALID_ARGUMENT;
        } else if (status_host != 0u) {
            result = OMNI_TENSOR_VULKAN_DOMAIN_ERROR;
        }
    }
    if (result == OMNI_TENSOR_VULKAN_SUCCESS) result = omni_tensor_backend_vulkan_copy_to_host(output, out_byte_len, host_out);
    omni_tensor_backend_vulkan_destroy_buffer_handle(status_buffer);
    omni_tensor_backend_vulkan_destroy_buffer_handle(output);
    return result;
}

int omni_tensor_backend_vulkan_round_i64_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    int64_t* host_out
) {
    return omni_tensor_backend_vulkan_round_i64_launch(
        input_device_ptr,
        byte_len,
        element_count,
        sizeof(double),
        op,
        omni_tensor_vulkan_round_i64_f64_spv,
        omni_tensor_vulkan_round_i64_f64_spv_size,
        1,
        host_out
    );
}

int omni_tensor_backend_vulkan_round_i64_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t element_count,
    unsigned int op,
    int64_t* host_out
) {
    return omni_tensor_backend_vulkan_round_i64_launch(
        input_device_ptr,
        byte_len,
        element_count,
        sizeof(float),
        op,
        omni_tensor_vulkan_round_i64_f32_spv,
        omni_tensor_vulkan_round_i64_f32_spv_size,
        0,
        host_out
    );
}

static int omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (output_element_count == 0 || output_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL || input_byte_len == 0 || shader_words == NULL || shader_size == 0 || push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (output_element_count > UINT32_MAX || push_size > UINT32_MAX || output_byte_len < output_element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < input_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[2] = {
        { input->buffer, (OmniVulkanDeviceSize)input_byte_len },
        { NULL, (OmniVulkanDeviceSize)output_byte_len }
    };
    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[1].buffer = output->buffer;

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 2, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)push_size
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptors.layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        shader_words,
        shader_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 2, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    uint32_t group_count = ((uint32_t)output_element_count + local_size - 1u) / local_size;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        push_data,
        (uint32_t)push_size,
        group_count
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (output_element_count == 0 || output_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL || input_byte_len == 0 || shader_words == NULL || shader_size == 0 || push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (output_element_count > UINT32_MAX || push_size > UINT32_MAX || output_byte_len < output_element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < input_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[2] = {
        { input->buffer, (OmniVulkanDeviceSize)input_byte_len },
        { NULL, (OmniVulkanDeviceSize)output_byte_len }
    };
    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[1].buffer = output->buffer;

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 2, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)push_size
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptors.layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        shader_words,
        shader_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 2, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    uint32_t group_count = ((uint32_t)output_element_count + local_size - 1u) / local_size;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        push_data,
        (uint32_t)push_size,
        group_count
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_dispatch_three_buffer_f64(
    const void* left_device_ptr,
    size_t left_byte_len,
    const void* right_device_ptr,
    size_t right_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (output_element_count == 0 || output_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (left_device_ptr == NULL || right_device_ptr == NULL ||
        left_byte_len == 0 || right_byte_len == 0 ||
        shader_words == NULL || shader_size == 0 ||
        push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (output_element_count > UINT32_MAX || push_size > UINT32_MAX || output_byte_len < output_element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)left_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL ||
        right->context != context ||
        left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < left_byte_len || right->byte_len < right_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[3] = {
        { left->buffer, (OmniVulkanDeviceSize)left_byte_len },
        { right->buffer, (OmniVulkanDeviceSize)right_byte_len },
        { NULL, (OmniVulkanDeviceSize)output_byte_len }
    };
    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[2].buffer = output->buffer;

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)push_size
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptors.layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        shader_words,
        shader_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    uint32_t group_count = ((uint32_t)output_element_count + local_size - 1u) / local_size;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        push_data,
        (uint32_t)push_size,
        group_count
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
    const void* left_device_ptr,
    size_t left_byte_len,
    const void* right_device_ptr,
    size_t right_byte_len,
    size_t output_byte_len,
    size_t output_element_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (output_element_count == 0 || output_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (left_device_ptr == NULL || right_device_ptr == NULL ||
        left_byte_len == 0 || right_byte_len == 0 ||
        shader_words == NULL || shader_size == 0 ||
        push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (output_element_count > UINT32_MAX || push_size > UINT32_MAX || output_byte_len < output_element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)left_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL ||
        right->context != context ||
        left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < left_byte_len || right->byte_len < right_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[3] = {
        { left->buffer, (OmniVulkanDeviceSize)left_byte_len },
        { right->buffer, (OmniVulkanDeviceSize)right_byte_len },
        { NULL, (OmniVulkanDeviceSize)output_byte_len }
    };
    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[2].buffer = output->buffer;

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)push_size
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptors.layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        shader_words,
        shader_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    uint32_t group_count = ((uint32_t)output_element_count + local_size - 1u) / local_size;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        push_data,
        (uint32_t)push_size,
        group_count
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_dispatch_one_input_two_outputs(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t first_output_byte_len,
    size_t second_output_byte_len,
    size_t work_item_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_first_device_ptr,
    void** out_second_device_ptr
) {
    if (out_first_device_ptr == NULL || out_second_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_first_device_ptr = NULL;
    *out_second_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (work_item_count == 0 || first_output_byte_len == 0 || second_output_byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL || input_byte_len == 0 ||
        shader_words == NULL || shader_size == 0 ||
        push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (work_item_count > UINT32_MAX || push_size > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < input_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[3] = {
        { input->buffer, (OmniVulkanDeviceSize)input_byte_len },
        { NULL, (OmniVulkanDeviceSize)first_output_byte_len },
        { NULL, (OmniVulkanDeviceSize)second_output_byte_len }
    };
    OmniTensorVulkanBuffer* first_output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, first_output_byte_len, &first_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[1].buffer = first_output->buffer;

    OmniTensorVulkanBuffer* second_output = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, second_output_byte_len, &second_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return status;
    }
    buffer_descriptors[2].buffer = second_output->buffer;

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)push_size
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptors.layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        shader_words,
        shader_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 3, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    uint32_t group_count = ((uint32_t)work_item_count + local_size - 1u) / local_size;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        push_data,
        (uint32_t)push_size,
        group_count
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(second_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return result;
    }

    *out_first_device_ptr = first_output;
    *out_second_device_ptr = second_output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_dispatch_one_input_three_outputs(
    const void* input_device_ptr,
    size_t input_byte_len,
    size_t first_output_byte_len,
    size_t second_output_byte_len,
    size_t third_output_byte_len,
    size_t work_item_count,
    const uint32_t* shader_words,
    size_t shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_first_device_ptr,
    void** out_second_device_ptr,
    void** out_third_device_ptr
) {
    if (out_first_device_ptr == NULL || out_second_device_ptr == NULL || out_third_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_first_device_ptr = NULL;
    *out_second_device_ptr = NULL;
    *out_third_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (work_item_count == 0 || first_output_byte_len == 0 || second_output_byte_len == 0 || third_output_byte_len == 0) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }
    if (input_device_ptr == NULL || input_byte_len == 0 ||
        shader_words == NULL || shader_size == 0 ||
        push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (work_item_count > UINT32_MAX || push_size > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < input_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanStorageBufferDescriptor buffer_descriptors[4] = {
        { input->buffer, (OmniVulkanDeviceSize)input_byte_len },
        { NULL, (OmniVulkanDeviceSize)first_output_byte_len },
        { NULL, (OmniVulkanDeviceSize)second_output_byte_len },
        { NULL, (OmniVulkanDeviceSize)third_output_byte_len }
    };
    OmniTensorVulkanBuffer* first_output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, first_output_byte_len, &first_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    buffer_descriptors[1].buffer = first_output->buffer;

    OmniTensorVulkanBuffer* second_output = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, second_output_byte_len, &second_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return status;
    }
    buffer_descriptors[2].buffer = second_output->buffer;

    OmniTensorVulkanBuffer* third_output = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, third_output_byte_len, &third_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(second_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return status;
    }
    buffer_descriptors[3].buffer = third_output->buffer;

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniTensorVulkanDescriptorResources descriptors = {0};
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_storage_descriptor_set_layout(device, 4, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)push_size
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptors.layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        shader_words,
        shader_size,
        &shader_module,
        &pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    result = omni_tensor_vulkan_allocate_storage_descriptor_set(device, buffer_descriptors, 4, &descriptors);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

    uint32_t group_count = ((uint32_t)work_item_count + local_size - 1u) / local_size;
    result = omni_tensor_vulkan_record_submit_single_dispatch(
        device,
        queue,
        context->queue_family_index,
        pipeline,
        pipeline_layout,
        descriptors.set,
        push_data,
        (uint32_t)push_size,
        group_count
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        goto cleanup;
    }

cleanup:
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    omni_tensor_vulkan_destroy_storage_descriptor_resources(device, &descriptors);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(third_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(second_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return result;
    }

    *out_first_device_ptr = first_output;
    *out_second_device_ptr = second_output;
    *out_third_device_ptr = third_output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static void omni_tensor_vulkan_destroy_storage_descriptor_resources(
    OmniVulkanDevice device,
    OmniTensorVulkanDescriptorResources* resources
) {
    if (device == NULL || resources == NULL) return;
    if (resources->pool != NULL) omni_vulkan_destroy_descriptor_pool(device, resources->pool, NULL);
    if (resources->layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, resources->layout, NULL);
    resources->layout = NULL;
    resources->pool = NULL;
    resources->set = NULL;
}

static int omni_tensor_vulkan_create_storage_descriptor_set_layout(
    OmniVulkanDevice device,
    uint32_t binding_count,
    OmniTensorVulkanDescriptorResources* resources
) {
    if (resources == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    resources->layout = NULL;
    resources->pool = NULL;
    resources->set = NULL;
    if (device == NULL || binding_count == 0 || binding_count > OMNI_TENSOR_VULKAN_MAX_STORAGE_DESCRIPTORS) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[OMNI_TENSOR_VULKAN_MAX_STORAGE_DESCRIPTORS];
    for (uint32_t i = 0; i < binding_count; i++) {
        bindings[i] = (OmniVulkanDescriptorSetLayoutBinding){
            i,
            OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER,
            1,
            OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
            NULL
        };
    }

    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        binding_count,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &resources->layout) != OMNI_VULKAN_SUCCESS ||
        resources->layout == NULL) {
        omni_tensor_vulkan_destroy_storage_descriptor_resources(device, resources);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_allocate_storage_descriptor_set(
    OmniVulkanDevice device,
    const OmniTensorVulkanStorageBufferDescriptor* descriptors,
    uint32_t descriptor_count,
    OmniTensorVulkanDescriptorResources* resources
) {
    if (device == NULL || descriptors == NULL || resources == NULL || resources->layout == NULL ||
        descriptor_count == 0 || descriptor_count > OMNI_TENSOR_VULKAN_MAX_STORAGE_DESCRIPTORS) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniVulkanDescriptorBufferInfo buffer_infos[OMNI_TENSOR_VULKAN_MAX_STORAGE_DESCRIPTORS];
    OmniVulkanWriteDescriptorSet writes[OMNI_TENSOR_VULKAN_MAX_STORAGE_DESCRIPTORS];
    for (uint32_t i = 0; i < descriptor_count; i++) {
        if (descriptors[i].buffer == NULL || descriptors[i].range == 0) {
            return OMNI_TENSOR_VULKAN_INVALID;
        }
        buffer_infos[i] = (OmniVulkanDescriptorBufferInfo){
            descriptors[i].buffer,
            0,
            descriptors[i].range
        };
    }

    OmniVulkanDescriptorPoolSize pool_size = {
        OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER,
        descriptor_count
    };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &resources->pool) != OMNI_VULKAN_SUCCESS ||
        resources->pool == NULL) {
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        resources->pool,
        1,
        &resources->layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &resources->set) != OMNI_VULKAN_SUCCESS ||
        resources->set == NULL) {
        if (resources->pool != NULL) omni_vulkan_destroy_descriptor_pool(device, resources->pool, NULL);
        resources->pool = NULL;
        resources->set = NULL;
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }

    for (uint32_t i = 0; i < descriptor_count; i++) {
        writes[i] = (OmniVulkanWriteDescriptorSet){
            OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
            NULL,
            resources->set,
            i,
            0,
            1,
            OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER,
            NULL,
            &buffer_infos[i],
            NULL
        };
    }
    omni_vulkan_update_descriptor_sets(device, descriptor_count, writes, 0, NULL);
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_vulkan_record_submit_single_dispatch(
    OmniVulkanDevice device,
    OmniVulkanQueue queue,
    uint32_t queue_family_index,
    OmniVulkanPipeline pipeline,
    OmniVulkanPipelineLayout pipeline_layout,
    OmniVulkanDescriptorSet descriptor_set,
    const void* push_data,
    uint32_t push_size,
    uint32_t group_count
) {
    if (device == NULL || queue == NULL || pipeline == NULL || pipeline_layout == NULL ||
        descriptor_set == NULL || push_data == NULL || push_size == 0 || group_count == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }

    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS ||
        command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS ||
        command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, push_size, push_data);
    omni_vulkan_cmd_dispatch(command_buffer, group_count, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    return result;
}

static int omni_tensor_vulkan_create_compute_pipeline_for_shader(
    OmniVulkanDevice device,
    OmniVulkanPipelineLayout pipeline_layout,
    const uint32_t* shader_words,
    size_t shader_size,
    OmniVulkanShaderModule* out_shader_module,
    OmniVulkanPipeline* out_pipeline
) {
    if (device == NULL || pipeline_layout == NULL ||
        shader_words == NULL || shader_size == 0 ||
        out_shader_module == NULL || out_pipeline == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_shader_module = NULL;
    *out_pipeline = NULL;

    OmniVulkanShaderModuleCreateInfo shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        shader_size,
        shader_words
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &shader_info, NULL, out_shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || *out_shader_module == NULL) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }

    OmniVulkanPipelineShaderStageCreateInfo stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        *out_shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, out_pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || *out_pipeline == NULL) {
        omni_vulkan_destroy_shader_module(device, *out_shader_module, NULL);
        *out_shader_module = NULL;
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static void omni_tensor_vulkan_cmd_barrier_two_outputs(
    OmniVulkanCommandBuffer command_buffer,
    OmniTensorVulkanBuffer* first_output,
    size_t first_output_byte_len,
    OmniTensorVulkanBuffer* second_output,
    size_t second_output_byte_len
) {
    OmniVulkanBufferMemoryBarrier barriers[2] = {
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            first_output->buffer,
            0,
            (OmniVulkanDeviceSize)first_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            second_output->buffer,
            0,
            (OmniVulkanDeviceSize)second_output_byte_len
        }
    };
    omni_vulkan_cmd_pipeline_barrier(
        command_buffer,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        0,
        0,
        NULL,
        2,
        barriers,
        0,
        NULL
    );
}

static void omni_tensor_vulkan_cmd_barrier_three_outputs(
    OmniVulkanCommandBuffer command_buffer,
    OmniTensorVulkanBuffer* first_output,
    size_t first_output_byte_len,
    OmniTensorVulkanBuffer* second_output,
    size_t second_output_byte_len,
    OmniTensorVulkanBuffer* third_output,
    size_t third_output_byte_len
) {
    OmniVulkanBufferMemoryBarrier barriers[3] = {
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            first_output->buffer,
            0,
            (OmniVulkanDeviceSize)first_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            second_output->buffer,
            0,
            (OmniVulkanDeviceSize)second_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            third_output->buffer,
            0,
            (OmniVulkanDeviceSize)third_output_byte_len
        }
    };
    omni_vulkan_cmd_pipeline_barrier(
        command_buffer,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        0,
        0,
        NULL,
        3,
        barriers,
        0,
        NULL
    );
}

static void omni_tensor_vulkan_cmd_barrier_four_outputs(
    OmniVulkanCommandBuffer command_buffer,
    OmniTensorVulkanBuffer* first_output,
    size_t first_output_byte_len,
    OmniTensorVulkanBuffer* second_output,
    size_t second_output_byte_len,
    OmniTensorVulkanBuffer* third_output,
    size_t third_output_byte_len,
    OmniTensorVulkanBuffer* fourth_output,
    size_t fourth_output_byte_len
) {
    OmniVulkanBufferMemoryBarrier barriers[4] = {
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            first_output->buffer,
            0,
            (OmniVulkanDeviceSize)first_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            second_output->buffer,
            0,
            (OmniVulkanDeviceSize)second_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            third_output->buffer,
            0,
            (OmniVulkanDeviceSize)third_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            fourth_output->buffer,
            0,
            (OmniVulkanDeviceSize)fourth_output_byte_len
        }
    };
    omni_vulkan_cmd_pipeline_barrier(
        command_buffer,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        0,
        0,
        NULL,
        4,
        barriers,
        0,
        NULL
    );
}

static int omni_tensor_backend_vulkan_dispatch_two_inputs_two_outputs_f64(
    const void* left_device_ptr,
    size_t left_byte_len,
    const void* right_device_ptr,
    size_t right_byte_len,
    size_t first_output_byte_len,
    size_t second_output_byte_len,
    size_t first_work_item_count,
    size_t second_work_item_count,
    const uint32_t* first_shader_words,
    size_t first_shader_size,
    const uint32_t* second_shader_words,
    size_t second_shader_size,
    const void* push_data,
    size_t push_size,
    uint32_t local_size,
    void** out_first_device_ptr,
    void** out_second_device_ptr
) {
    if (out_first_device_ptr == NULL || out_second_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_first_device_ptr = NULL;
    *out_second_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (first_work_item_count == 0 || second_work_item_count == 0 || first_output_byte_len == 0 || second_output_byte_len == 0) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }
    if (left_device_ptr == NULL || right_device_ptr == NULL ||
        left_byte_len == 0 || right_byte_len == 0 ||
        first_shader_words == NULL || first_shader_size == 0 ||
        second_shader_words == NULL || second_shader_size == 0 ||
        push_data == NULL || push_size == 0 || local_size == 0) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    if (first_work_item_count > UINT32_MAX || second_work_item_count > UINT32_MAX || push_size > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* left = (OmniTensorVulkanBuffer*)left_device_ptr;
    OmniTensorVulkanBuffer* right = (OmniTensorVulkanBuffer*)right_device_ptr;
    OmniTensorVulkanContext* context = left->context;
    if (context == NULL || context->device == NULL ||
        right->context != context ||
        left->buffer == NULL || right->buffer == NULL ||
        left->byte_len < left_byte_len || right->byte_len < right_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* first_output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, first_output_byte_len, &first_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    OmniTensorVulkanBuffer* second_output = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, second_output_byte_len, &second_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return status;
    }

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule first_shader_module = NULL;
    OmniVulkanShaderModule second_shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline first_pipeline = NULL;
    OmniVulkanPipeline second_pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[4] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {3, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        4,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)push_size
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo first_shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        first_shader_size,
        first_shader_words
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &first_shader_info, NULL, &first_shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || first_shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo second_shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        second_shader_size,
        second_shader_words
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    shader_result = omni_vulkan_create_shader_module(device, &second_shader_info, NULL, &second_shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || second_shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo first_stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        first_shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo first_pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        first_stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &first_pipeline_info, NULL, &first_pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || first_pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo second_stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        second_shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo second_pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        second_stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &second_pipeline_info, NULL, &second_pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || second_pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 4 };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo left_info = { left->buffer, 0, (OmniVulkanDeviceSize)left_byte_len };
    OmniVulkanDescriptorBufferInfo right_info = { right->buffer, 0, (OmniVulkanDeviceSize)right_byte_len };
    OmniVulkanDescriptorBufferInfo first_output_info = { first_output->buffer, 0, (OmniVulkanDeviceSize)first_output_byte_len };
    OmniVulkanDescriptorBufferInfo second_output_info = { second_output->buffer, 0, (OmniVulkanDeviceSize)second_output_byte_len };
    OmniVulkanWriteDescriptorSet writes[4] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &left_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &right_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &first_output_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 3, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &second_output_info, NULL }
    };
    omni_vulkan_update_descriptor_sets(device, 4, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, first_pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)push_size, push_data);
    uint32_t first_group_count = ((uint32_t)first_work_item_count + local_size - 1u) / local_size;
    omni_vulkan_cmd_dispatch(command_buffer, first_group_count, 1, 1);

    OmniVulkanBufferMemoryBarrier barriers[2] = {
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            first_output->buffer,
            0,
            (OmniVulkanDeviceSize)first_output_byte_len
        },
        {
            OMNI_VULKAN_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
            NULL,
            OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_ACCESS_SHADER_READ_BIT | OMNI_VULKAN_ACCESS_SHADER_WRITE_BIT,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            OMNI_VULKAN_QUEUE_FAMILY_IGNORED,
            second_output->buffer,
            0,
            (OmniVulkanDeviceSize)second_output_byte_len
        }
    };
    omni_vulkan_cmd_pipeline_barrier(
        command_buffer,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        OMNI_VULKAN_PIPELINE_STAGE_COMPUTE_SHADER_BIT,
        0,
        0,
        NULL,
        2,
        barriers,
        0,
        NULL
    );

    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, second_pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)push_size, push_data);
    uint32_t second_group_count = ((uint32_t)second_work_item_count + local_size - 1u) / local_size;
    omni_vulkan_cmd_dispatch(command_buffer, second_group_count, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (second_pipeline != NULL) omni_vulkan_destroy_pipeline(device, second_pipeline, NULL);
    if (first_pipeline != NULL) omni_vulkan_destroy_pipeline(device, first_pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (second_shader_module != NULL) omni_vulkan_destroy_shader_module(device, second_shader_module, NULL);
    if (first_shader_module != NULL) omni_vulkan_destroy_shader_module(device, first_shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(second_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(first_output);
        return result;
    }

    *out_first_device_ptr = first_output;
    *out_second_device_ptr = second_output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_dispatch_solve_multi_typed(
    const void* coefficients_device_ptr,
    size_t coefficients_byte_len,
    const void* rhs_device_ptr,
    size_t rhs_byte_len,
    size_t output_byte_len,
    size_t status_byte_len,
    size_t init_work_item_count,
    size_t element_size,
    size_t n,
    size_t rhs_cols,
    size_t result_count,
    uint32_t local_size,
    int require_float64,
    const uint32_t* init_spv,
    size_t init_spv_size,
    const uint32_t* pivot_spv,
    size_t pivot_spv_size,
    const uint32_t* pivot_reduce_spv,
    size_t pivot_reduce_spv_size,
    const uint32_t* pivot_commit_spv,
    size_t pivot_commit_spv_size,
    const uint32_t* row_swap_spv,
    size_t row_swap_spv_size,
    const uint32_t* factor_spv,
    size_t factor_spv_size,
    const uint32_t* eliminate_spv,
    size_t eliminate_spv_size,
    const uint32_t* backsolve_spv,
    size_t backsolve_spv_size,
    long* pivot_reduce_counter,
    long* factor_stage_counter,
    void** out_output_device_ptr,
    void** out_status_device_ptr
) {
    if (out_output_device_ptr == NULL || out_status_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_output_device_ptr = NULL;
    *out_status_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (require_float64) {
        if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    } else if (!omni_tensor_backend_vulkan_float32_available()) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (coefficients_device_ptr == NULL || rhs_device_ptr == NULL ||
        coefficients_byte_len == 0 || rhs_byte_len == 0 ||
        output_byte_len == 0 || status_byte_len < sizeof(uint32_t) * 4u ||
        init_work_item_count == 0 || element_size == 0 || n == 0 || rhs_cols == 0 ||
        local_size == 0 || n > UINT32_MAX || rhs_cols > UINT32_MAX ||
        result_count > UINT32_MAX || init_work_item_count > UINT32_MAX ||
        init_spv == NULL || pivot_spv == NULL || pivot_reduce_spv == NULL ||
        pivot_commit_spv == NULL || row_swap_spv == NULL || factor_spv == NULL ||
        eliminate_spv == NULL || backsolve_spv == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    size_t max_pivot_groups = (n + local_size - 1u) / local_size;
    if (max_pivot_groups == 0 || max_pivot_groups > UINT32_MAX / 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t pivot_scratch_slots = max_pivot_groups * 2u;
    if (pivot_scratch_slots > (SIZE_MAX / element_size)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t pivot_scratch_byte_len = pivot_scratch_slots * element_size;
    if (n > (SIZE_MAX / element_size)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t factor_scratch_byte_len = n * element_size;
    if (pivot_scratch_slots > (SIZE_MAX - 4u)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t required_status_slots = 4u + pivot_scratch_slots;
    if (required_status_slots > (SIZE_MAX / sizeof(uint32_t))) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (status_byte_len < required_status_slots * sizeof(uint32_t)) return OMNI_TENSOR_VULKAN_INVALID;

    OmniTensorVulkanBuffer* coefficients = (OmniTensorVulkanBuffer*)coefficients_device_ptr;
    OmniTensorVulkanBuffer* rhs = (OmniTensorVulkanBuffer*)rhs_device_ptr;
    OmniTensorVulkanContext* context = coefficients->context;
    if (context == NULL || context->device == NULL ||
        rhs->context != context ||
        coefficients->buffer == NULL || rhs->buffer == NULL ||
        coefficients->byte_len < coefficients_byte_len || rhs->byte_len < rhs_byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, output_byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    OmniTensorVulkanBuffer* status_output = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, status_byte_len, &status_output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniTensorVulkanBuffer* pivot_scratch = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, pivot_scratch_byte_len, &pivot_scratch);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniTensorVulkanBuffer* factor_scratch = NULL;
    status = omni_tensor_backend_vulkan_create_buffer_on_context(context, factor_scratch_byte_len, &factor_scratch);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(pivot_scratch);
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return status;
    }

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule init_shader_module = NULL;
    OmniVulkanShaderModule pivot_shader_module = NULL;
    OmniVulkanShaderModule pivot_reduce_shader_module = NULL;
    OmniVulkanShaderModule pivot_commit_shader_module = NULL;
    OmniVulkanShaderModule row_swap_shader_module = NULL;
    OmniVulkanShaderModule factor_shader_module = NULL;
    OmniVulkanShaderModule eliminate_shader_module = NULL;
    OmniVulkanShaderModule backsolve_shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline init_pipeline = NULL;
    OmniVulkanPipeline pivot_pipeline = NULL;
    OmniVulkanPipeline pivot_reduce_pipeline = NULL;
    OmniVulkanPipeline pivot_commit_pipeline = NULL;
    OmniVulkanPipeline row_swap_pipeline = NULL;
    OmniVulkanPipeline factor_pipeline = NULL;
    OmniVulkanPipeline eliminate_pipeline = NULL;
    OmniVulkanPipeline backsolve_pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[6] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {2, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {3, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {4, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {5, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        6,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        sizeof(OmniTensorVulkanSolveMultiPushConstants)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        init_spv,
        init_spv_size,
        &init_shader_module,
        &init_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        pivot_spv,
        pivot_spv_size,
        &pivot_shader_module,
        &pivot_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        pivot_reduce_spv,
        pivot_reduce_spv_size,
        &pivot_reduce_shader_module,
        &pivot_reduce_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        pivot_commit_spv,
        pivot_commit_spv_size,
        &pivot_commit_shader_module,
        &pivot_commit_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        row_swap_spv,
        row_swap_spv_size,
        &row_swap_shader_module,
        &row_swap_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        factor_spv,
        factor_spv_size,
        &factor_shader_module,
        &factor_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        eliminate_spv,
        eliminate_spv_size,
        &eliminate_shader_module,
        &eliminate_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;
    result = omni_tensor_vulkan_create_compute_pipeline_for_shader(
        device,
        pipeline_layout,
        backsolve_spv,
        backsolve_spv_size,
        &backsolve_shader_module,
        &backsolve_pipeline
    );
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) goto cleanup;

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 6 };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo coefficients_info = { coefficients->buffer, 0, (OmniVulkanDeviceSize)coefficients_byte_len };
    OmniVulkanDescriptorBufferInfo rhs_info = { rhs->buffer, 0, (OmniVulkanDeviceSize)rhs_byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)output_byte_len };
    OmniVulkanDescriptorBufferInfo status_info = { status_output->buffer, 0, (OmniVulkanDeviceSize)status_byte_len };
    OmniVulkanDescriptorBufferInfo pivot_scratch_info = { pivot_scratch->buffer, 0, (OmniVulkanDeviceSize)pivot_scratch_byte_len };
    OmniVulkanDescriptorBufferInfo factor_scratch_info = { factor_scratch->buffer, 0, (OmniVulkanDeviceSize)factor_scratch_byte_len };
    OmniVulkanWriteDescriptorSet writes[6] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &coefficients_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &rhs_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 2, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 3, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &status_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 4, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &pivot_scratch_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 5, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &factor_scratch_info, NULL }
    };
    omni_vulkan_update_descriptor_sets(device, 6, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniTensorVulkanSolveMultiPushConstants push = {
        (uint32_t)n,
        (uint32_t)rhs_cols,
        (uint32_t)result_count,
        0,
        (uint32_t)init_work_item_count,
        0,
        0,
        0
    };
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, init_pipeline);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
    uint32_t init_group_count = ((uint32_t)init_work_item_count + local_size - 1u) / local_size;
    omni_vulkan_cmd_dispatch(command_buffer, init_group_count, 1, 1);
    omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);

    for (uint32_t pivot = 0; pivot < (uint32_t)n; pivot++) {
        uint32_t pivot_candidate_count = (uint32_t)n - pivot;
        uint32_t pivot_group_count = (pivot_candidate_count + local_size - 1u) / local_size;
        push.pivot = pivot;
        push.work_count = pivot_candidate_count;
        push.scratch_src = 0;
        push.scratch_dst = 0;
        push.scratch_count = (uint32_t)max_pivot_groups;
        omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pivot_pipeline);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
        omni_vulkan_cmd_dispatch(command_buffer, pivot_group_count, 1, 1);
        omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);

        uint32_t current_count = pivot_group_count;
        uint32_t src_offset = 0;
        uint32_t dst_offset = (uint32_t)max_pivot_groups;
        if (current_count > 1u) {
            g_omni_tensor_vulkan_solve_pivot_reduce_call_count++;
            if (pivot_reduce_counter != NULL) (*pivot_reduce_counter)++;
        }
        while (current_count > 1u) {
            uint32_t reduced_count = (current_count + local_size - 1u) / local_size;
            push.work_count = current_count;
            push.scratch_src = src_offset;
            push.scratch_dst = dst_offset;
            push.scratch_count = (uint32_t)max_pivot_groups;
            omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pivot_reduce_pipeline);
            omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
            omni_vulkan_cmd_dispatch(command_buffer, reduced_count, 1, 1);
            omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);

            current_count = reduced_count;
            uint32_t tmp_offset = src_offset;
            src_offset = dst_offset;
            dst_offset = tmp_offset;
        }

        push.work_count = 1;
        push.scratch_src = src_offset;
        push.scratch_dst = dst_offset;
        push.scratch_count = (uint32_t)max_pivot_groups;
        omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pivot_commit_pipeline);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
        omni_vulkan_cmd_dispatch(command_buffer, 1, 1, 1);
        omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);

        push.work_count = (uint32_t)n + (uint32_t)rhs_cols;
        omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, row_swap_pipeline);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
        uint32_t swap_group_count = (push.work_count + local_size - 1u) / local_size;
        omni_vulkan_cmd_dispatch(command_buffer, swap_group_count, 1, 1);
        omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);

        uint32_t trailing = (uint32_t)n - pivot - 1u;
        if (trailing == 0u) continue;
        g_omni_tensor_vulkan_solve_factor_stage_call_count++;
        if (factor_stage_counter != NULL) (*factor_stage_counter)++;
        push.work_count = trailing;
        omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, factor_pipeline);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
        uint32_t factor_group_count = (trailing + local_size - 1u) / local_size;
        omni_vulkan_cmd_dispatch(command_buffer, factor_group_count, 1, 1);
        omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);

        size_t coeff_updates = (size_t)trailing * (size_t)trailing;
        size_t rhs_updates = (size_t)trailing * rhs_cols;
        size_t eliminate_work = coeff_updates + rhs_updates;
        if (eliminate_work > UINT32_MAX) {
            result = OMNI_TENSOR_VULKAN_UNSUPPORTED;
            goto cleanup;
        }
        push.work_count = (uint32_t)eliminate_work;
        omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, eliminate_pipeline);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
        uint32_t eliminate_group_count = ((uint32_t)eliminate_work + local_size - 1u) / local_size;
        omni_vulkan_cmd_dispatch(command_buffer, eliminate_group_count, 1, 1);
        omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);
    }

    for (uint32_t i = (uint32_t)n; i > 0u; i--) {
        push.pivot = i - 1u;
        push.work_count = (uint32_t)rhs_cols;
        omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, backsolve_pipeline);
        omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, sizeof(push), &push);
        uint32_t backsolve_group_count = ((uint32_t)rhs_cols + local_size - 1u) / local_size;
        omni_vulkan_cmd_dispatch(command_buffer, backsolve_group_count, 1, 1);
        omni_tensor_vulkan_cmd_barrier_four_outputs(command_buffer, output, output_byte_len, status_output, status_byte_len, pivot_scratch, pivot_scratch_byte_len, factor_scratch, factor_scratch_byte_len);
    }

    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (backsolve_pipeline != NULL) omni_vulkan_destroy_pipeline(device, backsolve_pipeline, NULL);
    if (eliminate_pipeline != NULL) omni_vulkan_destroy_pipeline(device, eliminate_pipeline, NULL);
    if (factor_pipeline != NULL) omni_vulkan_destroy_pipeline(device, factor_pipeline, NULL);
    if (row_swap_pipeline != NULL) omni_vulkan_destroy_pipeline(device, row_swap_pipeline, NULL);
    if (pivot_commit_pipeline != NULL) omni_vulkan_destroy_pipeline(device, pivot_commit_pipeline, NULL);
    if (pivot_reduce_pipeline != NULL) omni_vulkan_destroy_pipeline(device, pivot_reduce_pipeline, NULL);
    if (pivot_pipeline != NULL) omni_vulkan_destroy_pipeline(device, pivot_pipeline, NULL);
    if (init_pipeline != NULL) omni_vulkan_destroy_pipeline(device, init_pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (backsolve_shader_module != NULL) omni_vulkan_destroy_shader_module(device, backsolve_shader_module, NULL);
    if (eliminate_shader_module != NULL) omni_vulkan_destroy_shader_module(device, eliminate_shader_module, NULL);
    if (factor_shader_module != NULL) omni_vulkan_destroy_shader_module(device, factor_shader_module, NULL);
    if (row_swap_shader_module != NULL) omni_vulkan_destroy_shader_module(device, row_swap_shader_module, NULL);
    if (pivot_commit_shader_module != NULL) omni_vulkan_destroy_shader_module(device, pivot_commit_shader_module, NULL);
    if (pivot_reduce_shader_module != NULL) omni_vulkan_destroy_shader_module(device, pivot_reduce_shader_module, NULL);
    if (pivot_shader_module != NULL) omni_vulkan_destroy_shader_module(device, pivot_shader_module, NULL);
    if (init_shader_module != NULL) omni_vulkan_destroy_shader_module(device, init_shader_module, NULL);
    if (factor_scratch != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(factor_scratch);
    if (pivot_scratch != NULL) omni_tensor_backend_vulkan_destroy_buffer_handle(pivot_scratch);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(status_output);
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_output_device_ptr = output;
    *out_status_device_ptr = status_output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_read_status_payload_f64(
    void* output_device,
    size_t status_offset_bytes,
    double* out_status_payload
) {
    if (out_status_payload == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_status_payload = 0.0;
    int status = omni_tensor_backend_vulkan_copy_range_to_host(
        output_device,
        status_offset_bytes,
        sizeof(*out_status_payload),
        out_status_payload
    );
    return status;
}

static int omni_tensor_backend_vulkan_read_mapped_status_f64(
    void* output_device,
    size_t status_offset_bytes,
    OmniTensorVulkanStatusMapperF64 mapper
) {
    if (mapper == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    double status_payload = 0.0;
    int status = omni_tensor_backend_vulkan_read_status_payload_f64(
        output_device,
        status_offset_bytes,
        &status_payload
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    return mapper(status_payload);
}

static int omni_tensor_backend_vulkan_read_status_payload_f32(
    void* output_device,
    size_t status_offset_bytes,
    float* out_status_payload
) {
    if (out_status_payload == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_status_payload = 0.0f;
    int status = omni_tensor_backend_vulkan_copy_range_to_host(
        output_device,
        status_offset_bytes,
        sizeof(*out_status_payload),
        out_status_payload
    );
    return status;
}

static int omni_tensor_backend_vulkan_read_mapped_status_f32(
    void* output_device,
    size_t status_offset_bytes,
    OmniTensorVulkanStatusMapperF32 mapper
) {
    if (mapper == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    float status_payload = 0.0f;
    int status = omni_tensor_backend_vulkan_read_status_payload_f32(
        output_device,
        status_offset_bytes,
        &status_payload
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    return mapper(status_payload);
}

static int omni_tensor_backend_vulkan_tail_status_from_payload_f64(double status_payload) {
    if (!isfinite(status_payload)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return status_payload == 0.0 ? OMNI_TENSOR_VULKAN_SUCCESS : OMNI_TENSOR_VULKAN_SINGULAR;
}

static int omni_tensor_backend_vulkan_tail_status_from_payload_f32(float status_payload) {
    if (!isfinite(status_payload)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return status_payload == 0.0f ? OMNI_TENSOR_VULKAN_SUCCESS : OMNI_TENSOR_VULKAN_SINGULAR;
}

static int omni_tensor_backend_vulkan_read_tail_status_f64(
    void* output_device,
    size_t status_offset_bytes
) {
    return omni_tensor_backend_vulkan_read_mapped_status_f64(
        output_device,
        status_offset_bytes,
        omni_tensor_backend_vulkan_tail_status_from_payload_f64
    );
}

static int omni_tensor_backend_vulkan_read_tail_status_f32(
    void* output_device,
    size_t status_offset_bytes
) {
    return omni_tensor_backend_vulkan_read_mapped_status_f32(
        output_device,
        status_offset_bytes,
        omni_tensor_backend_vulkan_tail_status_from_payload_f32
    );
}

static int omni_tensor_backend_vulkan_singular_values_status_from_payload_f64(double status_payload) {
    if (!isfinite(status_payload)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    if (status_payload == 0.0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (status_payload == 2.0) return OMNI_TENSOR_VULKAN_NO_CONVERGENCE;
    return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
}

static int omni_tensor_backend_vulkan_singular_values_status_from_payload_f32(float status_payload) {
    if (!isfinite(status_payload)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    if (status_payload == 0.0f) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (status_payload == 2.0f) return OMNI_TENSOR_VULKAN_NO_CONVERGENCE;
    return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
}

static int omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64(double status_payload) {
    if (!isfinite(status_payload)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    if (status_payload == 0.0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (status_payload == 2.0) return OMNI_TENSOR_VULKAN_NO_CONVERGENCE;
    if (status_payload == 3.0) return OMNI_TENSOR_VULKAN_NOT_SYMMETRIC;
    return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
}

static int omni_tensor_backend_vulkan_read_singular_values_status_f64(
    void* output_device,
    size_t status_offset_bytes
) {
    return omni_tensor_backend_vulkan_read_mapped_status_f64(
        output_device,
        status_offset_bytes,
        omni_tensor_backend_vulkan_singular_values_status_from_payload_f64
    );
}

static int omni_tensor_backend_vulkan_read_singular_values_status_f32(
    void* output_device,
    size_t status_offset_bytes
) {
    return omni_tensor_backend_vulkan_read_mapped_status_f32(
        output_device,
        status_offset_bytes,
        omni_tensor_backend_vulkan_singular_values_status_from_payload_f32
    );
}

static int omni_tensor_backend_vulkan_read_symmetric_eigen_status_f64(
    void* output_device,
    size_t status_offset_bytes
) {
    return omni_tensor_backend_vulkan_read_mapped_status_f64(
        output_device,
        status_offset_bytes,
        omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64
    );
}

int omni_tensor_backend_vulkan_singular_values_status_payload_probe_for_tests(void) {
    if (omni_tensor_backend_vulkan_tail_status_from_payload_f64(0.0) != OMNI_TENSOR_VULKAN_SUCCESS) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_tail_status_from_payload_f64(1.0) != OMNI_TENSOR_VULKAN_SINGULAR) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_tail_status_from_payload_f64(NAN) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f64(0.0) != OMNI_TENSOR_VULKAN_SUCCESS) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f64(1.0) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f64(2.0) != OMNI_TENSOR_VULKAN_NO_CONVERGENCE) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f64(NAN) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f64(INFINITY) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f32(0.0f) != OMNI_TENSOR_VULKAN_SUCCESS) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f32(1.0f) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f32(2.0f) != OMNI_TENSOR_VULKAN_NO_CONVERGENCE) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f32(NAN) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_singular_values_status_from_payload_f32(INFINITY) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64(0.0) != OMNI_TENSOR_VULKAN_SUCCESS) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64(2.0) != OMNI_TENSOR_VULKAN_NO_CONVERGENCE) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64(3.0) != OMNI_TENSOR_VULKAN_NOT_SYMMETRIC) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64(1.0) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    if (omni_tensor_backend_vulkan_symmetric_eigen_status_from_payload_f64(NAN) != OMNI_TENSOR_VULKAN_EXECUTION_FAILED) {
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_read_status_code_u32(void* status_device) {
    uint32_t status_payload = 1u;
    int status = omni_tensor_backend_vulkan_copy_to_host(
        status_device,
        sizeof(status_payload),
        &status_payload
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (status_payload == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (status_payload == 1u) return OMNI_TENSOR_VULKAN_SINGULAR;
    return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
}

int omni_tensor_backend_vulkan_diagonal_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t diagonal_count = rows < cols ? rows : cols;
    if (diagonal_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || diagonal_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanDiagonalPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)diagonal_count,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        diagonal_count * sizeof(double),
        diagonal_count,
        omni_tensor_vulkan_diagonal_f64_spv,
        omni_tensor_vulkan_diagonal_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_diagonal_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t diagonal_count = rows < cols ? rows : cols;
    if (diagonal_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || diagonal_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanDiagonalPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)diagonal_count,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        diagonal_count * sizeof(float),
        diagonal_count,
        omni_tensor_vulkan_diagonal_f32_spv,
        omni_tensor_vulkan_diagonal_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_diagonal_matrix_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = n * n;
    if (n > SIZE_MAX / sizeof(double) || byte_len != n * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (output_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || output_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDiagonalMatrixPushConstants push = {
        (uint32_t)n,
        (uint32_t)output_count,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * sizeof(double),
        output_count,
        omni_tensor_vulkan_diagonal_matrix_f64_spv,
        omni_tensor_vulkan_diagonal_matrix_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_MATRIX_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_diagonal_matrix_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = n * n;
    if (n > SIZE_MAX / sizeof(float) || byte_len != n * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (output_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || output_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDiagonalMatrixPushConstants push = {
        (uint32_t)n,
        (uint32_t)output_count,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * sizeof(float),
        output_count,
        omni_tensor_vulkan_diagonal_matrix_f32_spv,
        omni_tensor_vulkan_diagonal_matrix_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_MATRIX_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_trace_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    double* out_trace
) {
    if (out_trace == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_trace = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanTracePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        sizeof(double),
        1,
        omni_tensor_vulkan_trace_f64_spv,
        omni_tensor_vulkan_trace_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_TRACE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(double), out_trace);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    return status;
}

int omni_tensor_backend_vulkan_trace_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    float* out_trace
) {
    if (out_trace == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_trace = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanTracePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        sizeof(float),
        1,
        omni_tensor_vulkan_trace_f32_spv,
        omni_tensor_vulkan_trace_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_TRACE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(float), out_trace);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    return status;
}

int omni_tensor_backend_vulkan_diagonal_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t diagonal_count = rows < cols ? rows : cols;
    if (diagonal_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || diagonal_count > UINT32_MAX ||
        diagonal_count > SIZE_MAX / element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanDiagonalPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)diagonal_count,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        diagonal_count * element_size,
        diagonal_count,
        omni_tensor_vulkan_diagonal_complex128_spv,
        omni_tensor_vulkan_diagonal_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_diagonal_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t diagonal_count = rows < cols ? rows : cols;
    if (diagonal_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || diagonal_count > UINT32_MAX ||
        diagonal_count > SIZE_MAX / element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanDiagonalPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)diagonal_count,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        diagonal_count * element_size,
        diagonal_count,
        omni_tensor_vulkan_diagonal_complex64_spv,
        omni_tensor_vulkan_diagonal_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_diagonal_matrix_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = n * n;
    const size_t element_size = sizeof(double) * 2u;
    if (n > SIZE_MAX / element_size || byte_len != n * element_size ||
        output_count > SIZE_MAX / element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (output_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || output_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDiagonalMatrixPushConstants push = {
        (uint32_t)n,
        (uint32_t)output_count,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        output_count,
        omni_tensor_vulkan_diagonal_matrix_complex128_spv,
        omni_tensor_vulkan_diagonal_matrix_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_MATRIX_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_diagonal_matrix_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = n * n;
    const size_t element_size = sizeof(float) * 2u;
    if (n > SIZE_MAX / element_size || byte_len != n * element_size ||
        output_count > SIZE_MAX / element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (output_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || output_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDiagonalMatrixPushConstants push = {
        (uint32_t)n,
        (uint32_t)output_count,
        0,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        output_count,
        omni_tensor_vulkan_diagonal_matrix_complex64_spv,
        omni_tensor_vulkan_diagonal_matrix_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DIAGONAL_MATRIX_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_trace_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    double* out_real,
    double* out_imag
) {
    if (out_real == NULL || out_imag == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_real = 0.0;
    *out_imag = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanTracePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        element_size,
        1,
        omni_tensor_vulkan_trace_complex128_spv,
        omni_tensor_vulkan_trace_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_TRACE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double trace[2] = {0.0, 0.0};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(trace), trace);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_real = trace[0];
        *out_imag = trace[1];
    }
    return status;
}

int omni_tensor_backend_vulkan_trace_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    float* out_real,
    float* out_imag
) {
    if (out_real == NULL || out_imag == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_real = 0.0f;
    *out_imag = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanTracePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        element_size,
        1,
        omni_tensor_vulkan_trace_complex64_spv,
        omni_tensor_vulkan_trace_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_TRACE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    float trace[2] = {0.0f, 0.0f};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(trace), trace);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_real = trace[0];
        *out_imag = trace[1];
    }
    return status;
}

int omni_tensor_backend_vulkan_norm_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    unsigned int mode,
    double* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (mode > 3u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanNormPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)mode,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        sizeof(double) * 2u,
        1,
        omni_tensor_vulkan_norm_f64_spv,
        omni_tensor_vulkan_norm_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_NORM_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double norm_payload[2] = {0.0, 0.0};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(norm_payload), norm_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_norm = mode == 0u && norm_payload[0] > 0.0
            ? norm_payload[0] * sqrt(norm_payload[1])
            : norm_payload[0];
    }
    return status;
}

int omni_tensor_backend_vulkan_norm_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    unsigned int mode,
    float* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (mode > 3u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanNormPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)mode,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        sizeof(float) * 2u,
        1,
        omni_tensor_vulkan_norm_f32_spv,
        omni_tensor_vulkan_norm_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_NORM_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    float norm_payload[2] = {0.0f, 0.0f};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(norm_payload), norm_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_norm = mode == 0u && norm_payload[0] > 0.0f
            ? norm_payload[0] * sqrtf(norm_payload[1])
            : norm_payload[0];
    }
    return status;
}

int omni_tensor_backend_vulkan_norm_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    unsigned int mode,
    double* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (mode > 3u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanNormPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)mode,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        sizeof(double) * 2u,
        1,
        omni_tensor_vulkan_norm_complex128_spv,
        omni_tensor_vulkan_norm_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_NORM_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double norm_payload[2] = {0.0, 0.0};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(norm_payload), norm_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_norm = mode == 0u && norm_payload[0] > 0.0
            ? norm_payload[0] * sqrt(norm_payload[1])
            : norm_payload[0];
    }
    return status;
}

int omni_tensor_backend_vulkan_norm_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    unsigned int mode,
    float* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (mode > 3u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanNormPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)mode,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        sizeof(float) * 2u,
        1,
        omni_tensor_vulkan_norm_complex64_spv,
        omni_tensor_vulkan_norm_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_NORM_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    float norm_payload[2] = {0.0f, 0.0f};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(norm_payload), norm_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_norm = mode == 0u && norm_payload[0] > 0.0f
            ? norm_payload[0] * sqrtf(norm_payload[1])
            : norm_payload[0];
    }
    return status;
}

int omni_tensor_backend_vulkan_rank_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    double tolerance,
    double* out_rank
) {
    if (out_rank == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_rank = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!isfinite(tolerance) || tolerance < 0.0) return OMNI_TENSOR_VULKAN_INVALID;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanRankPushConstants push = {
        tolerance,
        (uint32_t)rows,
        (uint32_t)cols
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * sizeof(double),
        1,
        omni_tensor_vulkan_rank_f64_spv,
        omni_tensor_vulkan_rank_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_RANK_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double rank_payload = 0.0;
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(rank_payload), &rank_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        if (!isfinite(rank_payload) || rank_payload < 0.0) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        *out_rank = rank_payload;
    }
    return status;
}

int omni_tensor_backend_vulkan_rank_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    float tolerance,
    float* out_rank
) {
    if (out_rank == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_rank = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!isfinite(tolerance) || tolerance < 0.0f) return OMNI_TENSOR_VULKAN_INVALID;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t max_rank = rows < cols ? rows : cols;
    if (max_rank > 16777216u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanRankF32PushConstants push = {
        (float)tolerance,
        (uint32_t)rows,
        (uint32_t)cols,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * sizeof(float),
        1,
        omni_tensor_vulkan_rank_f32_spv,
        omni_tensor_vulkan_rank_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_RANK_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    float rank_payload = 0.0f;
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(rank_payload), &rank_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        if (!isfinite(rank_payload) || rank_payload < 0.0f) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        if ((size_t)rank_payload > max_rank) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        *out_rank = rank_payload;
    }
    return status;
}

int omni_tensor_backend_vulkan_rank_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    double tolerance,
    double* out_rank
) {
    if (out_rank == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_rank = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!isfinite(tolerance) || tolerance < 0.0) return OMNI_TENSOR_VULKAN_INVALID;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanRankPushConstants push = {
        tolerance,
        (uint32_t)rows,
        (uint32_t)cols
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_rank_complex128_spv,
        omni_tensor_vulkan_rank_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_RANK_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double rank_payload[2] = {0.0, 0.0};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(rank_payload), rank_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        if (!isfinite(rank_payload[0]) || rank_payload[0] < 0.0) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        *out_rank = rank_payload[0];
    }
    return status;
}

int omni_tensor_backend_vulkan_rank_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    float tolerance,
    float* out_rank
) {
    if (out_rank == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_rank = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!isfinite(tolerance) || tolerance < 0.0f) return OMNI_TENSOR_VULKAN_INVALID;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t max_rank = rows < cols ? rows : cols;
    if (max_rank > 16777216u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanRankF32PushConstants push = {
        (float)tolerance,
        (uint32_t)rows,
        (uint32_t)cols,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_rank_complex64_spv,
        omni_tensor_vulkan_rank_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_RANK_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    float rank_payload[2] = {0.0f, 0.0f};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(rank_payload), rank_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        if (!isfinite(rank_payload[0]) || rank_payload[0] < 0.0f) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        if ((size_t)rank_payload[0] > max_rank) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        *out_rank = rank_payload[0];
    }
    return status;
}

int omni_tensor_backend_vulkan_determinant_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    double* out_determinant
) {
    if (out_determinant == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_determinant = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) {
        *out_determinant = 1.0;
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDeterminantPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * sizeof(double),
        1,
        omni_tensor_vulkan_determinant_f64_spv,
        omni_tensor_vulkan_determinant_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DETERMINANT_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double determinant_payload = 0.0;
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(determinant_payload), &determinant_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_determinant = determinant_payload;
    }
    return status;
}

int omni_tensor_backend_vulkan_solve_f64(
    const void* coefficients_device_ptr,
    size_t coefficients_byte_len,
    const void* rhs_device_ptr,
    size_t rhs_byte_len,
    size_t n,
    size_t rhs_cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n == 0 || rhs_cols == 0) return OMNI_TENSOR_VULKAN_SINGULAR;
    if (n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t coefficient_count = n * n;
    if (coefficient_count > SIZE_MAX / sizeof(double) || coefficients_byte_len != coefficient_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rhs_cols > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t result_count = n * rhs_cols;
    if (result_count > SIZE_MAX / sizeof(double) || rhs_byte_len != result_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n > UINT32_MAX || rhs_cols > UINT32_MAX || result_count > UINT32_MAX || coefficient_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (result_count > SIZE_MAX - coefficient_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = result_count + coefficient_count;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanSolvePushConstants push = {
        (uint32_t)n,
        (uint32_t)rhs_cols,
        (uint32_t)result_count,
        0
    };
    g_omni_tensor_vulkan_solve_serial_call_count++;
    g_omni_tensor_vulkan_solve_serial_f64_call_count++;
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_three_buffer_f64(
        coefficients_device_ptr,
        coefficients_byte_len,
        rhs_device_ptr,
        rhs_byte_len,
        output_count * sizeof(double),
        1,
        omni_tensor_vulkan_solve_f64_spv,
        omni_tensor_vulkan_solve_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SOLVE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f64(
        output_device,
        (result_count + coefficient_count) * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

long omni_tensor_backend_vulkan_solve_parallel_call_count(void) {
    return g_omni_tensor_vulkan_solve_parallel_call_count;
}

long omni_tensor_backend_vulkan_solve_multi_dispatch_call_count(void) {
    return g_omni_tensor_vulkan_solve_multi_dispatch_call_count;
}

long omni_tensor_backend_vulkan_solve_pivot_reduce_call_count(void) {
    return g_omni_tensor_vulkan_solve_pivot_reduce_call_count;
}

long omni_tensor_backend_vulkan_solve_factor_stage_call_count(void) {
    return g_omni_tensor_vulkan_solve_factor_stage_call_count;
}

long omni_tensor_backend_vulkan_solve_serial_call_count(void) {
    return g_omni_tensor_vulkan_solve_serial_call_count;
}

long omni_tensor_backend_vulkan_solve_parallel_f32_call_count(void) {
    return g_omni_tensor_vulkan_solve_parallel_f32_call_count;
}

long omni_tensor_backend_vulkan_solve_multi_dispatch_f32_call_count(void) {
    return g_omni_tensor_vulkan_solve_multi_dispatch_f32_call_count;
}

long omni_tensor_backend_vulkan_solve_pivot_reduce_f32_call_count(void) {
    return g_omni_tensor_vulkan_solve_pivot_reduce_f32_call_count;
}

long omni_tensor_backend_vulkan_solve_factor_stage_f32_call_count(void) {
    return g_omni_tensor_vulkan_solve_factor_stage_f32_call_count;
}

long omni_tensor_backend_vulkan_solve_serial_f32_call_count(void) {
    return g_omni_tensor_vulkan_solve_serial_f32_call_count;
}

long omni_tensor_backend_vulkan_solve_parallel_f64_call_count(void) {
    return g_omni_tensor_vulkan_solve_parallel_f64_call_count;
}

long omni_tensor_backend_vulkan_solve_multi_dispatch_f64_call_count(void) {
    return g_omni_tensor_vulkan_solve_multi_dispatch_f64_call_count;
}

long omni_tensor_backend_vulkan_solve_pivot_reduce_f64_call_count(void) {
    return g_omni_tensor_vulkan_solve_pivot_reduce_f64_call_count;
}

long omni_tensor_backend_vulkan_solve_factor_stage_f64_call_count(void) {
    return g_omni_tensor_vulkan_solve_factor_stage_f64_call_count;
}

long omni_tensor_backend_vulkan_solve_serial_f64_call_count(void) {
    return g_omni_tensor_vulkan_solve_serial_f64_call_count;
}

int omni_tensor_backend_vulkan_solve_parallel_f64(
    const void* coefficients_device_ptr,
    size_t coefficients_byte_len,
    const void* rhs_device_ptr,
    size_t rhs_byte_len,
    size_t n,
    size_t rhs_cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n == 0 || rhs_cols == 0) return OMNI_TENSOR_VULKAN_SINGULAR;
    if (n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t coefficient_count = n * n;
    if (coefficient_count > SIZE_MAX / sizeof(double) || coefficients_byte_len != coefficient_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rhs_cols > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t result_count = n * rhs_cols;
    if (result_count > SIZE_MAX / sizeof(double) || rhs_byte_len != result_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n > UINT32_MAX || rhs_cols > UINT32_MAX || result_count > UINT32_MAX || coefficient_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (result_count > SIZE_MAX - coefficient_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = result_count + coefficient_count;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    void* output_device = NULL;
    void* status_device = NULL;
    size_t init_work_item_count = coefficient_count > result_count ? coefficient_count : result_count;
    size_t max_pivot_groups = (n + OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE - 1u) /
        OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE;
    if (max_pivot_groups == 0 || max_pivot_groups > (SIZE_MAX - 4u) / 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t status_slots = 4u + max_pivot_groups * 2u;
    if (status_slots > SIZE_MAX / sizeof(uint32_t)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    g_omni_tensor_vulkan_solve_parallel_call_count++;
    g_omni_tensor_vulkan_solve_multi_dispatch_call_count++;
    g_omni_tensor_vulkan_solve_parallel_f64_call_count++;
    g_omni_tensor_vulkan_solve_multi_dispatch_f64_call_count++;
    int status = omni_tensor_backend_vulkan_dispatch_solve_multi_typed(
        coefficients_device_ptr,
        coefficients_byte_len,
        rhs_device_ptr,
        rhs_byte_len,
        output_count * sizeof(double),
        status_slots * sizeof(uint32_t),
        init_work_item_count,
        sizeof(double),
        n,
        rhs_cols,
        result_count,
        OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE,
        1,
        omni_tensor_vulkan_solve_parallel_init_f64_spv,
        omni_tensor_vulkan_solve_parallel_init_f64_spv_size,
        omni_tensor_vulkan_solve_multi_pivot_f64_spv,
        omni_tensor_vulkan_solve_multi_pivot_f64_spv_size,
        omni_tensor_vulkan_solve_multi_pivot_reduce_f64_spv,
        omni_tensor_vulkan_solve_multi_pivot_reduce_f64_spv_size,
        omni_tensor_vulkan_solve_multi_pivot_commit_f64_spv,
        omni_tensor_vulkan_solve_multi_pivot_commit_f64_spv_size,
        omni_tensor_vulkan_solve_multi_row_swap_f64_spv,
        omni_tensor_vulkan_solve_multi_row_swap_f64_spv_size,
        omni_tensor_vulkan_solve_multi_factor_f64_spv,
        omni_tensor_vulkan_solve_multi_factor_f64_spv_size,
        omni_tensor_vulkan_solve_multi_eliminate_f64_spv,
        omni_tensor_vulkan_solve_multi_eliminate_f64_spv_size,
        omni_tensor_vulkan_solve_multi_backsolve_f64_spv,
        omni_tensor_vulkan_solve_multi_backsolve_f64_spv_size,
        &g_omni_tensor_vulkan_solve_pivot_reduce_f64_call_count,
        &g_omni_tensor_vulkan_solve_factor_stage_f64_call_count,
        &output_device,
        &status_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_status_code_u32(status_device);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)status_device);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_solve_parallel_f32(
    const void* coefficients_device_ptr,
    size_t coefficients_byte_len,
    const void* rhs_device_ptr,
    size_t rhs_byte_len,
    size_t n,
    size_t rhs_cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n == 0 || rhs_cols == 0) return OMNI_TENSOR_VULKAN_SINGULAR;
    if (n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t coefficient_count = n * n;
    if (coefficient_count > SIZE_MAX / sizeof(float) || coefficients_byte_len != coefficient_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rhs_cols > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t result_count = n * rhs_cols;
    if (result_count > SIZE_MAX / sizeof(float) || rhs_byte_len != result_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n > UINT32_MAX || rhs_cols > UINT32_MAX || result_count > UINT32_MAX || coefficient_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (result_count > SIZE_MAX - coefficient_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = result_count + coefficient_count;
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    void* output_device = NULL;
    void* status_device = NULL;
    size_t init_work_item_count = coefficient_count > result_count ? coefficient_count : result_count;
    size_t max_pivot_groups = (n + OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE - 1u) /
        OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE;
    if (max_pivot_groups == 0 || max_pivot_groups > (SIZE_MAX - 4u) / 2u) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t status_slots = 4u + max_pivot_groups * 2u;
    if (status_slots > SIZE_MAX / sizeof(uint32_t)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    g_omni_tensor_vulkan_solve_parallel_call_count++;
    g_omni_tensor_vulkan_solve_multi_dispatch_call_count++;
    g_omni_tensor_vulkan_solve_parallel_f32_call_count++;
    g_omni_tensor_vulkan_solve_multi_dispatch_f32_call_count++;
    int status = omni_tensor_backend_vulkan_dispatch_solve_multi_typed(
        coefficients_device_ptr,
        coefficients_byte_len,
        rhs_device_ptr,
        rhs_byte_len,
        output_count * sizeof(float),
        status_slots * sizeof(uint32_t),
        init_work_item_count,
        sizeof(float),
        n,
        rhs_cols,
        result_count,
        OMNI_TENSOR_VULKAN_SOLVE_PARALLEL_LOCAL_SIZE,
        0,
        omni_tensor_vulkan_solve_parallel_init_f32_spv,
        omni_tensor_vulkan_solve_parallel_init_f32_spv_size,
        omni_tensor_vulkan_solve_multi_pivot_f32_spv,
        omni_tensor_vulkan_solve_multi_pivot_f32_spv_size,
        omni_tensor_vulkan_solve_multi_pivot_reduce_f32_spv,
        omni_tensor_vulkan_solve_multi_pivot_reduce_f32_spv_size,
        omni_tensor_vulkan_solve_multi_pivot_commit_f32_spv,
        omni_tensor_vulkan_solve_multi_pivot_commit_f32_spv_size,
        omni_tensor_vulkan_solve_multi_row_swap_f32_spv,
        omni_tensor_vulkan_solve_multi_row_swap_f32_spv_size,
        omni_tensor_vulkan_solve_multi_factor_f32_spv,
        omni_tensor_vulkan_solve_multi_factor_f32_spv_size,
        omni_tensor_vulkan_solve_multi_eliminate_f32_spv,
        omni_tensor_vulkan_solve_multi_eliminate_f32_spv_size,
        omni_tensor_vulkan_solve_multi_backsolve_f32_spv,
        omni_tensor_vulkan_solve_multi_backsolve_f32_spv_size,
        &g_omni_tensor_vulkan_solve_pivot_reduce_f32_call_count,
        &g_omni_tensor_vulkan_solve_factor_stage_f32_call_count,
        &output_device,
        &status_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_status_code_u32(status_device);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)status_device);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_lu_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    size_t* out_pivots,
    size_t* out_swap_count,
    void** out_device_ptr
) {
    if (out_swap_count == NULL || out_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_swap_count = 0;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > 0 && out_pivots == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t metadata_offset_count = element_count;
    size_t output_count = element_count + n;
    if (output_count > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 2u;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if ((n + 2u) > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanLuPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * sizeof(double),
        1,
        omni_tensor_vulkan_lu_f64_spv,
        omni_tensor_vulkan_lu_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_LU_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    size_t metadata_count = n + 2u;
    double* metadata = (double*)malloc(metadata_count * sizeof(double));
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        output_device,
        metadata_offset_count * sizeof(double),
        metadata_count * sizeof(double),
        metadata
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    double swap_payload = metadata[n];
    double status_payload = metadata[n + 1u];
    status = omni_tensor_backend_vulkan_tail_status_from_payload_f64(status_payload);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }
    if (!isfinite(swap_payload) || swap_payload < 0.0 || swap_payload > (double)n) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    size_t swap_count = (size_t)swap_payload;
    if ((double)swap_count != swap_payload) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }

    for (size_t i = 0; i < n; i++) {
        double pivot_payload = metadata[i];
        if (!isfinite(pivot_payload) || pivot_payload < 0.0 || pivot_payload >= (double)n) {
            free(metadata);
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
            return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
        size_t pivot = (size_t)pivot_payload;
        if ((double)pivot != pivot_payload) {
            free(metadata);
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
            return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
        out_pivots[i] = pivot;
    }

    free(metadata);
    *out_swap_count = swap_count;
    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_inverse_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count * 2u;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanInversePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * sizeof(double),
        1,
        omni_tensor_vulkan_inverse_f64_spv,
        omni_tensor_vulkan_inverse_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_INVERSE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f64(
        output_device,
        element_count * 2u * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_cholesky_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanCholeskyPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * sizeof(double),
        1,
        omni_tensor_vulkan_cholesky_f64_spv,
        omni_tensor_vulkan_cholesky_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_CHOLESKY_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f64(
        output_device,
        element_count * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_qr_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_q_device_ptr,
    void** out_r_device_ptr
) {
    if (out_q_device_ptr == NULL || out_r_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_q_device_ptr = NULL;
    *out_r_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows < cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t q_count = rows * cols;
    if (q_count > SIZE_MAX / sizeof(double) || byte_len != q_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (cols != 0 && cols > SIZE_MAX / cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_count = cols * cols;
    if (r_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_storage_count = r_count + 1u;
    if (r_storage_count > SIZE_MAX / sizeof(double)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (cols == 0 || q_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || q_count > UINT32_MAX || r_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanQrPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)q_count,
        (uint32_t)r_count
    };
    void* q_device = NULL;
    void* r_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_two_outputs(
        input_device_ptr,
        byte_len,
        q_count * sizeof(double),
        r_storage_count * sizeof(double),
        1,
        omni_tensor_vulkan_qr_f64_spv,
        omni_tensor_vulkan_qr_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_QR_LOCAL_SIZE,
        &q_device,
        &r_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f64(
        r_device,
        r_count * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)r_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)q_device);
        return status;
    }

    *out_q_device_ptr = q_device;
    *out_r_device_ptr = r_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_determinant_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    float* out_determinant
) {
    if (out_determinant == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_determinant = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) {
        *out_determinant = 1.0f;
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDeterminantPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * sizeof(float),
        1,
        omni_tensor_vulkan_determinant_f32_spv,
        omni_tensor_vulkan_determinant_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DETERMINANT_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    float determinant_payload = 0.0f;
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(determinant_payload), &determinant_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_determinant = determinant_payload;
    }
    return status;
}

int omni_tensor_backend_vulkan_solve_f32(
    const void* coefficients_device_ptr,
    size_t coefficients_byte_len,
    const void* rhs_device_ptr,
    size_t rhs_byte_len,
    size_t n,
    size_t rhs_cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n == 0 || rhs_cols == 0) return OMNI_TENSOR_VULKAN_SINGULAR;
    if (n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t coefficient_count = n * n;
    if (coefficient_count > SIZE_MAX / sizeof(float) || coefficients_byte_len != coefficient_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rhs_cols > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t result_count = n * rhs_cols;
    if (result_count > SIZE_MAX / sizeof(float) || rhs_byte_len != result_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n > UINT32_MAX || rhs_cols > UINT32_MAX || result_count > UINT32_MAX || coefficient_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (result_count > SIZE_MAX - coefficient_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = result_count + coefficient_count;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanSolvePushConstants push = {
        (uint32_t)n,
        (uint32_t)rhs_cols,
        (uint32_t)result_count,
        0
    };
    g_omni_tensor_vulkan_solve_serial_call_count++;
    g_omni_tensor_vulkan_solve_serial_f32_call_count++;
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
        coefficients_device_ptr,
        coefficients_byte_len,
        rhs_device_ptr,
        rhs_byte_len,
        output_count * sizeof(float),
        1,
        omni_tensor_vulkan_solve_f32_spv,
        omni_tensor_vulkan_solve_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SOLVE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f32(
        output_device,
        (result_count + coefficient_count) * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_lu_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    size_t* out_pivots,
    size_t* out_swap_count,
    void** out_device_ptr
) {
    if (out_swap_count == NULL || out_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_swap_count = 0;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > 16777216u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > 0 && out_pivots == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t metadata_offset_count = element_count;
    size_t output_count = element_count + n;
    if (output_count > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 2u;
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if ((n + 2u) > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanLuPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * sizeof(float),
        1,
        omni_tensor_vulkan_lu_f32_spv,
        omni_tensor_vulkan_lu_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_LU_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    size_t metadata_count = n + 2u;
    float* metadata = (float*)malloc(metadata_count * sizeof(float));
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        output_device,
        metadata_offset_count * sizeof(float),
        metadata_count * sizeof(float),
        metadata
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    float swap_payload = metadata[n];
    float status_payload = metadata[n + 1u];
    status = omni_tensor_backend_vulkan_tail_status_from_payload_f32(status_payload);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }
    if (!isfinite(swap_payload) || swap_payload < 0.0f || swap_payload > (float)n) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    size_t swap_count = (size_t)swap_payload;
    if ((float)swap_count != swap_payload) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }

    for (size_t i = 0; i < n; i++) {
        float pivot_payload = metadata[i];
        if (!isfinite(pivot_payload) || pivot_payload < 0.0f || pivot_payload >= (float)n) {
            free(metadata);
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
            return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
        size_t pivot = (size_t)pivot_payload;
        if ((float)pivot != pivot_payload) {
            free(metadata);
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
            return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
        out_pivots[i] = pivot;
    }

    free(metadata);
    *out_swap_count = swap_count;
    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_read_tail_status_complex128(
    void* device_ptr,
    size_t byte_offset
) {
    double status_payload[2] = {0.0, 0.0};
    int status = omni_tensor_backend_vulkan_copy_range_to_host(
        device_ptr,
        byte_offset,
        sizeof(status_payload),
        status_payload
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    return omni_tensor_backend_vulkan_tail_status_from_payload_f64(status_payload[0]);
}

static int omni_tensor_backend_vulkan_read_tail_status_complex64(
    void* device_ptr,
    size_t byte_offset
) {
    float status_payload[2] = {0.0f, 0.0f};
    int status = omni_tensor_backend_vulkan_copy_range_to_host(
        device_ptr,
        byte_offset,
        sizeof(status_payload),
        status_payload
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    return omni_tensor_backend_vulkan_tail_status_from_payload_f32(status_payload[0]);
}

int omni_tensor_backend_vulkan_determinant_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    double* out_real,
    double* out_imag
) {
    if (out_real == NULL || out_imag == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_real = 0.0;
    *out_imag = 0.0;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) {
        *out_real = 1.0;
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDeterminantPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_determinant_complex128_spv,
        omni_tensor_vulkan_determinant_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DETERMINANT_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    double determinant_payload[2] = {0.0, 0.0};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(determinant_payload), determinant_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_real = determinant_payload[0];
        *out_imag = determinant_payload[1];
    }
    return status;
}

int omni_tensor_backend_vulkan_determinant_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    float* out_real,
    float* out_imag
) {
    if (out_real == NULL || out_imag == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_real = 0.0f;
    *out_imag = 0.0f;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) {
        *out_real = 1.0f;
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanDeterminantPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_determinant_complex64_spv,
        omni_tensor_vulkan_determinant_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_DETERMINANT_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    float determinant_payload[2] = {0.0f, 0.0f};
    status = omni_tensor_backend_vulkan_copy_to_host(output_device, sizeof(determinant_payload), determinant_payload);
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
    if (status == OMNI_TENSOR_VULKAN_SUCCESS) {
        *out_real = determinant_payload[0];
        *out_imag = determinant_payload[1];
    }
    return status;
}

int omni_tensor_backend_vulkan_solve_complex128(
    const void* coefficients_device_ptr,
    size_t coefficients_byte_len,
    const void* rhs_device_ptr,
    size_t rhs_byte_len,
    size_t n,
    size_t rhs_cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n == 0 || rhs_cols == 0) return OMNI_TENSOR_VULKAN_SINGULAR;
    const size_t element_size = sizeof(double) * 2u;
    if (n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t coefficient_count = n * n;
    if (coefficient_count > SIZE_MAX / element_size || coefficients_byte_len != coefficient_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rhs_cols > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t result_count = n * rhs_cols;
    if (result_count > SIZE_MAX / element_size || rhs_byte_len != result_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n > UINT32_MAX || rhs_cols > UINT32_MAX || result_count > UINT32_MAX || coefficient_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (result_count > SIZE_MAX - coefficient_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = result_count + coefficient_count;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanSolvePushConstants push = {
        (uint32_t)n,
        (uint32_t)rhs_cols,
        (uint32_t)result_count,
        0
    };
    g_omni_tensor_vulkan_solve_serial_call_count++;
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_three_buffer_f64(
        coefficients_device_ptr,
        coefficients_byte_len,
        rhs_device_ptr,
        rhs_byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_solve_complex128_spv,
        omni_tensor_vulkan_solve_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SOLVE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex128(
        output_device,
        (result_count + coefficient_count) * element_size
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_solve_complex64(
    const void* coefficients_device_ptr,
    size_t coefficients_byte_len,
    const void* rhs_device_ptr,
    size_t rhs_byte_len,
    size_t n,
    size_t rhs_cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n == 0 || rhs_cols == 0) return OMNI_TENSOR_VULKAN_SINGULAR;
    const size_t element_size = sizeof(float) * 2u;
    if (n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t coefficient_count = n * n;
    if (coefficient_count > SIZE_MAX / element_size || coefficients_byte_len != coefficient_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rhs_cols > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t result_count = n * rhs_cols;
    if (result_count > SIZE_MAX / element_size || rhs_byte_len != result_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n > UINT32_MAX || rhs_cols > UINT32_MAX || result_count > UINT32_MAX || coefficient_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (result_count > SIZE_MAX - coefficient_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = result_count + coefficient_count;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanSolvePushConstants push = {
        (uint32_t)n,
        (uint32_t)rhs_cols,
        (uint32_t)result_count,
        0
    };
    g_omni_tensor_vulkan_solve_serial_call_count++;
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_three_buffer_f32(
        coefficients_device_ptr,
        coefficients_byte_len,
        rhs_device_ptr,
        rhs_byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_solve_complex64_spv,
        omni_tensor_vulkan_solve_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SOLVE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex64(
        output_device,
        (result_count + coefficient_count) * element_size
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_inverse_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count * 2u;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanInversePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_inverse_complex128_spv,
        omni_tensor_vulkan_inverse_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_INVERSE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex128(
        output_device,
        element_count * 2u * element_size
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_inverse_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count * 2u;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanInversePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_inverse_complex64_spv,
        omni_tensor_vulkan_inverse_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_INVERSE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex64(
        output_device,
        element_count * 2u * element_size
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_cholesky_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanCholeskyPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_cholesky_complex128_spv,
        omni_tensor_vulkan_cholesky_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_CHOLESKY_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex128(
        output_device,
        element_count * element_size
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_cholesky_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanCholeskyPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_cholesky_complex64_spv,
        omni_tensor_vulkan_cholesky_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_CHOLESKY_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex64(
        output_device,
        element_count * element_size
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_qr_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_q_device_ptr,
    void** out_r_device_ptr
) {
    if (out_q_device_ptr == NULL || out_r_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_q_device_ptr = NULL;
    *out_r_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows < cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    const size_t element_size = sizeof(double) * 2u;
    size_t q_count = rows * cols;
    if (q_count > SIZE_MAX / element_size || byte_len != q_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (cols != 0 && cols > SIZE_MAX / cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_count = cols * cols;
    if (r_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_storage_count = r_count + 1u;
    if (r_storage_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (cols == 0 || q_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || q_count > UINT32_MAX || r_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanQrPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)q_count,
        (uint32_t)r_count
    };
    void* q_device = NULL;
    void* r_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_two_outputs(
        input_device_ptr,
        byte_len,
        q_count * element_size,
        r_storage_count * element_size,
        1,
        omni_tensor_vulkan_qr_complex128_spv,
        omni_tensor_vulkan_qr_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_QR_LOCAL_SIZE,
        &q_device,
        &r_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex128(
        r_device,
        r_count * element_size
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)r_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)q_device);
        return status;
    }

    *out_q_device_ptr = q_device;
    *out_r_device_ptr = r_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_qr_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_q_device_ptr,
    void** out_r_device_ptr
) {
    if (out_q_device_ptr == NULL || out_r_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_q_device_ptr = NULL;
    *out_r_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows < cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    const size_t element_size = sizeof(float) * 2u;
    size_t q_count = rows * cols;
    if (q_count > SIZE_MAX / element_size || byte_len != q_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (cols != 0 && cols > SIZE_MAX / cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_count = cols * cols;
    if (r_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_storage_count = r_count + 1u;
    if (r_storage_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (cols == 0 || q_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || q_count > UINT32_MAX || r_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanQrPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)q_count,
        (uint32_t)r_count
    };
    void* q_device = NULL;
    void* r_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_two_outputs(
        input_device_ptr,
        byte_len,
        q_count * element_size,
        r_storage_count * element_size,
        1,
        omni_tensor_vulkan_qr_complex64_spv,
        omni_tensor_vulkan_qr_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_QR_LOCAL_SIZE,
        &q_device,
        &r_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_complex64(
        r_device,
        r_count * element_size
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)r_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)q_device);
        return status;
    }

    *out_q_device_ptr = q_device;
    *out_r_device_ptr = r_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_lu_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    size_t* out_pivots,
    size_t* out_swap_count,
    void** out_device_ptr
) {
    if (out_swap_count == NULL || out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_swap_count = 0;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > 0 && out_pivots == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    size_t element_count = n * n;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t metadata_offset_count = element_count;
    size_t output_count = element_count + n;
    if (output_count > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 2u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanLuPushConstants push = {(uint32_t)n, 0, 0, 0};
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_lu_complex128_spv,
        omni_tensor_vulkan_lu_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_LU_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    size_t metadata_count = n + 2u;
    double* metadata = (double*)malloc(metadata_count * element_size);
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        output_device,
        metadata_offset_count * element_size,
        metadata_count * element_size,
        metadata
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    double swap_payload = metadata[n * 2u];
    double status_payload = metadata[(n + 1u) * 2u];
    status = omni_tensor_backend_vulkan_tail_status_from_payload_f64(status_payload);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }
    if (!isfinite(swap_payload) || swap_payload < 0.0 || swap_payload > (double)n) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    size_t swap_count = (size_t)swap_payload;
    if ((double)swap_count != swap_payload) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    for (size_t i = 0; i < n; i++) {
        double pivot_payload = metadata[i * 2u];
        if (!isfinite(pivot_payload) || pivot_payload < 0.0 || pivot_payload >= (double)n) {
            free(metadata);
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
            return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
        size_t pivot = (size_t)pivot_payload;
        if ((double)pivot != pivot_payload) {
            free(metadata);
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
            return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
        out_pivots[i] = pivot;
    }

    free(metadata);
    *out_swap_count = swap_count;
    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_lu_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    size_t* out_pivots,
    size_t* out_swap_count,
    void** out_device_ptr
) {
    if (out_swap_count == NULL || out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_swap_count = 0;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > 16777216u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > 0 && out_pivots == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    size_t element_count = n * n;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t metadata_offset_count = element_count;
    size_t output_count = element_count + n;
    if (output_count > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 2u;
    if (output_count > SIZE_MAX / element_size) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanLuPushConstants push = {(uint32_t)n, 0, 0, 0};
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * element_size,
        1,
        omni_tensor_vulkan_lu_complex64_spv,
        omni_tensor_vulkan_lu_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_LU_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    size_t metadata_count = n + 2u;
    float* metadata = (float*)malloc(metadata_count * element_size);
    if (metadata == NULL) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
    }
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        output_device,
        metadata_offset_count * element_size,
        metadata_count * element_size,
        metadata
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    float swap_payload = metadata[n * 2u];
    float status_payload = metadata[(n + 1u) * 2u];
    status = omni_tensor_backend_vulkan_tail_status_from_payload_f32(status_payload);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }
    if (!isfinite(swap_payload) || swap_payload < 0.0f || swap_payload > (float)n) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    size_t swap_count = (size_t)swap_payload;
    if ((float)swap_count != swap_payload) {
        free(metadata);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    }
    for (size_t i = 0; i < n; i++) {
        float pivot_payload = metadata[i * 2u];
        if (!isfinite(pivot_payload) || pivot_payload < 0.0f || pivot_payload >= (float)n) {
            free(metadata);
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
            return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
        size_t pivot = (size_t)pivot_payload;
        if ((float)pivot != pivot_payload) {
            free(metadata);
            omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
            return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        }
        out_pivots[i] = pivot;
    }

    free(metadata);
    *out_swap_count = swap_count;
    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_inverse_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count * 2u;
    if (output_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    output_count += 1u;
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanInversePushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * sizeof(float),
        1,
        omni_tensor_vulkan_inverse_f32_spv,
        omni_tensor_vulkan_inverse_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_INVERSE_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f32(
        output_device,
        element_count * 2u * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_cholesky_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_count = element_count + 1u;
    if (output_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;

    OmniTensorVulkanCholeskyPushConstants push = {
        (uint32_t)n,
        0,
        0,
        0
    };
    void* output_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_count * sizeof(float),
        1,
        omni_tensor_vulkan_cholesky_f32_spv,
        omni_tensor_vulkan_cholesky_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_CHOLESKY_LOCAL_SIZE,
        &output_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f32(
        output_device,
        element_count * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output_device);
        return status;
    }

    *out_device_ptr = output_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_qr_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_q_device_ptr,
    void** out_r_device_ptr
) {
    if (out_q_device_ptr == NULL || out_r_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_q_device_ptr = NULL;
    *out_r_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows < cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t q_count = rows * cols;
    if (q_count > SIZE_MAX / sizeof(float) || byte_len != q_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (cols != 0 && cols > SIZE_MAX / cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_count = cols * cols;
    if (r_count > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t r_storage_count = r_count + 1u;
    if (r_storage_count > SIZE_MAX / sizeof(float)) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (cols == 0 || q_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || q_count > UINT32_MAX || r_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanQrPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)q_count,
        (uint32_t)r_count
    };
    void* q_device = NULL;
    void* r_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_two_outputs(
        input_device_ptr,
        byte_len,
        q_count * sizeof(float),
        r_storage_count * sizeof(float),
        1,
        omni_tensor_vulkan_qr_f32_spv,
        omni_tensor_vulkan_qr_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_QR_LOCAL_SIZE,
        &q_device,
        &r_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_tail_status_f32(
        r_device,
        r_count * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)r_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)q_device);
        return status;
    }

    *out_q_device_ptr = q_device;
    *out_r_device_ptr = r_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_singular_values_validate_shape_f64(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t* out_k,
    size_t* out_output_storage_count
) {
    if (out_k == NULL || out_output_storage_count == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_k = 0u;
    *out_output_storage_count = 0u;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t k = rows < cols ? rows : cols;
    *out_k = k;
    if (k == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || k > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (k > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t visible_output_count = k + 2u;
    if (k != 0u && k > SIZE_MAX / k) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t gram_count = k * k;
    if (visible_output_count > SIZE_MAX - gram_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_storage_count = visible_output_count + gram_count;
    if (output_storage_count > UINT32_MAX ||
        output_storage_count > SIZE_MAX / sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_output_storage_count = output_storage_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_values_oversized_index_guard_for_tests(void) {
    size_t rows = (size_t)UINT32_MAX;
    size_t cols = 2u;
    size_t element_count = rows * cols;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    return omni_tensor_backend_vulkan_singular_values_validate_shape_f64(
        element_count * sizeof(double),
        rows,
        cols,
        &k,
        &output_storage_count
    );
}

int omni_tensor_backend_vulkan_singular_norm_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    uint32_t mode,
    double* out_norm
);

int omni_tensor_backend_vulkan_singular_norm_zero_size_validation_probe_for_tests(void) {
    double out_norm = 123.0;
    int expected_status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!omni_tensor_backend_vulkan_available()) {
        expected_status = OMNI_TENSOR_VULKAN_UNAVAILABLE;
    } else if (!omni_tensor_backend_vulkan_float64_available()) {
        expected_status = OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    int status = omni_tensor_backend_vulkan_singular_norm_f64(
        NULL,
        0u,
        65u,
        65u,
        0u,
        &out_norm
    );
    if (status != expected_status) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_values_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_f64(
        byte_len,
        rows,
        cols,
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanSingularValuesPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)k,
        (uint32_t)output_storage_count
    };
    void* output = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_storage_count * sizeof(double),
        1u,
        omni_tensor_vulkan_singular_values_f64_spv,
        omni_tensor_vulkan_singular_values_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SINGULAR_VALUES_LOCAL_SIZE,
        &output
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_singular_values_status_f64(
        output,
        k * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output);
        return status;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t scalar_size,
    size_t* out_k,
    size_t* out_output_storage_count
) {
    if (out_k == NULL || out_output_storage_count == NULL || scalar_size == 0u) return OMNI_TENSOR_VULKAN_INVALID;
    *out_k = 0u;
    *out_output_storage_count = 0u;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t scalar_count = element_count * 2u;
    if (scalar_count > SIZE_MAX / scalar_size || byte_len != scalar_count * scalar_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t k = rows < cols ? rows : cols;
    *out_k = k;
    if (k == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX / 2u || cols > UINT32_MAX / 2u || k > UINT32_MAX / 2u || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (k > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t visible_output_count = k + 2u;
    if (k > SIZE_MAX / 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t real_k = k * 2u;
    if (real_k != 0u && real_k > ((size_t)UINT32_MAX / 64u) / real_k) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (real_k != 0u && real_k > SIZE_MAX / real_k) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t gram_count = real_k * real_k;
    if (real_k > SIZE_MAX - gram_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t hidden_count = real_k + gram_count;
    if (visible_output_count > SIZE_MAX - hidden_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_storage_count = visible_output_count + hidden_count;
    if (output_storage_count > UINT32_MAX ||
        output_storage_count > SIZE_MAX / scalar_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_output_storage_count = output_storage_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_values_complex_iteration_guard_for_tests(void) {
    size_t k = 0u;
    size_t output_storage_count = 0u;
    size_t rows = 4096u;
    size_t cols = 4096u;
    size_t element_count = rows * cols;
    int status128 = omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
        element_count * 2u * sizeof(double),
        rows,
        cols,
        sizeof(double),
        &k,
        &output_storage_count
    );
    int status64 = omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
        element_count * 2u * sizeof(float),
        rows,
        cols,
        sizeof(float),
        &k,
        &output_storage_count
    );
    if (status128 == OMNI_TENSOR_VULKAN_UNSUPPORTED && status64 == OMNI_TENSOR_VULKAN_UNSUPPORTED) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
}

int omni_tensor_backend_vulkan_singular_values_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
        byte_len,
        rows,
        cols,
        sizeof(double),
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanSingularValuesPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)k,
        (uint32_t)output_storage_count
    };
    void* output = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        output_storage_count * sizeof(double),
        1u,
        omni_tensor_vulkan_singular_values_complex128_spv,
        omni_tensor_vulkan_singular_values_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SINGULAR_VALUES_LOCAL_SIZE,
        &output
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_singular_values_status_f64(
        output,
        k * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output);
        return status;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_svd_validate_shape_f64(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t* out_k,
    size_t* out_u_count,
    size_t* out_u_storage_count,
    size_t* out_s_storage_count,
    size_t* out_v_count
) {
    if (out_k == NULL || out_u_count == NULL || out_u_storage_count == NULL ||
        out_s_storage_count == NULL || out_v_count == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_k = 0u;
    *out_u_count = 0u;
    *out_u_storage_count = 0u;
    *out_s_storage_count = 0u;
    *out_v_count = 0u;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t k = rows < cols ? rows : cols;
    *out_k = k;
    if (k == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || k > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rows != 0 && k > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t u_count = rows * k;
    if (cols != 0 && k > SIZE_MAX / cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t v_count = cols * k;
    if (u_count > UINT32_MAX || v_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (k != 0u && k > SIZE_MAX / k) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t gram_count = k * k;
    if (u_count > SIZE_MAX - gram_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t u_storage_count = u_count + gram_count;
    if (u_storage_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (k > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t s_storage_count = k + 1u;
    if (u_storage_count > SIZE_MAX / sizeof(double) ||
        s_storage_count > SIZE_MAX / sizeof(double) ||
        v_count > SIZE_MAX / sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_u_count = u_count;
    *out_u_storage_count = u_storage_count;
    *out_s_storage_count = s_storage_count;
    *out_v_count = v_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_svd_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_u_device_ptr,
    void** out_s_device_ptr,
    void** out_v_device_ptr
) {
    if (out_u_device_ptr == NULL || out_s_device_ptr == NULL || out_v_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_u_device_ptr = NULL;
    *out_s_device_ptr = NULL;
    *out_v_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t u_count = 0u;
    size_t u_storage_count = 0u;
    size_t s_storage_count = 0u;
    size_t v_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_svd_validate_shape_f64(
        byte_len,
        rows,
        cols,
        &k,
        &u_count,
        &u_storage_count,
        &s_storage_count,
        &v_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || u_count == 0u || u_storage_count == 0u || s_storage_count == 0u || v_count == 0u) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    OmniTensorVulkanSvdPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)k,
        (uint32_t)u_count,
        (uint32_t)s_storage_count,
        (uint32_t)v_count
    };
    void* u_device = NULL;
    void* s_device = NULL;
    void* v_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_three_outputs(
        input_device_ptr,
        byte_len,
        u_storage_count * sizeof(double),
        s_storage_count * sizeof(double),
        v_count * sizeof(double),
        1u,
        omni_tensor_vulkan_svd_f64_spv,
        omni_tensor_vulkan_svd_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SVD_LOCAL_SIZE,
        &u_device,
        &s_device,
        &v_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_singular_values_status_f64(
        s_device,
        k * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)v_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)s_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)u_device);
        return status;
    }

    *out_u_device_ptr = u_device;
    *out_s_device_ptr = s_device;
    *out_v_device_ptr = v_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_symmetric_eigen_validate_shape_f64(
    size_t byte_len,
    size_t n,
    size_t* out_value_storage_count,
    size_t* out_vector_count,
    size_t* out_vector_storage_count
) {
    if (out_value_storage_count == NULL || out_vector_count == NULL ||
        out_vector_storage_count == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_value_storage_count = 0u;
    *out_vector_count = 0u;
    *out_vector_storage_count = 0u;
    if (n != 0 && n > SIZE_MAX / n) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = n * n;
    if (element_count > SIZE_MAX / sizeof(double) || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (n == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (n > UINT32_MAX || element_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (element_count > UINT32_MAX / 64u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (n > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t value_storage_count = n + 1u;
    if (element_count > SIZE_MAX - element_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t vector_storage_count = element_count + element_count;
    if (value_storage_count > UINT32_MAX || vector_storage_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (value_storage_count > SIZE_MAX / sizeof(double) ||
        vector_storage_count > SIZE_MAX / sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_value_storage_count = value_storage_count;
    *out_vector_count = element_count;
    *out_vector_storage_count = vector_storage_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_symmetric_eigen_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t n,
    void** out_values_device_ptr,
    void** out_vectors_device_ptr
) {
    if (out_values_device_ptr == NULL || out_vectors_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_values_device_ptr = NULL;
    *out_vectors_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t value_storage_count = 0u;
    size_t vector_count = 0u;
    size_t vector_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_symmetric_eigen_validate_shape_f64(
        byte_len,
        n,
        &value_storage_count,
        &vector_count,
        &vector_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (n == 0 || value_storage_count == 0u || vector_count == 0u || vector_storage_count == 0u) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    OmniTensorVulkanSymmetricEigenPushConstants push = {
        (uint32_t)n,
        (uint32_t)value_storage_count,
        (uint32_t)vector_count,
        0u
    };
    void* values_device = NULL;
    void* vectors_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_two_outputs(
        input_device_ptr,
        byte_len,
        value_storage_count * sizeof(double),
        vector_storage_count * sizeof(double),
        1u,
        omni_tensor_vulkan_symmetric_eigen_f64_spv,
        omni_tensor_vulkan_symmetric_eigen_f64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SYMMETRIC_EIGEN_LOCAL_SIZE,
        &values_device,
        &vectors_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_symmetric_eigen_status_f64(
        values_device,
        n * sizeof(double)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)vectors_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)values_device);
        return status;
    }

    *out_values_device_ptr = values_device;
    *out_vectors_device_ptr = vectors_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_norm_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    uint32_t mode,
    double* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0;
    if (mode > 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_f64(
        byte_len,
        rows,
        cols,
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;
    void* singular_values = NULL;
    int status = omni_tensor_backend_vulkan_singular_values_f64(
        input_device_ptr,
        byte_len,
        rows,
        cols,
        &singular_values
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (singular_values == NULL) return OMNI_TENSOR_VULKAN_SUCCESS;

    size_t read_offset = mode == 0u ? 0u : (k + 1u) * sizeof(double);
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        singular_values,
        read_offset,
        sizeof(double),
        out_norm
    );
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)singular_values);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (!isfinite(*out_norm)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_norm_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    uint32_t mode,
    double* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0;
    if (mode > 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
        byte_len,
        rows,
        cols,
        sizeof(double),
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;
    void* singular_values = NULL;
    int status = omni_tensor_backend_vulkan_singular_values_complex128(
        input_device_ptr,
        byte_len,
        rows,
        cols,
        &singular_values
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (singular_values == NULL) return OMNI_TENSOR_VULKAN_SUCCESS;

    size_t read_offset = mode == 0u ? 0u : (k + 1u) * sizeof(double);
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        singular_values,
        read_offset,
        sizeof(double),
        out_norm
    );
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)singular_values);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (!isfinite(*out_norm)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_singular_values_validate_shape_f32(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t* out_k,
    size_t* out_output_storage_count
) {
    if (out_k == NULL || out_output_storage_count == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_k = 0u;
    *out_output_storage_count = 0u;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t k = rows < cols ? rows : cols;
    *out_k = k;
    if (k == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || k > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (k > SIZE_MAX - 2u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t visible_output_count = k + 2u;
    if (k != 0u && k > SIZE_MAX / k) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t gram_count = k * k;
    if (visible_output_count > SIZE_MAX - gram_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t output_storage_count = visible_output_count + gram_count;
    if (output_storage_count > UINT32_MAX ||
        output_storage_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_output_storage_count = output_storage_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_values_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_f32(
        byte_len,
        rows,
        cols,
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanSingularValuesPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)k,
        (uint32_t)output_storage_count
    };
    void* output = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_storage_count * sizeof(float),
        1u,
        omni_tensor_vulkan_singular_values_f32_spv,
        omni_tensor_vulkan_singular_values_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SINGULAR_VALUES_LOCAL_SIZE,
        &output
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_singular_values_status_f32(
        output,
        k * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output);
        return status;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_values_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
        byte_len,
        rows,
        cols,
        sizeof(float),
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;

    OmniTensorVulkanSingularValuesPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)k,
        (uint32_t)output_storage_count
    };
    void* output = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        output_storage_count * sizeof(float),
        1u,
        omni_tensor_vulkan_singular_values_complex64_spv,
        omni_tensor_vulkan_singular_values_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SINGULAR_VALUES_LOCAL_SIZE,
        &output
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_singular_values_status_f32(
        output,
        k * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)output);
        return status;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

static int omni_tensor_backend_vulkan_svd_validate_shape_f32(
    size_t byte_len,
    size_t rows,
    size_t cols,
    size_t* out_k,
    size_t* out_u_count,
    size_t* out_u_storage_count,
    size_t* out_s_storage_count,
    size_t* out_v_count
) {
    if (out_k == NULL || out_u_count == NULL || out_u_storage_count == NULL ||
        out_s_storage_count == NULL || out_v_count == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_k = 0u;
    *out_u_count = 0u;
    *out_u_storage_count = 0u;
    *out_s_storage_count = 0u;
    *out_v_count = 0u;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    size_t k = rows < cols ? rows : cols;
    *out_k = k;
    if (k == 0 || element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (rows > UINT32_MAX || cols > UINT32_MAX || k > UINT32_MAX || element_count > UINT32_MAX) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    if (rows != 0 && k > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t u_count = rows * k;
    if (cols != 0 && k > SIZE_MAX / cols) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t v_count = cols * k;
    if (u_count > UINT32_MAX || v_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (k != 0u && k > SIZE_MAX / k) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t gram_count = k * k;
    if (u_count > SIZE_MAX - gram_count) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t u_storage_count = u_count + gram_count;
    if (u_storage_count > UINT32_MAX) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (k > SIZE_MAX - 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t s_storage_count = k + 1u;
    if (u_storage_count > SIZE_MAX / sizeof(float) ||
        s_storage_count > SIZE_MAX / sizeof(float) ||
        v_count > SIZE_MAX / sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }
    *out_u_count = u_count;
    *out_u_storage_count = u_storage_count;
    *out_s_storage_count = s_storage_count;
    *out_v_count = v_count;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_svd_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_u_device_ptr,
    void** out_s_device_ptr,
    void** out_v_device_ptr
) {
    if (out_u_device_ptr == NULL || out_s_device_ptr == NULL || out_v_device_ptr == NULL) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    *out_u_device_ptr = NULL;
    *out_s_device_ptr = NULL;
    *out_v_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t u_count = 0u;
    size_t u_storage_count = 0u;
    size_t s_storage_count = 0u;
    size_t v_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_svd_validate_shape_f32(
        byte_len,
        rows,
        cols,
        &k,
        &u_count,
        &u_storage_count,
        &s_storage_count,
        &v_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || u_count == 0u || u_storage_count == 0u || s_storage_count == 0u || v_count == 0u) {
        return OMNI_TENSOR_VULKAN_SUCCESS;
    }

    OmniTensorVulkanSvdPushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)k,
        (uint32_t)u_count,
        (uint32_t)s_storage_count,
        (uint32_t)v_count
    };
    void* u_device = NULL;
    void* s_device = NULL;
    void* v_device = NULL;
    int status = omni_tensor_backend_vulkan_dispatch_one_input_three_outputs(
        input_device_ptr,
        byte_len,
        u_storage_count * sizeof(float),
        s_storage_count * sizeof(float),
        v_count * sizeof(float),
        1u,
        omni_tensor_vulkan_svd_f32_spv,
        omni_tensor_vulkan_svd_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_SVD_LOCAL_SIZE,
        &u_device,
        &s_device,
        &v_device
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    status = omni_tensor_backend_vulkan_read_singular_values_status_f32(
        s_device,
        k * sizeof(float)
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)v_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)s_device);
        omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)u_device);
        return status;
    }

    *out_u_device_ptr = u_device;
    *out_s_device_ptr = s_device;
    *out_v_device_ptr = v_device;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_norm_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    uint32_t mode,
    float* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0f;
    if (mode > 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_f32(
        byte_len,
        rows,
        cols,
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;
    void* singular_values = NULL;
    int status = omni_tensor_backend_vulkan_singular_values_f32(
        input_device_ptr,
        byte_len,
        rows,
        cols,
        &singular_values
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (singular_values == NULL) return OMNI_TENSOR_VULKAN_SUCCESS;

    size_t read_offset = mode == 0u ? 0u : (k + 1u) * sizeof(float);
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        singular_values,
        read_offset,
        sizeof(float),
        out_norm
    );
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)singular_values);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (!isfinite(*out_norm)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_singular_norm_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    uint32_t mode,
    float* out_norm
) {
    if (out_norm == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_norm = 0.0f;
    if (mode > 1u) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t k = 0u;
    size_t output_storage_count = 0u;
    int validation_status = omni_tensor_backend_vulkan_singular_values_validate_shape_complex(
        byte_len,
        rows,
        cols,
        sizeof(float),
        &k,
        &output_storage_count
    );
    if (validation_status != OMNI_TENSOR_VULKAN_SUCCESS) return validation_status;
    if (k == 0 || output_storage_count == 0u) return OMNI_TENSOR_VULKAN_SUCCESS;
    void* singular_values = NULL;
    int status = omni_tensor_backend_vulkan_singular_values_complex64(
        input_device_ptr,
        byte_len,
        rows,
        cols,
        &singular_values
    );
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (singular_values == NULL) return OMNI_TENSOR_VULKAN_SUCCESS;

    size_t read_offset = mode == 0u ? 0u : (k + 1u) * sizeof(float);
    status = omni_tensor_backend_vulkan_copy_range_to_host(
        singular_values,
        read_offset,
        sizeof(float),
        out_norm
    );
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)singular_values);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;
    if (!isfinite(*out_norm)) return OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_transpose_f32(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (element_count > UINT32_MAX || rows > UINT32_MAX || cols > UINT32_MAX ||
        element_count > SIZE_MAX / sizeof(float) || byte_len != element_count * sizeof(float)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanTransposePushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)element_count,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_transpose_f32_spv,
        omni_tensor_vulkan_transpose_f32_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_TRANSPOSE_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_transpose_f64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (input_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    if (element_count > UINT32_MAX || rows > UINT32_MAX || cols > UINT32_MAX || byte_len != element_count * sizeof(double)) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanBuffer* input = (OmniTensorVulkanBuffer*)input_device_ptr;
    OmniTensorVulkanContext* context = input->context;
    if (context == NULL || context->device == NULL || input->buffer == NULL || input->byte_len < byte_len) {
        return OMNI_TENSOR_VULKAN_INVALID;
    }
    OmniVulkanDevice device = context->device;

    OmniTensorVulkanBuffer* output = NULL;
    int status = omni_tensor_backend_vulkan_create_buffer_on_context(context, byte_len, &output);
    if (status != OMNI_TENSOR_VULKAN_SUCCESS) return status;

    OmniVulkanQueue queue = NULL;
    OmniVulkanShaderModule shader_module = NULL;
    OmniVulkanDescriptorSetLayout descriptor_set_layout = NULL;
    OmniVulkanDescriptorPool descriptor_pool = NULL;
    OmniVulkanDescriptorSet descriptor_set = NULL;
    OmniVulkanPipelineLayout pipeline_layout = NULL;
    OmniVulkanPipeline pipeline = NULL;
    OmniVulkanCommandPool command_pool = NULL;
    OmniVulkanCommandBuffer command_buffer = NULL;
    int result = OMNI_TENSOR_VULKAN_SUCCESS;

    omni_vulkan_get_device_queue(device, context->queue_family_index, 0, &queue);
    if (queue == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetLayoutBinding bindings[2] = {
        {0, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL},
        {1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, NULL}
    };
    OmniVulkanDescriptorSetLayoutCreateInfo layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
        NULL,
        0,
        2,
        bindings
    };
    if (omni_vulkan_create_descriptor_set_layout(device, &layout_info, NULL, &descriptor_set_layout) != OMNI_VULKAN_SUCCESS || descriptor_set_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanPushConstantRange push_range = {
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        0,
        (uint32_t)sizeof(OmniTensorVulkanTransposePushConstants)
    };
    OmniVulkanPipelineLayoutCreateInfo pipeline_layout_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
        NULL,
        0,
        1,
        &descriptor_set_layout,
        1,
        &push_range
    };
    if (omni_vulkan_create_pipeline_layout(device, &pipeline_layout_info, NULL, &pipeline_layout) != OMNI_VULKAN_SUCCESS || pipeline_layout == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanShaderModuleCreateInfo shader_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
        NULL,
        0,
        omni_tensor_vulkan_transpose_f64_spv_size,
        omni_tensor_vulkan_transpose_f64_spv
    };
    int saved_stderr = -1;
    int stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult shader_result = omni_vulkan_create_shader_module(device, &shader_info, NULL, &shader_module);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (shader_result != OMNI_VULKAN_SUCCESS || shader_module == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanPipelineShaderStageCreateInfo stage_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
        NULL,
        0,
        OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT,
        shader_module,
        "main",
        NULL
    };
    OmniVulkanComputePipelineCreateInfo pipeline_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
        NULL,
        0,
        stage_info,
        pipeline_layout,
        NULL,
        -1
    };
    saved_stderr = -1;
    stderr_silenced = omni_tensor_vulkan_silence_stderr_begin(&saved_stderr);
    OmniVulkanResult pipeline_result = omni_vulkan_create_compute_pipelines(device, NULL, 1, &pipeline_info, NULL, &pipeline);
    if (stderr_silenced) omni_tensor_vulkan_silence_stderr_end(saved_stderr);
    if (pipeline_result != OMNI_VULKAN_SUCCESS || pipeline == NULL) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorPoolSize pool_size = { OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, 2 };
    OmniVulkanDescriptorPoolCreateInfo pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
        NULL,
        0,
        1,
        1,
        &pool_size
    };
    if (omni_vulkan_create_descriptor_pool(device, &pool_info, NULL, &descriptor_pool) != OMNI_VULKAN_SUCCESS || descriptor_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorSetAllocateInfo set_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
        NULL,
        descriptor_pool,
        1,
        &descriptor_set_layout
    };
    if (omni_vulkan_allocate_descriptor_sets(device, &set_info, &descriptor_set) != OMNI_VULKAN_SUCCESS || descriptor_set == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanDescriptorBufferInfo input_info = { input->buffer, 0, (OmniVulkanDeviceSize)byte_len };
    OmniVulkanDescriptorBufferInfo output_info = { output->buffer, 0, (OmniVulkanDeviceSize)byte_len };
    OmniVulkanWriteDescriptorSet writes[2] = {
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 0, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &input_info, NULL },
        { OMNI_VULKAN_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET, NULL, descriptor_set, 1, 0, 1, OMNI_VULKAN_DESCRIPTOR_TYPE_STORAGE_BUFFER, NULL, &output_info, NULL }
    };
    omni_vulkan_update_descriptor_sets(device, 2, writes, 0, NULL);

    OmniVulkanCommandPoolCreateInfo command_pool_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
        NULL,
        0,
        context->queue_family_index
    };
    if (omni_vulkan_create_command_pool(device, &command_pool_info, NULL, &command_pool) != OMNI_VULKAN_SUCCESS || command_pool == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferAllocateInfo command_buffer_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
        NULL,
        command_pool,
        OMNI_VULKAN_COMMAND_BUFFER_LEVEL_PRIMARY,
        1
    };
    if (omni_vulkan_allocate_command_buffers(device, &command_buffer_info, &command_buffer) != OMNI_VULKAN_SUCCESS || command_buffer == NULL) {
        result = OMNI_TENSOR_VULKAN_ALLOCATION_FAILED;
        goto cleanup;
    }

    OmniVulkanCommandBufferBeginInfo begin_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
        NULL,
        OMNI_VULKAN_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
        NULL
    };
    if (omni_vulkan_begin_command_buffer(command_buffer, &begin_info) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniTensorVulkanTransposePushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)element_count,
        0
    };
    omni_vulkan_cmd_bind_pipeline(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline);
    omni_vulkan_cmd_bind_descriptor_sets(command_buffer, OMNI_VULKAN_PIPELINE_BIND_POINT_COMPUTE, pipeline_layout, 0, 1, &descriptor_set, 0, NULL);
    omni_vulkan_cmd_push_constants(command_buffer, pipeline_layout, OMNI_VULKAN_SHADER_STAGE_COMPUTE_BIT, 0, (uint32_t)sizeof(push), &push);
    uint32_t group_count = ((uint32_t)element_count + OMNI_TENSOR_VULKAN_TRANSPOSE_LOCAL_SIZE - 1u) / OMNI_TENSOR_VULKAN_TRANSPOSE_LOCAL_SIZE;
    omni_vulkan_cmd_dispatch(command_buffer, group_count, 1, 1);
    if (omni_vulkan_end_command_buffer(command_buffer) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

    OmniVulkanSubmitInfo submit_info = {
        OMNI_VULKAN_STRUCTURE_TYPE_SUBMIT_INFO,
        NULL,
        0,
        NULL,
        NULL,
        1,
        &command_buffer,
        0,
        NULL
    };
    if (omni_vulkan_queue_submit(queue, 1, &submit_info, NULL) != OMNI_VULKAN_SUCCESS ||
        omni_vulkan_queue_wait_idle(queue) != OMNI_VULKAN_SUCCESS) {
        result = OMNI_TENSOR_VULKAN_EXECUTION_FAILED;
        goto cleanup;
    }

cleanup:
    if (command_pool != NULL) omni_vulkan_destroy_command_pool(device, command_pool, NULL);
    if (descriptor_pool != NULL) omni_vulkan_destroy_descriptor_pool(device, descriptor_pool, NULL);
    if (pipeline != NULL) omni_vulkan_destroy_pipeline(device, pipeline, NULL);
    if (pipeline_layout != NULL) omni_vulkan_destroy_pipeline_layout(device, pipeline_layout, NULL);
    if (descriptor_set_layout != NULL) omni_vulkan_destroy_descriptor_set_layout(device, descriptor_set_layout, NULL);
    if (shader_module != NULL) omni_vulkan_destroy_shader_module(device, shader_module, NULL);
    if (result != OMNI_TENSOR_VULKAN_SUCCESS) {
        omni_tensor_backend_vulkan_destroy_buffer_handle(output);
        return result;
    }

    *out_device_ptr = output;
    return OMNI_TENSOR_VULKAN_SUCCESS;
}

int omni_tensor_backend_vulkan_transpose_complex128(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float64_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(double) * 2u;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (element_count > UINT32_MAX || rows > UINT32_MAX || cols > UINT32_MAX ||
        element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanTransposePushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)element_count,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f64(
        input_device_ptr,
        byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_transpose_complex128_spv,
        omni_tensor_vulkan_transpose_complex128_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_TRANSPOSE_LOCAL_SIZE,
        out_device_ptr
    );
}

int omni_tensor_backend_vulkan_transpose_complex64(
    const void* input_device_ptr,
    size_t byte_len,
    size_t rows,
    size_t cols,
    void** out_device_ptr
) {
    if (out_device_ptr == NULL) return OMNI_TENSOR_VULKAN_INVALID;
    *out_device_ptr = NULL;
    if (!omni_tensor_backend_vulkan_available()) return OMNI_TENSOR_VULKAN_UNAVAILABLE;
    if (!omni_tensor_backend_vulkan_float32_available()) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    if (rows != 0 && cols > SIZE_MAX / rows) return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    size_t element_count = rows * cols;
    const size_t element_size = sizeof(float) * 2u;
    if (element_count == 0 || byte_len == 0) return OMNI_TENSOR_VULKAN_SUCCESS;
    if (element_count > UINT32_MAX || rows > UINT32_MAX || cols > UINT32_MAX ||
        element_count > SIZE_MAX / element_size || byte_len != element_count * element_size) {
        return OMNI_TENSOR_VULKAN_UNSUPPORTED;
    }

    OmniTensorVulkanTransposePushConstants push = {
        (uint32_t)rows,
        (uint32_t)cols,
        (uint32_t)element_count,
        0
    };
    return omni_tensor_backend_vulkan_dispatch_two_buffer_f32(
        input_device_ptr,
        byte_len,
        byte_len,
        element_count,
        omni_tensor_vulkan_transpose_complex64_spv,
        omni_tensor_vulkan_transpose_complex64_spv_size,
        &push,
        sizeof(push),
        OMNI_TENSOR_VULKAN_TRANSPOSE_LOCAL_SIZE,
        out_device_ptr
    );
}

void omni_tensor_backend_vulkan_free(void* device_ptr) {
    if (device_ptr == NULL) return;
    omni_tensor_backend_vulkan_destroy_buffer_handle((OmniTensorVulkanBuffer*)device_ptr);
}

void* omni_tensor_backend_vulkan_retain(void* device_ptr) {
    if (device_ptr == NULL) return NULL;
    OmniTensorVulkanBuffer* handle = (OmniTensorVulkanBuffer*)device_ptr;
    if (handle->ref_count < UINT32_MAX) handle->ref_count++;
    return handle;
}
