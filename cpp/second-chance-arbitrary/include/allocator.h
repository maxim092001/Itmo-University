#pragma once

#include "pool.h"

#include <cstdint>

class AllocatorWithPool : private PoolAllocator {
public:
    using PoolAllocator::pointer;

    AllocatorWithPool(const std::size_t size)
            : PoolAllocator(size) {}

    template<class T, class... Args>
    pointer create(Args &&... args) {
        auto ptr = allocate(sizeof(T));
        new(*ptr) T(std::forward<Args>(args)...);
        return ptr;
    }

    template<class T>
    void destroy(const pointer ptr) {
        static_cast<T *>(*ptr)->~T();
        deallocate(ptr);
    }
};
