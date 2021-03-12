#pragma once

#include <cstddef>
#include <cstdint>
#include <functional>
#include <map>
#include <new>
#include <vector>

class PoolAllocator {
    struct Chunk {
        std::size_t pos;
        std::size_t size;
        std::size_t size_unaligned;

        Chunk(std::size_t pos = static_cast<std::size_t>(-1),
              std::size_t size = 0,
              std::size_t size_unaligned = 0)
                : pos(pos), size(size), size_unaligned(size_unaligned) {}
    };

    void *resolve(const std::intptr_t ptr_value) {
        auto chunk_it = m_chunks.find(ptr_value);
        if (chunk_it == m_chunks.end()) {
            return nullptr;
        }
        return &m_data[chunk_it->second.pos];
    }

public:
    class pointer {
        std::reference_wrapper<PoolAllocator> m_pool;
        std::intptr_t m_ptr_value;

        friend class PoolAllocator;

        pointer(PoolAllocator &pool, const std::intptr_t ptr_value)
                : m_pool(pool), m_ptr_value(ptr_value) {}

    public:
        void *operator*() const {
            return m_pool.get().resolve(m_ptr_value);
        }
    };

    PoolAllocator(const std::size_t size) {
        init(size);
    }

    pointer allocate(const std::size_t unaligned);

    void deallocate(const pointer ptr);

private:
    void defragmentation();
    void init(const std::size_t size) {
        m_data.resize(size * 8);
        m_used.assign(size * 8, false);
        m_chunk_max_size = m_used_size = m_used_unaligned = 0;
        m_size_unaligned = size;
        m_size = size * 8;
    }
    std::map<int, Chunk> m_chunks;
    std::vector<std::byte> m_data;
    std::vector<bool> m_used;

    std::size_t m_used_size;
    std::size_t m_used_unaligned;

    std::size_t m_size_unaligned;
    std::size_t m_size;

    std::size_t m_chunk_max_size;

};
