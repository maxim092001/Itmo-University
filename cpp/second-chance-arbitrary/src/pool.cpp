#include "pool.h"

#include <cstddef>
#include <new>
#include <vector>

PoolAllocator::pointer PoolAllocator::allocate(const std::size_t unaligned) {
    std::size_t size = ((unaligned + 7) / 8) * 8;
    std::size_t idx;

    for (idx = 0; idx < m_size; ++idx) {
        if (!m_used[idx]) {
            std::size_t k = size;
            std::size_t j = idx;
            for (; k > 0 && j < m_size; ++j, --k) {
                if (m_used[j]) {
                    break;
                }
            }
            if (k == 0) {
                break;
            } else {
                idx = j - 1;
            }
        }
    }

    if (m_used_unaligned > m_size_unaligned - unaligned ||
        m_size_unaligned < unaligned) {
        throw std::bad_alloc{};
    }

    if (idx == m_size) {
        defragmentation();
        idx = m_used_size;
    }

    m_used_unaligned += unaligned;
    m_used_size += size;
    m_chunks[m_chunk_max_size] = Chunk(idx, size, unaligned);

    for (std::size_t j = 0; j < size; ++j) {
        m_used[idx + j] = true;
    }

    return pointer(*this, m_chunk_max_size++);
}

void PoolAllocator::deallocate(const pointer ptr) {
    std::size_t chunk = ptr.m_ptr_value;
    auto it = m_chunks.find(chunk);
    for (std::size_t i = 0; i < it->second.size; ++i) {
        m_used[it->second.pos + i] = false;
    }
    m_used_unaligned -= it->second.size_unaligned;
    m_used_size -= it->second.size;
    m_chunks.erase(it);
}

void PoolAllocator::defragmentation() {
    for (auto &chunk : m_chunks) {
        std::size_t size = chunk.second.size;
        std::size_t &idx = chunk.second.pos;
        std::size_t offset = 0;
        for (; idx > 0 && !m_used[idx - 1];) {
            --idx;
            ++offset;
        }
        --idx;
        for (std::size_t q = 0; q < size; ++q) {
            swap(m_data[idx + q], m_data[idx + q + offset]);
            swap(m_used[idx + q], m_used[idx + q + offset]);
        }
    }
}
