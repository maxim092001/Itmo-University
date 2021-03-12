#pragma once

#include <cstddef>
#include <deque>
#include <new>
#include <ostream>
#include <list>

template<class Key, class KeyProvider, class Allocator>
class Cache {
public:
    template<class... AllocArgs>
    Cache(const std::size_t cache_size, AllocArgs &&... alloc_args)
            : m_max_size(cache_size), m_alloc(std::forward<AllocArgs>(alloc_args)...) {}

    std::size_t size() const { return m_queue.size(); }

    bool empty() const { return m_queue.empty(); }

    template<class T>
    T &get(const Key &key);

    std::ostream &print(std::ostream &strm) const;

    friend std::ostream &operator<<(std::ostream &strm, const Cache &cache) {
        return cache.print(strm);
    }

private:
    const std::size_t m_max_size;
    Allocator m_alloc;

    struct Pair {
        typename Allocator::pointer ptr;
        bool used = false;

        Pair(typename Allocator::pointer ptr)
                : ptr(ptr) {}
    };

    std::list<Pair> m_queue;
};

template<class Key, class KeyProvider, class Allocator>
template<class T>
inline T &Cache<Key, KeyProvider, Allocator>::get(const Key &key) {
    for (Pair &element : m_queue) {
        if (*static_cast<KeyProvider *>(*element.ptr) == key) {
            element.used = true;
            return *static_cast<T *>(*element.ptr);
        }
    }
    while (m_queue.size() >= m_max_size) {
        Pair e = m_queue.front();
        m_queue.pop_front();
        if (e.used) {
            e.used = false;
            m_queue.emplace_back(e);
        } else {
            m_alloc.template destroy<KeyProvider>(e.ptr);
        }
    }
    m_queue.emplace_back(Pair(m_alloc.template create<T>(key)));
    return *static_cast<T *>(*m_queue.back().ptr);
}

template<class Key, class KeyProvider, class Allocator>
inline std::ostream &Cache<Key, KeyProvider, Allocator>::print(std::ostream &strm) const {
    return strm << "<empty>\n";
}
