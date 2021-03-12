#pragma once

#include <list>
#include <string_view>
#include <set>
#include <map>
#include <vector>

namespace genome {

// one possible starting point
//

    using Node = std::string_view;

    class Edge {
    public:
        Edge(Node from, Node to);

        const auto &from() const { return m_from; }

        const auto &to() const { return m_to; }

        bool operator==(const Edge &other) const;

    private:
        Node m_from;
        Node m_to;
        size_t m_id;
        static size_t id;
    };

    class Graph {
    public:
        void add_edge(Edge edge);

        std::vector<Edge> find_euler_path();

    private:
        std::map<Node, std::multiset<Node>> g;
    };

} // namespace genome
