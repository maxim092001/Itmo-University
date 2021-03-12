#include "graph.h"
#include <cmath>
#include <stack>
#include <vector>
#include <algorithm>

namespace genome {

    size_t Edge::id = 0;

    Edge::Edge(Node from, Node to)
            : m_from(from), m_to(to), m_id(id++) {
    }

    bool Edge::operator==(const Edge &other) const {
        return m_from == other.m_from && m_to == other.m_to && m_id == other.m_id;
    }

    void Graph::add_edge(Edge edge) {
        g[edge.from()].insert(edge.to());
    }

    std::vector<Edge> Graph::find_euler_path() {
        std::map<Node, int> incoming;
        for (const auto & t : g) {
            for (const auto & node : t.second) {
                incoming[node]++;
            }
        }

        Node start_node;

        for (const auto & t : g) {
            if (t.second.size() - incoming[t.first] == 1) {
                start_node = t.first;
                break;
            }
        }

        std::stack<Node> stack;
        stack.push(start_node);
        std::vector<Node> nodes_on_path;

        while (!stack.empty()) {
            Node v = stack.top();
            std::multiset<Node>& set = g[v];
            if (!set.empty()) {
                stack.push(*set.begin());
                set.erase(set.begin());
            } else {
                nodes_on_path.emplace_back(v);
                stack.pop();
            }
        }

        std::reverse(nodes_on_path.begin(), nodes_on_path.end());

        std::vector<Edge> edge_path;

        for (size_t i = 0; i < nodes_on_path.size() - 1; ++i) {
            edge_path.emplace_back(nodes_on_path[i], nodes_on_path[i + 1]);
        }

        return edge_path;
    }

} // namespace genome
