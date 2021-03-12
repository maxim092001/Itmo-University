#include "genome.h"
#include "graph.h"

namespace genome {

    std::string generate_string(const std::vector<Edge> & euler_path) {
        std::string ans;
        ans = euler_path[0].from();
        for (const auto & i : euler_path) {
            ans += i.to().back();
        }
        return ans;
    }

    std::string assembly(size_t k, const std::vector<std::string> & input) {
        if (k == 0 || input.empty()) {
            return "";
        }
        Graph g = Graph();

        for (const auto &i : input) {
            std::string_view view(i);
            for (size_t j = 0; j + k < i.size(); j++) {
                std::string_view temp = view.substr(j, k + 1);
                g.add_edge(Edge(temp.substr(0, k), temp.substr(1, k)));
            }
        }
        return generate_string(g.find_euler_path());
    }
}
