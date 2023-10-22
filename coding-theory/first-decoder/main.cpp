#include <algorithm>
#include <cassert>
#include <cmath>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <map>
#include <numeric>
#include <optional>
#include <random>
#include <set>
#include <unordered_set>
#include <vector>

using namespace std;

typedef vector<vector<bool>> matrix;
typedef vector<pair<int, int>> span;
typedef set<int, greater<>> greater_set;

random_device rd{};
mt19937 rng{rd()};

namespace utils {

double MAX_VALUE = -1e18;

enum class Command { Decode, Encode, Simulate };

Command commandFromString(const string &s) {
  if (s == "Decode")
    return Command::Decode;
  else if (s == "Encode")
    return Command::Encode;
  else
    return Command::Simulate;
}

void generate_vector(const int n, const int i, vector<bool> &base_vector,
                     vector<vector<bool>> &result) {

  if (i == n) {
    result.emplace_back(base_vector);
    return;
  }

  base_vector[i] = false;
  generate_vector(n, i + 1, base_vector, result);

  base_vector[i] = true;
  generate_vector(n, i + 1, base_vector, result);
}

void generate_combinations(const int size, vector<vector<bool>> &result) {

  if (size == 0) {
    result = {};
  }

  vector<bool> zeros(size);
  generate_vector(size, 0, zeros, result);
}

vector<double> convert_vector(const vector<bool> &vec) {
  vector<double> result(vec.size());
  std::transform(vec.begin(), vec.end(), result.begin(),
                 [](int v) { return 1 - 2 * v; });
  return result;
}

vector<bool> gen_vec(const int size) {
  vector<bool> result(size);
  std::generate(result.begin(), result.end(), []() { return rand() % 2; });
  return result;
}
} // namespace utils

namespace msf {

void add_rows(matrix &m, int i, int j) {
  for (int ind = 0; ind < m[0].size(); ind++) {
    m[j][ind] = m[j][ind] ^ m[i][ind];
  }
}

void top_to_bottom(matrix &matrix) {
  int n = (int)matrix.size();
  int m = (int)matrix[0].size();
  int i = 0;
  int j = 0;

  while (i < n && j < m) {
    if (matrix[i][j] != 1) {
      int k;

      bool one_exists = false;

      for (k = i + 1; k < n; k++) {
        if (matrix[k][j] == 1) {
          one_exists = true;
          break;
        }
      }

      if (!one_exists) {
        j++;
        continue;
      }

      swap(matrix[i], matrix[k]);
    }

    for (int k = i + 1; k < n; k++) {
      if (matrix[k][j] == 1) {
        add_rows(matrix, i, k);
      }
    }

    i++;
    j++;
  }
}

void bottom_to_top(matrix &matrix) {
  int n = (int)matrix.size();
  int m = (int)matrix[0].size();

  int j = m - 1;

  unordered_set<int> s;
  for (int k = 0; k < n; k++) {
    s.insert(k);
  }

  while (s.size() > 1) {
    bool one_exists = false;
    int k;
    for (k = n - 1; k >= 0; k--) {

      if (s.count(k) == 0) {
        continue;
      }

      if (matrix[k][j] == 1) {
        one_exists = true;
        break;
      }
    }

    if (!one_exists) {
      j--;
      continue;
    }

    s.erase(k);

    for (const int i : s) {
      if (matrix[i][j]) {
        add_rows(matrix, k, i);
      }
    }

    j--;
  }
}

void to_msf(matrix &m) {
  top_to_bottom(m);
  bottom_to_top(m);
}

span generate_span(const matrix &matrix) {
  int n = (int)matrix.size();
  int m = (int)matrix[0].size();

  vector<pair<int, int>> result;
  for (int i = 0; i < n; i++) {

    int first = -1;
    int last = -1;

    for (int j = 0; j < m; j++) {
      bool flag = matrix[i][j];

      if (first == -1 && flag) {
        first = j;
      }

      if (flag) {
        last = j;
      }
    }
    result.emplace_back(first, last - 1);
  }

  return result;
}
} // namespace msf

namespace lattice {

vector<greater_set> active_rows(const matrix &matrix, const span &span) {
  int n = (int)matrix.size();
  int m = (int)matrix[0].size();
  vector<greater_set> result(m);

  for (int j = 0; j < m; j++) {
    for (int i = 0; i < n; i++) {
      auto [b, e] = span[i];
      if (j >= b && j <= e) {
        result[j].insert(i);
      }
    }
  }

  return result;
}

class Node {
public:
  map<int, bool> value;
  double distance = 0;
  bool b = false;

  optional<Node *> main_parent = std::nullopt;
  optional<Node *> parent0 = std::nullopt;
  optional<Node *> parent1 = std::nullopt;

  static Node *fill(const pair<int, bool> &p,
                    const optional<Node *> &parent0_opt,
                    const optional<Node *> &parent1_opt) {
    Node *n = new Node();
    map<int, bool> mp;
    mp[p.first] = p.second;
    n->value = mp;
    if (parent0_opt.has_value()) {
      n->parent0 = parent0_opt.value();
    }
    if (parent1_opt.has_value()) {
      n->parent1 = parent1_opt.value();
    }
    return n;
  }

  static Node *fill(const optional<Node *> &parent0_opt,
                    const optional<Node *> &parent1_opt) {
    Node *n = new Node();
    if (parent0_opt.has_value()) {
      n->parent0 = parent0_opt.value();
    }
    if (parent1_opt.has_value()) {
      n->parent1 = parent1_opt.value();
    }
    return n;
  }
};

typedef vector<Node *> lattice_layer;
typedef vector<lattice_layer> lattice_t;

bool edge_value(const greater_set &active_rows_union, Node *from, Node *to,
                const matrix &matrix, const int column_number) {
  map<int, bool> values;

  for (const auto &[ind, v] : from->value) {
    values[ind] = v;
  }

  for (const auto &[ind, v] : to->value) {
    values[ind] = v;
  }

  auto v =
      std::accumulate(active_rows_union.begin(), active_rows_union.end(), false,
                      [&values, &matrix, &column_number](bool acc, int i) {
                        return acc ^ (values[i] && matrix[i][column_number]);
                      });
  return v;
}

void create_layer(lattice_t &lattice, const vector<greater_set> &active_rows,
                  int layer_number, const matrix &matrix) {

  const greater_set &current_active_rows = active_rows[layer_number - 1];
  const greater_set &next_active_rows = active_rows[layer_number];

  greater_set active_rows_union;

  set_union(current_active_rows.begin(), current_active_rows.end(),
            next_active_rows.begin(), next_active_rows.end(),
            inserter(active_rows_union, active_rows_union.begin()),
            greater<int>{});

  greater_set active_rows_intersection;

  set_intersection(
      current_active_rows.begin(), current_active_rows.end(),
      next_active_rows.begin(), next_active_rows.end(),
      inserter(active_rows_intersection, active_rows_intersection.begin()),
      greater<int>{});

  vector<int> difference_active_rows;

  set_difference(
      next_active_rows.begin(), next_active_rows.end(),
      active_rows_intersection.begin(), active_rows_intersection.end(),
      inserter(difference_active_rows, difference_active_rows.begin()),
      greater<int>{});

  vector<vector<bool>> difference_combinations;
  utils::generate_combinations((int)difference_active_rows.size(),
                               difference_combinations);

  lattice_layer &cur_layer = lattice[layer_number];
  lattice_layer &next_layer = lattice[layer_number + 1];

  for (Node *&cur_node : cur_layer) {
    map<int, bool> next_node_values;

    for (const int active_row : active_rows_intersection) {
      next_node_values[active_row] = cur_node->value[active_row];
    }

    for (const vector<bool> &combination : difference_combinations) {
      map<int, bool> next_node_values_copy = next_node_values;

      for (int i = 0; i < difference_active_rows.size(); i++) {
        next_node_values_copy[difference_active_rows[i]] = combination[i];
      }

      auto same_node_it = find_if(next_layer.begin(), next_layer.end(),
                                  [&next_node_values_copy](Node *node) {
                                    return node->value == next_node_values_copy;
                                  });

      if (same_node_it != next_layer.end()) {
        Node *existing_node = *same_node_it;
        bool edge_val = edge_value(active_rows_union, cur_node, existing_node,
                                   matrix, layer_number);
        if (edge_val) {
          existing_node->parent1 = cur_node;
        } else {
          existing_node->parent0 = cur_node;
        }
      } else {
        Node *node = new Node();

        node->value = next_node_values_copy;

        bool edge_val =
            edge_value(active_rows_union, cur_node, node, matrix, layer_number);

        if (edge_val) {
          node->parent1 = cur_node;
        } else {
          node->parent0 = cur_node;
        }

        next_layer.emplace_back(node);
      }
    }
  }
}

lattice_t start_fake_layers(const vector<greater_set> &active_rows,
                            const matrix &matrix) {

  Node *start = new Node();

  Node *zero_layer =
      Node::fill({*(active_rows[0].begin()), false}, start, nullopt);
  Node *first_layer =
      Node::fill({*(active_rows[0].begin()), true}, nullopt, start);

  lattice_t lattice(matrix[0].size());
  lattice[0] = {start};
  lattice[1] = {zero_layer, first_layer};

  return lattice;
}

Node *last_fake_node(lattice_t &lattice) {

  auto zero_prelast =
      find_if(lattice.back().begin(), lattice.back().end(), [](Node *node) {
        bool v = (*node->value.begin()).second;
        return !v;
      });

  auto first_prelast =
      find_if(lattice.back().begin(), lattice.back().end(), [](Node *node) {
        bool v = (*node->value.begin()).second;
        return v;
      });

  Node *last = Node::fill(*zero_prelast, *first_prelast);

  return last;
}

lattice_t build_lattice(const vector<greater_set> &active_rows,
                        const matrix &matrix) {

  lattice_t lattice = start_fake_layers(active_rows, matrix);

  for (int i = 1; i < active_rows.size() - 1; i++) {
    create_layer(lattice, active_rows, i, matrix);
  }

  Node *last = last_fake_node(lattice);
  lattice.push_back({last});

  return lattice;
}

vector<bool> decode(lattice_t &lattice, const vector<double> &vec) {
  lattice[0][0]->distance = 0;

  for (int i = 1; i < lattice.size(); i++) {
    lattice_layer &cur_layer = lattice[i];

    for (Node *&cur_node : cur_layer) {
      double max_distance = utils::MAX_VALUE;
      Node *from_node;
      bool flag;

      if (cur_node->parent0.has_value()) {
        if (vec[i - 1] + cur_node->parent0.value()->distance > max_distance) {
          max_distance = vec[i - 1] + cur_node->parent0.value()->distance;
          from_node = cur_node->parent0.value();
          flag = false;
        }
      }

      if (cur_node->parent1.has_value()) {
        if (-vec[i - 1] + cur_node->parent1.value()->distance > max_distance) {
          max_distance = -vec[i - 1] + cur_node->parent1.value()->distance;
          from_node = cur_node->parent1.value();
          flag = true;
        }
      }

      cur_node->distance = max_distance;
      cur_node->main_parent = from_node;
      cur_node->b = flag;
    }
  }

  vector<bool> path(vec.size());
  optional<Node *> cur = lattice.back()[0];
  int iter = 0;

  while (cur.has_value()) {
    path[iter] = cur.value()->b;
    iter++;
    cur = cur.value()->main_parent;
  }

  reverse(path.begin(), path.end());

  return path;
}
} // namespace lattice

namespace encoder {
inline vector<bool> encode(const matrix &m, const vector<bool> &vec) {
  vector<bool> result(m[0].size());

  for (int j = 0; j < m[0].size(); j++) {
    for (int i = 0; i < m.size(); i++) {
      result[j] = result[j] ^ (vec[i] && m[i][j]);
    }
  }

  return result;
}
} // namespace encoder

namespace solvers {
inline void decode(const matrix &matr, lattice::lattice_t &lattice,
                   vector<double> &vecdouble) {

  for (int i = 0; i < matr[0].size(); i++) {
    double x;
    cin >> x;
    vecdouble[i] = x;
  }

  auto res = lattice::decode(lattice, vecdouble);

  for (const bool it : res) {
    cout << it << " ";
  }
  cout << endl;
}

inline void encode(const matrix &matr, const matrix &matrix_copy,
                   vector<bool> &vecbool) {
  for (int i = 0; i < matr.size(); i++) {
    bool b;
    cin >> b;
    vecbool[i] = b;
  }
  auto res = encoder::encode(matrix_copy, vecbool);

  for (const bool it : res) {
    cout << it << " ";
  }
  cout << endl;
}

inline void simulate(const double &mdouble, const double &ndouble, const int n,
                     matrix &matrix_copy, lattice::lattice_t &lattice) {
  double noise;
  int max_iterations;
  int max_errors;

  cin >> noise >> max_iterations >> max_errors;

  double sigma = sqrt(0.5 * pow(10.0, -noise / 10.0) * (mdouble / ndouble));

  normal_distribution<double> distr(0, sigma);

  int errors_counter = 0;
  int iter;

  for (iter = 0; iter < max_iterations; iter++) {

    vector<bool> vec = utils::gen_vec(n);
    vector<bool> encoded = encoder::encode(matrix_copy, vec);
    vector<double> noisy = utils::convert_vector(encoded);

    for (double &i : noisy) {
      double x = distr(rng);
      i += x;
    }

    vector<bool> decoded = lattice::decode(lattice, noisy);

    if (encoded != decoded) {
      errors_counter++;
      if (errors_counter == max_errors) {
        break;
      }
    }
  }
  cout << setprecision(8) << (double)errors_counter / (double)iter << endl;
}
} // namespace solvers

int main() {
  freopen("input.txt", "r", stdin);
  freopen("output.txt", "w", stdout);

  ios::sync_with_stdio(0);
  cin.tie(0);
  cout.tie(0);

  int m, n;
  double mdouble, ndouble;
  cin >> m >> n;

  mdouble = m;
  ndouble = n;

  matrix matr(n, vector<bool>(m));

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      bool b;
      cin >> b;
      matr[i][j] = b;
    }
  }

  matrix matrix_copy = matr;
  msf::to_msf(matr);

  span matrix_span = msf::generate_span(matr);

  auto active_rows = lattice::active_rows(matr, matrix_span);
  auto lattice = lattice::build_lattice(active_rows, matr);

  for (const auto &it : lattice) {
    cout << it.size() << " ";
  }
  cout << endl;

  vector<bool> vecbool(matr.size());
  vector<double> vecdouble(matr[0].size());

  string s;
  while (cin >> s) {

    utils::Command command = utils::commandFromString(s);

    if (command == utils::Command::Encode) {
      solvers::encode(matr, matrix_copy, vecbool);
    } else if (command == utils::Command::Decode) {
      solvers::decode(matr, lattice, vecdouble);
    } else {
      solvers::simulate(mdouble, ndouble, n, matrix_copy, lattice);
    }
  }
  return 0;
}
