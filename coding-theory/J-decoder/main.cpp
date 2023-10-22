#include <algorithm>
#include <fstream>
#include <iostream>
#include <numeric>
#include <random>
#include <set>

typedef std::vector<int> vec_int;

namespace utils {
enum class Command { Decode, Encode, Simulate };

Command command_from_string(const std::string &s) {
  if (s == "Decode")
    return Command::Decode;
  else if (s == "Encode")
    return Command::Encode;
  else
    return Command::Simulate;
}

inline int field_elem_deg(int a) {
  int d = 0;
  while (a > 0) {
    d++;
    a >>= 1;
  }
  return d;
}

inline int field_elem_multiply(int a, int b, int n, const vec_int &log_to_poly,
                               const vec_int &poly_to_log) {
  if (a == 0 || b == 0) {
    return 0;
  }

  return log_to_poly[(poly_to_log[a] + poly_to_log[b]) % n];
}

void field(const int n, const int p, vec_int &log_to_poly,
           vec_int &poly_to_log) {

  int poly = 1;
  poly_to_log[0] = 0;

  for (int i = 0; i < n; ++i) {
    log_to_poly[i] = poly;
    poly_to_log[poly] = i;

    poly <<= 1;

    if (field_elem_deg(poly) == field_elem_deg(p)) {
      poly ^= p;
    }
  }
}

inline void poly_multiply(const vec_int &a, int a_size, const vec_int &b,
                          int b_size, vec_int &res, int n,
                          const vec_int &log_to_poly,
                          const vec_int &poly_to_log) {

  for (int i = 0; i < a_size; ++i) {
    for (int j = 0; j < b_size; ++j) {
      res[i + j] ^=
          field_elem_multiply(a[i], b[j], n, log_to_poly, poly_to_log);
    }
  }
}

int cyclotomic_classes(int n, int d, int max_min_poly_deg,
                       const vec_int &log_to_poly, const vec_int &poly_to_log,
                       vec_int &generator) {
  std::vector<bool> used(d);
  std::fill(used.begin(), used.end(), false);

  generator[0] = 1;
  int generator_size = 1;

  for (int k = 1; k < d; ++k) {
    if (used[k]) {
      continue;
    }

    int current_k = k;

    vec_int poly(max_min_poly_deg);
    std::fill(poly.begin(), poly.end(), 0);

    poly[0] = log_to_poly[current_k];
    poly[1] = 1;

    int poly_size = 2;

    used[current_k] = true;

    current_k = (current_k << 1) % n;

    while (current_k != k) {
      const vec_int multiplier = {log_to_poly[current_k], 1};
      vec_int tmp(max_min_poly_deg);
      std::fill(tmp.begin(), tmp.end(), 0);

      poly_multiply(poly, poly_size, multiplier, 2, tmp, n, log_to_poly,
                    poly_to_log);

      poly_size++;

      for (int i = 0; i < poly_size; ++i) {
        poly[i] = tmp[i];
      }

      if (current_k < d) {
        used[current_k] = true;
      }

      current_k = (current_k << 1) % n;
    }

    vec_int tmp(n);
    std::fill(tmp.begin(), tmp.end(), 0);

    poly_multiply(generator, generator_size, poly, poly_size, tmp, n,
                  log_to_poly, poly_to_log);

    generator_size += poly_size - 1;

    for (int i = 0; i < generator_size; ++i) {
      generator[i] = tmp[i];
    }
  }
  return generator_size;
}
} // namespace utils

namespace encode {
void encode(const vec_int &generator, int generator_size,
            const vec_int &message, int message_size, vec_int &result) {
  const int result_size = generator_size + message_size - 1;

  for (int i = 0; i < generator_size - 1; ++i) {
    result[i] = 0;
  }

  for (int i = 0; i < message_size; ++i) {
    result[generator_size - 1 + i] = message[i];
  }

  vec_int temporary(result_size);

  for (int i = 0; i < result_size; ++i) {
    temporary[i] = result[i];
  }

  int temporary_sz = result_size;

  while (temporary_sz > 0) {
    if (temporary[temporary_sz - 1] != 0) {
      break;
    }
    temporary_sz--;
  }

  for (; temporary_sz >= generator_size; temporary_sz--) {
    if (temporary[temporary_sz - 1] == 0) {
      continue;
    }
    for (int i = temporary_sz - generator_size; i < temporary_sz; ++i) {
      temporary[i] ^= generator[i + generator_size - temporary_sz];
    }
  }

  for (int i = 0; i < generator_size - 1; ++i) {
    result[i] = temporary[i];
  }
}
} // namespace encode

namespace decode {
void syndromes(const vec_int &message, int message_size, vec_int &syndromes,
               int syndromes_size, const vec_int &log_to_poly,
               const vec_int &poly_to_log) {
  std::vector<bool> used(syndromes_size + 1);

  std::fill(used.begin(), used.end(), false);

  for (int power = 1; power <= syndromes_size; ++power) {
    if (!used[power]) {
      int syndrome = message[message_size - 1];

      for (int pos = (int)message_size - 2; pos >= 0; pos--) {
        if (syndrome == 0) {
          syndrome = message[pos];
        } else {
          syndrome =
              log_to_poly[(poly_to_log[syndrome] + power) % message_size] ^
              message[pos];
        }
      }

      syndromes[power - 1] = syndrome;
      used[power] = true;

      int next_power = (power << 1) % message_size;

      while (next_power != power) {

        syndrome = utils::field_elem_multiply(syndrome, syndrome, message_size,
                                              log_to_poly, poly_to_log);

        if (next_power <= syndromes_size) {
          used[next_power] = true;
          syndromes[next_power - 1] = syndrome;
        }

        next_power = (next_power << 1) % message_size;
      }
    }
  }
}

int berlekamp_massey(const int syndromes_size, const vec_int &syndromes,
                     const int n, const vec_int &log_to_poly,
                     const vec_int &poly_to_log, vec_int &c) {
  for (int i = 0; i < syndromes_size; ++i) {
    c[i] = 0;
  }

  c[0] = 1;
  int c_size = 1;

  vec_int b(syndromes_size);
  std::fill(b.begin(), b.end(), 0);

  b[0] = 1;
  int bSize = 1;

  int len = 0;
  int m = 1;

  for (int step = 0; step < syndromes_size; step++) {
    int d = syndromes[step];
    for (int i = 1; i <= len; ++i) {
      d ^= utils::field_elem_multiply(c[i], syndromes[step - i], n, log_to_poly,
                                      poly_to_log);
    }

    if (d != 0) {
      vec_int difference_c(n);
      std::fill(difference_c.begin(), difference_c.end(), 0);

      for (int i = 0; i < bSize; ++i) {
        difference_c[i + m] =
            utils::field_elem_multiply(b[i], d, n, log_to_poly, poly_to_log);
      }

      if (2 * len <= step) {

        for (int i = 0; i < c_size; ++i) {
          b[i] =
              c[i] == 0
                  ? 0
                  : log_to_poly[(poly_to_log[c[i]] + n - poly_to_log[d]) % n];
        }

        for (int i = c_size; i < syndromes_size; ++i) {
          b[i] = 0;
        }

        len = step + 1 - len;
        bSize = c_size;
        m = 1;

      } else {
        m++;
      }

      int difference_c_size = n;

      while (difference_c_size > 0 &&
             difference_c[difference_c_size - 1] == 0) {
        difference_c_size--;
      }

      if (difference_c_size != 0) {
        for (int i = 0; i < difference_c_size; ++i) {
          c[i] ^= difference_c[i];
        }

        c_size = std::max(c_size, difference_c_size);

        while (c_size > 0 && c[c_size - 1] == 0) {
          c_size--;
        }
      }
    } else {
      m++;
    }
  }

  return c_size;
}

inline int mistakes(const vec_int &sigma, const int sigma_size, const int n,
                    const vec_int &log_to_poly, const vec_int &poly_to_log,
                    vec_int &mistakes) {
  int mistakes_size = 0;

  for (int i = 0; i < n; ++i) {
    int res = sigma[0];

    for (int j = 1; j < sigma_size; ++j) {
      if (sigma[j] > 0) {
        res = res ^ log_to_poly[(poly_to_log[sigma[j]] + i * j) % n];
      }
    }

    if (res == 0) {
      mistakes[mistakes_size] = (n - i) % n;
      mistakes_size++;
    }
  }

  return mistakes_size;
}

void decode(int d, const vec_int &message, int n, const vec_int &log_to_poly,
            const vec_int &poly_to_log, vec_int &result) {

  const int syn_size = d - 1;
  vec_int syndr(syn_size);

  syndromes(message, n, syndr, syn_size, log_to_poly, poly_to_log);

  bool has_non_zero =
      std::accumulate(syndr.begin(), syndr.end(), false,
                      [](bool acc, int el) { return acc | (el != 0); });

  if (!has_non_zero) {
    for (int i = 0; i < n; ++i) {
      result[i] = message[i];
    }
    return;
  }

  vec_int sigma(d + 1);
  const int sigma_size =
      berlekamp_massey(syn_size, syndr, n, log_to_poly, poly_to_log, sigma);

  vec_int mist(d);
  const int mist_size =
      mistakes(sigma, sigma_size, n, log_to_poly, poly_to_log, mist);

  for (int i = 0; i < n; ++i) {
    result[i] = message[i];
  }

  for (int i = 0; i < mist_size; ++i) {
    result[mist[i]] ^= 1;
  }
}
} // namespace decode

namespace simulate {

inline void generate_random_vector(vec_int &word, std::mt19937 &gen) {
  std::generate(word.begin(), word.end(), [&gen]() { return gen() % 2; });
}

double simulate(const double noise_level, const int number_of_iterations,
                int max_errors, vec_int &gen, const int gen_size,
                const int code_size, const int d, const int n,
                vec_int &log_to_poly, vec_int &poly_to_log) {

  std::random_device rd;
  std::mt19937 generator{rd()};
  std::uniform_real_distribution<double> distribution(0.0, 1.0);

  int result_errors = 0;
  int result_trues = 0;
  for (; result_trues < number_of_iterations; result_trues++) {
    vec_int random_word(code_size);
    generate_random_vector(random_word, generator);

    vec_int expected_word(n);
    encode::encode(gen, gen_size, random_word, code_size, expected_word);
    vec_int noisy_word(n);

    for (int i = 0; i < n; ++i) {
      noisy_word[i] = expected_word[i];

      if (distribution(generator) < noise_level) {
        noisy_word[i] ^= 1;
      }
    }

    vec_int decode_word(n);
    decode::decode(d, noisy_word, n, log_to_poly, poly_to_log, decode_word);

    for (int i = 0; i < n; ++i) {
      if (decode_word[i] != expected_word[i]) {
        result_errors++;
        break;
      }
    }

    if (result_errors == max_errors) {
      break;
    }
  }

  return ((double)result_errors) / result_trues;
}
} // namespace simulate

namespace solvers {

void solve_encoder(const int code_size, const int n, const vec_int &generator,
                   const int generator_size) {

  vec_int input(code_size);
  for (int i = 0; i < code_size; ++i) {
    std::cin >> input[i];
  }

  vec_int result(n);
  encode::encode(generator, generator_size, input, code_size, result);

  for (int i = 0; i < n; ++i) {
    std::cout << result[i] << ' ';
  }
  std::cout << std::endl;
}

void solve_decoder(const int n, const int d, const vec_int &log_to_poly,
                   const vec_int &poly_to_log) {

  vec_int input(n);
  for (int i = 0; i < n; ++i) {
    std::cin >> input[i];
  }

  vec_int result(n);
  decode::decode(d, input, n, log_to_poly, poly_to_log, result);

  for (int i = 0; i < n; ++i) {
    std::cout << result[i] << ' ';
  }
  std::cout << std::endl;
}

void solve_simulate(vec_int &generator, const int generator_size,
                    vec_int &log_to_poly, vec_int &poly_to_log,
                    const int code_size, const int n, const int d) {
  double noise_level;
  int number_of_iterations;
  int max_errors;

  std::cin >> noise_level >> number_of_iterations >> max_errors;

  const double res = simulate::simulate(
      noise_level, number_of_iterations, max_errors, generator, generator_size,
      code_size, d, n, log_to_poly, poly_to_log);

  std::cout << res << std::endl;
}
} // namespace solvers

int main() {

  freopen("input.txt", "r", stdin);
  freopen("output.txt", "w", stdout);

  std::ios::sync_with_stdio(false);
  std::cin.tie(nullptr);
  std::cout.tie(nullptr);

  int n;
  int p;
  int d;

  std::cin >> n >> p >> d;

  vec_int log_to_poly(n);
  vec_int poly_to_log(n + 1);

  utils::field(n, p, log_to_poly, poly_to_log);

  const int max_min_poly_deg = utils::field_elem_deg(n) + 1;

  vec_int generator(n + 1);

  const int generator_size = utils::cyclotomic_classes(
      n, d, max_min_poly_deg, log_to_poly, poly_to_log, generator);

  const int code_size = n - generator_size + 1;
  std::cout << code_size << std::endl;

  for (int i = 0; i < generator_size; ++i) {
    std::cout << generator[i] << ' ';
  }

  std::cout << std::endl;

  std::string s;
  while (std::cin >> s) {
    const utils::Command command = utils::command_from_string(s);
    if (command == utils::Command::Encode) {
      solvers::solve_encoder(code_size, n, generator, generator_size);
    } else if (command == utils::Command::Decode) {
      solvers::solve_decoder(n, d, log_to_poly, poly_to_log);
    } else {
      solvers::solve_simulate(generator, generator_size, log_to_poly,
                              poly_to_log, code_size, n, d);
    }
  }
}
