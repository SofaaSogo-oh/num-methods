#include "sertable/blister.hh"
#include "sertable/try_blister.hh"
#include <cmath>
#include <iomanip>
#include <stdexcept>
#include <utility>

using R3 = double;
constexpr double eps1 = 1e-3, eps2 = 1e-5;

const auto &&tmp_fill_cell = [](char c) {
  return ostream_invoker{[c](std::ostream &os) -> std::ostream & {
    os << std::setfill(c) << c << std::setfill(' ');
    return os;
  }};
};

const auto &&fill_cell = tmp_fill_cell('-');
const auto &&hard_fill_cell = tmp_fill_cell('=');

const auto &&table_header = row_wrapper("k", "x_k", "x_k-1", "dx", "dy");
const auto &&table_line =
    row_wrapper(fill_cell, fill_cell, fill_cell, fill_cell, fill_cell);
const auto &&table_hard_line =
    row_wrapper(hard_fill_cell, hard_fill_cell, hard_fill_cell, hard_fill_cell,
                hard_fill_cell);

inline constexpr auto argument_check = [](const R3 &init_aprx) -> void {
  if (!(-1 <= init_aprx && init_aprx <= -.5))
    throw std::invalid_argument("Given argument should be in [-1, -1/2]");
};

decltype(auto) sim(const R3 &init_aprx, double eps) {
  return [&init_aprx = std::as_const(init_aprx),
          eps](std::ostream &os) -> std::tuple<size_t, R3, double> {
    argument_check(init_aprx);
    auto &&f = [](double x) { return std::exp(2. * x) + x * x * x; };
    auto &&h = [](double x) {
      return x - (std::exp(2. * x) + (x * x * x)) / 2.;
    };
    R3 x_k = init_aprx;
    size_t k;
    for (k = 0;; ++k) {
      auto &&prev = std::exchange(x_k, h(x_k));
      auto &&dx = std::abs(x_k - prev), dy = f(x_k);
      os << row_wrapper(k, x_k, prev, dx, dy);
      if (dx < eps && std::abs(dy) < eps)
        break;
    }
    return {k, x_k, eps};
  };
}

decltype(auto) newthon(const R3 &init_aprx, double eps) {
  return [&init_aprx = std::as_const(init_aprx),
          eps](std::ostream &os) -> std::tuple<size_t, R3, double> {
    argument_check(init_aprx);
    auto &&f = [](double x) { return std::exp(2. * x) + x * x * x; };
    auto &&g = [](double x) { return 2. * std::exp(2. * x) + 3. * x * x; };
    R3 x_k = init_aprx;
    size_t k;
    for (k = 0;; ++k) {
      auto &&prev = std::exchange(x_k, x_k - f(x_k) / g(x_k));
      auto &&dx = std::abs(x_k - prev), dy = f(x_k);
      os << row_wrapper(k, x_k, prev, dx, dy);
      if (dx < eps && std::abs(dy) < eps)
        break;
    }
    return {k, x_k, eps};
  };
}

decltype(auto) mod_newthon(const R3 &init_aprx, double eps) {
  return [&init_aprx = std::as_const(init_aprx),
          eps](std::ostream &os) -> std::tuple<size_t, R3, double> {
    auto &&f = [](double x) { return std::exp(2. * x) + x * x * x; };
    auto &&g = [](double x) { return 2. * std::exp(2. * x) + 3. * x * x; };
    R3 x_k = init_aprx, g_0 = g(init_aprx);
    size_t k;
    for (k = 0;; ++k) {
      auto &&prev = std::exchange(x_k, x_k - f(x_k) / g_0);
      auto &&dx = std::abs(x_k - prev), dy = f(x_k);
      os << row_wrapper(k, x_k, prev, dx, dy);
      if (dx < eps && std::abs(dy) < eps)
        break;
    }
    return {k, x_k, eps};
  };
}

template <typename F> auto table_print(F &&method) {
  return ostream_invoker{
      [method = std::forward<F>(method)](std::ostream &os) -> std::ostream & {
        os << table_hard_line << table_header << table_line;
        auto &&[n, x_n, eps] = std::invoke(method, os);
        os << table_line;
        os << "Iterations count: n = " << n + 1 << std::endl;
        os << "Result approximation: x_n = " << x_n << std::endl;
        os << "Given eps: eps = " << eps << std::endl;
        os << table_hard_line;
        return os;
      }};
}

int main() {
  std::cout << "Insert init approx: ";
  R3 init_aprx;
  std::cin >> init_aprx;
  try_block([init_aprx] {
    std::cout << "Simple iteration method: \n"
              << table_print(sim(init_aprx, eps1)) << '\n'
              << table_print(sim(init_aprx, eps2));
  });

  try_block([init_aprx] {
    std::cout << "Newthon method: \n"
              << table_print(newthon(init_aprx, eps1)) << '\n'
              << table_print(newthon(init_aprx, eps2));
  });

  try_block([init_aprx] {
    std::cout << "Modified newthon method: \n"
              << table_print(mod_newthon(init_aprx, eps1)) << '\n'
              << table_print(mod_newthon(init_aprx, eps2));
  });
  return 0;
}
