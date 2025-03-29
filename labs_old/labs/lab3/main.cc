#include "sertable/blister.hh"
#include "sertable/try_blister.hh"
#include <algorithm>
#include <tuple>
#include <utility>

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

using R3 = std::tuple<double, double>;

template <typename... T>
auto operator-(const std::tuple<T...> &t1, const std::tuple<T...> &t2) {
  return std::invoke(
      [t1, t2]<size_t... inx>(std::index_sequence<inx...>) {
        return std::make_tuple(std::get<inx>(t1) - std::get<inx>(t2)...);
      },
      std::index_sequence_for<T...>());
}

template <typename... T>
std::ostream &operator<<(std::ostream &os, const std::tuple<T...> &t) {
  return std::apply(
      [&os]<typename... U>(U &&...args) -> std::ostream & {
        return os << "(",
               (os << ...
                   << ostream_invoker([arg = std::forward<U>(args)](
                                          std::ostream &os) -> std::ostream & {
                        return os << arg << " ";
                      }))
                   << ")";
      },
      t);
}

template <typename... T> auto maxs(const std::tuple<T...> &seq) {
  return std::apply([](auto &&...args) { return std::max({args...}); }, seq);
}

#if 1
auto sim(R3 init_aprox, double eps) {
  return [init_aprox, eps](std::ostream &os) -> std::tuple<size_t, R3, double> {
    auto f = [](double x, double y) -> double {
      return x + ((-x) / (2. * x - y)) * (2. * x + y - 7.) +
             (1. / (2. * x * y)) * (x * y - 7);
    };
    auto g = [](double x, double y) -> double {
      return y + ((-y) / (y - 2. * x)) * (2. * x + y - 7.) +
             (2. / (y - 2. * x)) * (x * y - 7);
    };
    size_t k;
    R3 x_k = init_aprox;
    for (k = 0;; ++k) {
      auto &&prev = std::exchange(
          x_k, std::make_tuple(std::apply(f, x_k), std::apply(g, x_k)));
      if (maxs(x_k - prev) < eps)
        break;
    }
    return {k, x_k, eps};
  };
}
#endif

int main() { return 0; }
