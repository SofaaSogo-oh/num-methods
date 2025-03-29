#include "sertable/blister.hh"
#include "sertable/try_blister.hh"
#include <algorithm>
#include <tuple>
#include <utility>

using R3 = std::tuple<double, double>;

template <typename... T>
auto operator-(const std::tuple<T...> &t1, const std::tuple<T...> &t2) {
  return std::apply(
      [t1, t2](auto &&...inx) {
        return std::make_tuple(std::get<inx>(t1) - std::get<inx>(t2)...);
      },
      std::index_sequence_for<T...>());
}

template <typename... T> auto maxs(const std::tuple<T...> &seq) {
  return std::apply(
      [](auto &&car, auto &&...cdr) {
        return ((car = std::max(cdr, car)), ...);
      },
      seq);
}

auto sim(R3 init_aprox, double eps) {
  return [init_aprox, eps](std::ostream &os) -> std::tuple<size_t, R3, double> {
    auto f = [](double x, double y) -> double {};
    auto g = [](double x, double y) -> double {};
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

int main() {
  try_block([] {
    std::cout << ostream_invoker{[](std::ostream &os) -> std::ostream & {}};
  });
  return 0;
}
