#include "sertable/blister.hh"
#include "sertable/try_blister.hh"
#include <algorithm>
#include <array>
#include <cstdlib>
#include <iterator>
#include <ostream>
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

const auto &&table_header = row_wrapper("k", "x^(k)", "x^(k+1)", "dx^(k+1)",
                                        "y^(k)", "y^(k+1)", "dy^(k+1)");
const auto &&table_line =
    row_wrapper(fill_cell, fill_cell, fill_cell, fill_cell, fill_cell,
                fill_cell, fill_cell);
const auto &&table_hard_line =
    row_wrapper(hard_fill_cell, hard_fill_cell, hard_fill_cell, hard_fill_cell,
                hard_fill_cell, hard_fill_cell, hard_fill_cell);

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

#if 0
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

template <typename... T> auto abss(const std::tuple<T...> &seq) {
  return std::apply(
      [](auto &&...args) { return std::make_tuple(std::abs(args)...); }, seq);
}

#endif

template <typename T, size_t N> struct Rn : std::array<T, N> {
  Rn &operator-=(const Rn &obj) {
    std::transform(this->begin(), this->end(), obj.begin(), this->begin(),
                   std::minus<T>());
    return *this;
  }
  constexpr auto to_tuple() const {
    return std::invoke(
        [&arr =
             std::as_const(*this)]<size_t... inx>(std::index_sequence<inx...>) {
          return std::make_tuple(arr[inx]...);
        },
        std::make_index_sequence<N>());
  }
};

template <typename T, size_t N>
Rn<T, N> operator-(const Rn<T, N> &a, const Rn<T, N> &b) {
  auto res = a;
  res -= b;
  return res;
}

template <typename T, size_t N>
std::ostream &operator<<(std::ostream &os, const Rn<T, N> &obj) {
  os << "(";
  std::transform(obj.begin(), obj.end(), std::ostream_iterator<std::string>(os),
                 [](auto &&i) {
                   std::stringstream ss;
                   ss << i << " ";
                   return ss.str();
                 });
  os << ")";
  return os;
}

template <typename T, size_t N>
std::istream &operator>>(std::istream &is, Rn<T, N> &obj) {
  std::copy_n(std::istream_iterator<T>(is), N, obj.begin());
  return is;
}

template <typename T, size_t N> Rn<T, N> abss(const Rn<T, N> &seq) {
  Rn<T, N> res;
  std::transform(seq.begin(), seq.end(), res.begin(),
                 [](auto &&x) { return x < 0 ? -x : x; });
  return res;
}

template <typename T, size_t N> T maxs(const Rn<T, N> &seq) {
  return *std::max_element(seq.begin(), seq.end());
}

using R3 = Rn<double, 2>;
constexpr double eps1 = 1e-3, eps2 = 1e-5;

auto convert_to_row = [](size_t k, R3 cur, R3 prev) {
  auto [cx, cy] = cur.to_tuple();
  auto [px, py] = prev.to_tuple();
  auto [dx, dy] = abss(cur - prev).to_tuple();
  return std::make_tuple(k, px, cx, dx, py, cy, dy);
};

auto &&check_input = [](double x, double y) {
  if (!((std::abs(3 * (y - 3) * (y - 4)) < (2 * x - y) * (2 * x - y)) &&
        (std::abs(3 * (2 * x - 3) * (x - 2)) < (2 * x - y) * (2 * x - y))))
    throw std::invalid_argument(std::invoke([&]() {
      std::stringstream ss;
      ss << "Given argument is incorrect, it should be in this set:"
         << std::endl;
      ss << "{3|(y-3)(y-4)| < (2x-y)^2" << std::endl;
      ss << "{3|(2x-3)(x-2)| < (2x-y)^2" << std::endl;
      ss << "Given argument is: " << R3{x, y} << std::endl;
      return ss.str();
    }));
};

#if 1
auto sim(R3 init_aprox, double eps) {
  return [init_aprox, eps](std::ostream &os) -> std::tuple<size_t, R3, double> {
    std::apply(check_input, init_aprox.to_tuple());
    auto &&f = [](double x, double y) -> double {
      return x - ((2 * x - 3) * (x - 2)) / (2 * x - y);
    };
    auto &&g = [](double x, double y) -> double {
      return y + ((y - 3) * (y - 4)) / (2 * x - y);
    };
    size_t k;
    R3 x_k = init_aprox;
    for (k = 0;; ++k) {
      auto &&prev = std::exchange(x_k, R3{std::apply(f, x_k.to_tuple()),
                                          std::apply(g, x_k.to_tuple())});
      os << std::apply([](auto &&...args) { return row_wrapper(args...); },
                       convert_to_row(k, x_k, prev));
      if (maxs(abss(x_k - prev)) < eps)
        break;
    }
    return {k, x_k, eps};
  };
}
#endif

int main() {
  std::cout << "Insert init approx: ";
  R3 init_aprx;
  std::cin >> init_aprx;
  try_block([init_aprx] {
    std::cout << "Simple iteration method: \n"
              << table_print(sim(init_aprx, eps1)) << '\n'
              << table_print(sim(init_aprx, eps2));
  });
  return 0;
}
