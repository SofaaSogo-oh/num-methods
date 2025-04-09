#include "lab3/Rn.hh"
#include "sertable/blister.hh"
#include "sertable/try_blister.hh"
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <ostream>
#include <utility>

const auto &&tmp_fill_cell = [](char c) {
  return ostream_invoker{[c](std::ostream &os) -> std::ostream & {
    os << std::setfill(c) << c << std::setfill(' ');
    return os;
  }};
};

const auto &&fill_cell = tmp_fill_cell('-');
const auto &&hard_fill_cell = tmp_fill_cell('=');

const auto &&table_header =
    row_wrapper("k+1", "x^(k)", "x^(k+1)", "dx^(k+1)", "y^(k)", "y^(k+1)",
                "dy^(k+1)", "f0(x^(k+1))", "f1(x^(k+1))");
const auto &&table_line =
    row_wrapper(fill_cell, fill_cell, fill_cell, fill_cell, fill_cell,
                fill_cell, fill_cell, fill_cell, fill_cell);
const auto &&table_hard_line =
    row_wrapper(hard_fill_cell, hard_fill_cell, hard_fill_cell, hard_fill_cell,
                hard_fill_cell, hard_fill_cell, hard_fill_cell, hard_fill_cell,
                hard_fill_cell);

template <typename F> auto table_print(F &&method) {
  return ostream_invoker{
      [method = std::forward<F>(method)](std::ostream &os) -> std::ostream & {
        os << table_hard_line << table_header << table_line;
        auto &&[n, x_n, eps] = std::invoke(method, os);
        os << table_line;
        os << "Iterations count: n = " << n << std::endl;
        os << "Result approximation: x_n = " << x_n << std::endl;
        os << "Given eps: eps = " << eps << std::endl;
        os << table_hard_line;
        return os;
      }};
}

using R3 = Rn<double, 2>;
constexpr double eps1 = 1e-3, eps2 = 1e-5;

template <typename T, size_t N> void throw_any_nan(const Rn<T, N> &rhs) {
  if (std::any_of(rhs.begin(), rhs.end(),
                  [](auto &&arg) { return std::isnan(arg); }))
    throw std::logic_error{std::invoke([&rhs] {
      std::stringstream ss;
      ss << "There exists nan value in the given Rn vector" << std::endl
         << "Rn = " << rhs << std::endl;
      return ss.str();
    })};
}

auto &&argument_check = [](double x, double y) {
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

using method_result = std::tuple<size_t, R3, double>;

double f0(double x, double y) { return 2 * x + y - 7; }

double f1(double x, double y) { return x * y - 6; }

auto convert_to_row = [](size_t k, R3 cur, R3 prev) {
  auto [cx, cy] = cur.to_tuple();
  auto [px, py] = prev.to_tuple();
  auto [dx, dy] = Rnu::abs(cur - prev).to_tuple();
  return std::make_tuple(k, px, cx, dx, py, cy, dy, f0(cx, cy), f1(cx, cy));
};

auto sim(const R3 &init_aprox, double eps) {
  return [init_aprox, eps](std::ostream &os) -> method_result {
    /*std::apply(argument_check, init_aprox.to_tuple());*/
    auto &&f = [](double x, double y) -> double {
      return x + ((-x) / (2 * x - y)) * (2 * x + y - 7) +
             (1 / (2 * x - y)) * (x * y - 6);
    };
    auto &&g = [](double x, double y) -> double {
      return y + ((-y) / (y - 2 * x)) * (2 * x + y - 7) +
             (2 / (y - 2 * x)) * (x * y - 6);
    };
    auto &&next = [&f, &g](const R3 &cur) -> R3 {
      auto &&tup = cur.to_tuple();
      return {std::apply(f, tup), std::apply(g, tup)};
    };
    auto &&ev_f = [](auto &&func, R3 x_k) -> double {
      return std::abs(std::apply(func, x_k.to_tuple()));
    };
    size_t k;
    R3 x_k = init_aprox;
    for (k = 1;; ++k) {
      auto &&prev = std::exchange(x_k, next(x_k));
      os << std::apply([](auto &&...args) { return row_wrapper(args...); },
                       convert_to_row(k, x_k, prev));
      throw_any_nan(x_k);
      if (Rnu::norm(x_k - prev) < eps &&
          (ev_f(f0, x_k) < eps && ev_f(f1, x_k) < eps))
        break;
    }
    return {k, x_k, eps};
  };
}

#if 1
auto newthon(const R3 &init_aprx, double eps) {
  return [init_aprx, eps](std::ostream &os) -> method_result {
    auto &&f = [](double x, double y) -> double {
      return x - (x * (2 * x + y - 7) - (x * y - 6)) / (2 * x - y);
    };
    auto &&g = [](double x, double y) -> double {
      return y - (-y * (2 * x + y - 7) + 2 * (x * y - 6)) / (2 * x - y);
    };
    auto &&next = [&f, &g](const R3 &cur) -> R3 {
      auto &&tup = cur.to_tuple();
      return {std::apply(f, tup), std::apply(g, tup)};
    };
    auto &&ev_f = [](auto &&func, const R3 &cur) {
      return std::abs(std::apply(func, cur.to_tuple()));
    };
    size_t k;
    R3 x_k = init_aprx;
    for (k = 1;; ++k) {
      auto &&prev = std::exchange(x_k, next(x_k));
      os << std::apply([](auto &&...args) { return row_wrapper(args...); },
                       convert_to_row(k, x_k, prev));
      throw_any_nan(x_k);
      if (Rnu::norm(x_k - prev) < eps &&
          (ev_f(f0, x_k) < eps && ev_f(f1, x_k) < eps))
        break;
    }
    return {k, x_k, eps};
  };
}

auto mod_newthon(const R3 &init_aprx, double eps) {
  return [init_aprx, eps](std::ostream &os) -> method_result {
    auto &&[x0, y0] = init_aprx.to_tuple();
    auto &&f = [x0, y0](double x, double y) -> double {
      return x - (x0 * (2 * x + y - 7) - (x * y - 6)) / (2 * x0 - y0);
    };
    auto &&g = [x0, y0](double x, double y) -> double {
      return y - (-y0 * (2 * x + y - 7) + 2 * (x * y - 6)) / (2 * x0 - y0);
    };
    auto &&next = [&f, &g](const R3 &cur) -> R3 {
      auto &&tup = cur.to_tuple();
      return {std::apply(f, tup), std::apply(g, tup)};
    };
    auto &&ev_f = [](auto &&func, const R3 &cur) {
      return std::abs(std::apply(func, cur.to_tuple()));
    };
    size_t k;
    R3 x_k = init_aprx;
    for (k = 1;; ++k) {
      auto &&prev = std::exchange(x_k, next(x_k));
      os << std::apply([](auto &&...args) { return row_wrapper(args...); },
                       convert_to_row(k, x_k, prev));
      throw_any_nan(x_k);
      if (Rnu::norm(x_k - prev) < eps &&
          (ev_f(f0, x_k) < eps && ev_f(f1, x_k) < eps))
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
  try_block([init_aprx] {
    std::cout << "Newthon method: \n"
              << table_print(newthon(init_aprx, eps1)) << '\n'
              << table_print(newthon(init_aprx, eps2));
  });
  try_block([init_aprx] {
    std::cout << "Mod Newthon method: \n"
              << table_print(mod_newthon(init_aprx, eps1)) << '\n'
              << table_print(mod_newthon(init_aprx, eps2));
  });
  return 0;
}
