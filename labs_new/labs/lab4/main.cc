#include "sertable/blister.hh"
#include <cmath>
#include <iostream>
#include <ranges>

namespace cnst {
constexpr double gamma = -1.7, m = -1.7, alpha = -.5, beta = .4, M = .1,
                 N = -.5;
constexpr size_t I = 10, J = 50;
constexpr double l = 1.;
constexpr double h = l / I, k = h;
} // namespace cnst
//

const auto &&fill_cell = tmp_fill_cell('-');
const auto &&hard_fill_cell = tmp_fill_cell('=');

constexpr size_t table_width = 12;

const auto &&table_header = std::invoke(
    format_n_seq,
    [](auto &&x) {
      if (x == 0)
        return std::string("t");
      std::stringstream ss;
      ss << "x=" << cnst::h * (x - 1);
      return ss.str();
    },
    size_const<table_width>());

const auto &&table_line = std::invoke(
    format_n_seq, [](auto &&x) { return fill_cell; },
    size_const<table_width>());

const auto &&table_hard_line = std::invoke(
    format_n_seq, [](auto &&x) { return hard_fill_cell; },
    size_const<table_width>());

auto &&f = [](double x) -> double {
  return cnst::gamma * std::exp(cnst::m * x) + std::cos(cnst::gamma * x);
};

auto &&phi = [](double t) -> double {
  return cnst::alpha * t + std::sin(cnst::beta * t);
};

auto &&psi = [](double t) -> double {
  return std::exp(cnst::N * t) + cnst::M * std::sin(cnst::N + cnst::m * t);
};

using arr_t = std::array<std::array<double, cnst::J>, cnst::I + 1>;

auto print_table(const double k, const arr_t &u) {
  return ostream_invoker{[&u, k](std::ostream &os) -> std::ostream & {
    os << table_hard_line << table_header << table_line;
    size_t J = 1. / k;
    for (size_t j = 0; j < cnst::J; ++j) {
      os << cell_wrapper("t = " + std::to_string(j * k));
      for (size_t i = 0; i <= cnst::I; ++i)
        os << cell_wrapper(u[i][j]);
      os << "|" << std::endl;
    }
    os << table_hard_line;
    std::cout << J << std::endl;
    return os;
  }};
}

arr_t explicit_method() {
  const double k = cnst::h * cnst::h / 6.;
  arr_t u;
  for (size_t i = 0; i <= cnst::I; ++i)
    u[i][0] = f(k * i);
  for (size_t j = 0; j < cnst::J; ++j) {
    u[0][j] = phi(k * j);
    u[cnst::I][j] = psi(k * j);
  }
  for (size_t j = 0; j < cnst::J - 1; ++j)
    for (size_t i = 0; i < cnst::I; ++i)
      u[i][j + 1] = (u[i + 1][j] + 4. * u[i][j] + u[i - 1][j]) / 6.;
  return u;
}

arr_t implicit_method(double S) {
  const double k = cnst::h * cnst::h / S;
  arr_t u, a, b;

  for (size_t i = 0; i <= cnst::I; ++i)
    u[i][0] = f(cnst::h * i);

  for (size_t j = 0; j < cnst::J; ++j) {
    u[0][j] = phi(k * j);
    u[cnst::I][j] = psi(k * j);
  }

  for (size_t j = 0; j < cnst::J - 1; ++j) {
    a[1][j + 1] = 1. / (2. + S);
    b[1][j + 1] = phi(k * j) + S * u[1][j];
  }

  for (size_t j = 0; j < cnst::J - 1; ++j) {
    for (size_t i = 2; i < cnst::I; ++i) {
      a[i][j + 1] = 1. / (2. + S - a[i - 1][j + 1]);
      b[i][j + 1] = a[i - 1][j + 1] * b[i - 1][j + 1] + S * u[i][j];
    }
    for (size_t i = cnst::I - 1; i > 0; --i)
      u[i][j + 1] = a[i][j + 1] * (b[i][j + 1] + u[i + 1][j + 1]);
  }

  return u;
}

#if 1
int main() {
  std::cout << print_table(cnst::h * cnst::h / 6., explicit_method());
  for (auto &&S :
       std::views::istream<double>(std::cin) |
           std::ranges::views::filter([](auto &&x) { return x > 0.; }) |
           std::views::take(3))
    std::cout << print_table(cnst::h * cnst::h / S, implicit_method(S));
  return 0;
}
#endif
