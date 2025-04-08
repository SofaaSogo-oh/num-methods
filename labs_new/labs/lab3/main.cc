#include "sertable/blister.hh"
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

template <size_t sz> using size_const = std::integral_constant<size_t, sz>;
constexpr size_t table_width = 12;

auto &&format_n_seq = []<size_t n>(auto &&format, size_const<n>) {
  return std::invoke(
      [&format]<std::size_t... inx>(std::index_sequence<inx...>) {
        return row_wrapper(format(inx)...);
      },
      std::make_index_sequence<n>());
};

namespace lab3 {
constexpr double gamma = -1.7, m = -1.7, alpha = -.5, betta = .4, M = .1,
                 N = -.5;
constexpr double l = 1.0, h = l / 10;
constexpr double a = 1.0, k = h / a;
} // namespace lab3

const auto &&table_header = std::invoke(
    format_n_seq,
    [](auto &&x) {
      if (x == 0)
        return std::string("t");
      std::stringstream ss;
      ss << "x=" << lab3::h * (x - 1);
      return ss.str();
    },
    size_const<table_width>());

const auto &&table_line = std::invoke(
    format_n_seq, [](auto &&x) { return fill_cell; },
    size_const<table_width>());

const auto &&table_hard_line = std::invoke(
    format_n_seq, [](auto &&x) { return hard_fill_cell; },
    size_const<table_width>());

constexpr auto &&f = [](double x) -> double {
  return lab3::gamma * std::exp(lab3::m * x) + std::cos(lab3::gamma * x);
};

constexpr auto &&F = [](double x) -> double {
  return lab3::alpha * std::exp(x) + lab3::betta * std::cos(lab3::gamma * x);
};

constexpr auto &&phi = [](double t) -> double {
  return lab3::alpha * t + std::sin(lab3::betta * t);
};

constexpr auto &&psi = [](double t) -> double {
  using namespace lab3;
  return std::exp(lab3::N * t) + lab3::M * std::sin(lab3::m * t) + lab3::N;
};

int main() {
  std::array<std::array<double, 12>, 11> dp;
  auto dp_printer = ostream_invoker{[&dp](std::ostream &os) -> std::ostream & {
    os << table_hard_line;
    for (auto &&i : dp) {
      for (auto &&j : i)
        os << j << " ";
      os << std::endl;
    }
    os << table_hard_line;
    return os;
  }};

  for (size_t i = 0; i <= 10; ++i) {
    dp[i][1] = f(i * lab3::h);
    dp[i][0] = dp[i][1] - lab3::k * F(i * lab3::h);
  }
  for (size_t j = 1; j <= 11; ++j) {
    dp[0][j] = phi(j * lab3::k);
    dp[9][j] = psi(j * lab3::k);
  }

  for (size_t j = 1; j <= 10; ++j)
    for (size_t i = 1; i <= 10; ++i)
      dp[i][j + 1] = dp[i + 1][j] + dp[i - 1][j] - dp[i][j - 1];

  std::cout << table_hard_line << table_header << table_line;
  for (size_t j = 0; j <= 11; ++j) {
    if (j == 0)
      std::cout << cell_wrapper("v. layer");
    else
      std::cout << cell_wrapper("t=" + std::to_string(lab3::k * j));
    for (size_t i = 0; i <= 10; ++i)
      std::cout << cell_wrapper(dp[i][j]);
    std::cout << "|" << std::endl;
  }

  return 0;
}
