#include "sertable/blister.hh"
#include <cmath>
#include <iostream>

namespace cnst {
constexpr double gamma = -1.7, m = -1.7, alpha = -.5, beta = .4, M = .1,
                 N = -.5;
constexpr size_t I = 11, J = 10;
constexpr double l = 1., x0 = 0., xn = x0 + l;
constexpr double h = l / J;
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

void print_table(const std::array<std::array<double, cnst::J>, cnst::I> &arr) {}

int main() {
  std::cout << "foo" << std::endl;
  return 0;
}
