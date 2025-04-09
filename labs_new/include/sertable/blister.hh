#pragma once

#include <functional>
#include <iomanip>
#include <iostream>
#include <tuple>

constexpr size_t cell_width = 15;

template <typename T> struct ostream_invoker {
  T func;
  ostream_invoker(T &&f) : func(f) {}
};

template <typename T>
std::ostream &operator<<(std::ostream &os, const ostream_invoker<T> &inv) {
  return std::invoke(inv.func, os);
}

template <typename T> auto cell_wrapper(T &&data) {
  return ostream_invoker{
      [arg = std::forward<T>(data)](std::ostream &os) -> std::ostream & {
        return os << "|" << std::setw(cell_width) << std::right << arg;
      }};
}
// T... ↦ ostream& ↦ ostream&
 auto row_wrapper(auto &&...data) {
  return ostream_invoker{
      [args = std::make_tuple(data...)](std::ostream &os) -> std::ostream & {
        return std::apply(
            [&os](auto &&...args) -> std::ostream & {
              (os << ... << cell_wrapper(args)) << "|" << std::endl;
              return os;
            },
            std::move(args));
      }};
}

auto tmp_fill_cell(char c) {
  return ostream_invoker{[c](std::ostream &os) -> std::ostream & {
    os << std::setfill(c) << c << std::setfill(' ');
    return os;
  }};
};

template <size_t sz> using size_const = std::integral_constant<size_t, sz>;

auto &&format_n_seq = []<size_t n>(auto &&format, size_const<n>) {
  return std::invoke(
      [&format]<std::size_t... inx>(std::index_sequence<inx...>) {
        return row_wrapper(format(inx)...);
      },
      std::make_index_sequence<n>());
};

