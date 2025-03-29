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
template <typename... T> auto row_wrapper(T &&...data) {
  return ostream_invoker{[args = std::forward_as_tuple(data...)](
                             std::ostream &os) -> std::ostream & {
    return std::apply(
        [&os](auto &&...args) -> std::ostream & {
          (os << ... << cell_wrapper(args)) << "|" << std::endl;
          return os;
        },
        std::move(args));
  }};
}
