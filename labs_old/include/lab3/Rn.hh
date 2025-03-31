#pragma once

#include <algorithm>
#include <array>
#include <functional>
#include <iterator>
#include <ostream>
#include <sstream>
#include <utility>

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

namespace Rnu {
template <typename T, size_t N> Rn<T, N> abs(const Rn<T, N> &seq) {
  Rn<T, N> res;
  std::transform(seq.begin(), seq.end(), res.begin(),
                 [](auto &&x) { return x < 0 ? -x : x; });
  return res;
}

template <typename T, size_t N> T max(const Rn<T, N> &seq) {
  return *std::max_element(seq.begin(), seq.end());
}

template <typename T, size_t N> T norm(const Rn<T, N> &seq) {
  return max(abs(seq));
}
} // namespace Rnu
