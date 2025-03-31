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

double f0(double x, double y) { return 2 * x + y - 7; }

double f1(double x, double y) { return x * y - 6; }

auto sim(const R3 &init_aprox, double eps) {
  return [init_aprox, eps](std::ostream &os) -> method_result {
    std::apply(argument_check, init_aprox.to_tuple());
    auto &&f = [](double x, double y) -> double {
      return x - ((2 * x - 3) * (x - 2)) / (2 * x - y);
    };
    auto &&g = [](double x, double y) -> double {
      return y + ((y - 3) * (y - 4)) / (2 * x - y);
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
    for (k = 0;; ++k) {
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

auto newthon(const R3 &init_aprx, double eps) {
  return [init_aprx, eps](std::ostream &os) -> method_result {
    auto &&f = [](double x, double y) -> double {
      return x - (x * f0(x, y) - f1(x, y)) / (2 * x - y);
    };
    auto &&g = [](double x, double y) -> double {
      return y - (-y * f0(x, y) + 2 * f1(x, y)) / (2 * x - y);
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
    for (k = 0;; ++k) {
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
