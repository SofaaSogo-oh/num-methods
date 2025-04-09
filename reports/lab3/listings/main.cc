double f0(double x, double y) { return 2 * x + y - 7; }

double f1(double x, double y) { return x * y - 6; }

auto sim(const R3 &init_aprox, double eps) {
  return [init_aprox, eps](std::ostream &os) -> method_result {
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
