inline constexpr auto argument_check = [](const R3 &init_aprx) -> void {
  if (!(-1 <= init_aprx && init_aprx <= -.5))
    throw std::invalid_argument("Given argument should be in [-1, -1/2]");
};

decltype(auto) sim(const R3 &init_aprx, double eps) {
  return [&init_aprx = std::as_const(init_aprx),
          eps](std::ostream &os) -> std::tuple<size_t, R3, double> {
    argument_check(init_aprx);
    auto &&f = [](double x) { return std::exp(2. * x) + x * x * x; };
    auto &&h = [](double x) {
      return x - (std::exp(2. * x) + (x * x * x)) / 2.;
    };
    R3 x_k = init_aprx;
    size_t k;
    for (k = 0;; ++k) {
      auto &&prev = std::exchange(x_k, h(x_k));
      auto &&dx = std::abs(x_k - prev), dy = f(x_k);
      os << row_wrapper{k, x_k, prev, dx, dy} << std::endl;
      if (dx < eps && std::abs(dy) < eps)
        break;
    }
    return {k, x_k, eps};
  };
}

decltype(auto) newton(const R3 &init_aprx, double eps) {
  return [&init_aprx = std::as_const(init_aprx),
          eps](std::ostream &os) -> std::tuple<size_t, R3, double> {
    argument_check(init_aprx);
    auto &&f = [](double x) { return std::exp(2. * x) + x * x * x; };
    auto &&g = [](double x) { return 2. * std::exp(2. * x) + 3. * x * x; };
    R3 x_k = init_aprx;
    size_t k;
    for (k = 0;; ++k) {
      auto &&prev = std::exchange(x_k, x_k - f(x_k) / g(x_k));
      auto &&dx = std::abs(x_k - prev), dy = f(x_k);
      os << row_wrapper{k, x_k, prev, dx, dy} << std::endl;
      if (dx < eps && std::abs(dy) < eps)
        break;
    }
    return {k, x_k, eps};
  };
}

decltype(auto) mod_newton(const R3 &init_aprx, double eps) {
  return [&init_aprx = std::as_const(init_aprx),
          eps](std::ostream &os) -> std::tuple<size_t, R3, double> {
    argument_check(init_aprx);
    auto &&f = [](double x) { return std::exp(2. * x) + x * x * x; };
    auto &&g = [](double x) { return 2. * std::exp(2. * x) + 3. * x * x; };
    R3 x_k = init_aprx, g_0 = g(init_aprx);
    size_t k;
    for (k = 0;; ++k) {
      auto &&prev = std::exchange(x_k, x_k - f(x_k) / g_0);
      auto &&dx = std::abs(x_k - prev), dy = f(x_k);
      os << row_wrapper{k, x_k, prev, dx, dy} << std::endl;
      if (dx < eps && std::abs(dy) < eps)
        break;
    }
    return {k, x_k, eps};
  };
}
