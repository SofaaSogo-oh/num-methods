namespace cnst {
constexpr double gamma = -1.7, m = -1.7, alpha = -.5, betta = .4, M = .1,
                 N = -.5;
constexpr double l = 1.0, h = l / 10;
constexpr double a = 1.0, k = h / a;
constexpr size_t I = 10, J = 11;
} // namespace cnst

constexpr auto &&f = [](double x) -> double {
  return cnst::gamma * std::exp(cnst::m * x) + std::cos(cnst::gamma * x);
};

constexpr auto &&F = [](double x) -> double {
  return cnst::alpha * std::exp(x) + cnst::betta * std::cos(cnst::gamma * x);
};

constexpr auto &&phi = [](double t) -> double {
  return cnst::alpha * t + std::sin(cnst::betta * t);
};

constexpr auto &&psi = [](double t) -> double {
  return std::exp(cnst::N * t) + cnst::M * std::sin(cnst::m * t) + cnst::N;
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
    dp[i][1] = f(i * cnst::h);
    dp[i][0] = dp[i][1] - cnst::k * F(i * cnst::h);
  }
  dp[0][0] = std::numeric_limits<double>::quiet_NaN();
  dp[10][0] = std::numeric_limits<double>::quiet_NaN();
  for (size_t j = 1; j <= 11; ++j) {
    dp[0][j] = phi(j * cnst::k);
    dp[10][j] = psi(j * cnst::k);
  }

  for (size_t j = 1; j <= 10; ++j)
    for (size_t i = 1; i < 10; ++i)
      dp[i][j + 1] = dp[i + 1][j] + dp[i - 1][j] - dp[i][j - 1];

  std::cout << table_hard_line << table_header << table_line;
  for (size_t j = 0; j <= 11; ++j) {
    if (j == 0)
      std::cout << cell_wrapper("v. layer");
    else
      std::cout << cell_wrapper("t=" + std::to_string(cnst::k * j));
    for (size_t i = 0; i <= 10; ++i)
      std::cout << cell_wrapper(dp[i][j]);
    std::cout << "|" << std::endl;
  }
  std::cout << table_hard_line;
  return 0;
}
