#pragma once
#include <functional>
#include <iostream>

void try_block(auto &&throwable) {
  try {
    std::invoke(throwable);
  } catch (std::exception &err) {
    std::cerr << err.what() << std::endl;
  }
}
