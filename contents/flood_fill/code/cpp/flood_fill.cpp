#include <array>
#include <cassert>
#include <iostream>
#include <queue>
#include <stack>
#include <vector>

using CartesianIndex = std::array<int, 2>;

auto inbounds(CartesianIndex size, CartesianIndex loc) {
  if (loc[0] < 0 || loc[1] < 0) {
    return false;
  } else if (loc[0] >= size[0] || loc[1] >= size[1]) {
    return false;
  }
  return true;
}

auto find_neighbors(
    std::vector<std::vector<float>> const& grid,
    CartesianIndex loc,
    float old_value,
    float /* new_value */) {

  const std::vector<CartesianIndex> possible_neighbors{
      {loc[0], loc[1] + 1},
      {loc[0] + 1, loc[1]},
      {loc[0], loc[1] - 1},
      {loc[0] - 1, loc[1]}};

  std::vector<CartesianIndex> neighbors;

  for (auto const& possible_neighbor : possible_neighbors) {
    const auto size = CartesianIndex{
        static_cast<int>(grid[0].size()), static_cast<int>(grid.size())};
    const auto x = static_cast<std::size_t>(possible_neighbor[0]);
    const auto y = static_cast<std::size_t>(possible_neighbor[1]);
    if (inbounds(size, possible_neighbor) && grid[x][y] == old_value) {
      neighbors.push_back(possible_neighbor);
    }
  }

  return neighbors;
}

void recursive_fill(
    std::vector<std::vector<float>>& grid,
    CartesianIndex loc,
    float old_value,
    float new_value) {
  if (old_value == new_value) {
    return;
  }

  const auto x = static_cast<std::size_t>(loc[0]);
  const auto y = static_cast<std::size_t>(loc[1]);

  grid[x][y] = new_value;

  const auto possible_neighbors = find_neighbors(grid, loc, old_value, new_value);
  for (auto const& possible_neighbor : possible_neighbors) {
    recursive_fill(grid, possible_neighbor, old_value, new_value);
  }
}

void queue_fill(
    std::vector<std::vector<float>>& grid,
    CartesianIndex loc,
    float old_value,
    float new_value) {
  if (old_value == new_value) {
    return;
  }

  auto q = std::queue<CartesianIndex>{};
  q.push(loc);
  const auto x = static_cast<std::size_t>(loc[0]);
  const auto y = static_cast<std::size_t>(loc[1]);
  grid[x][y] = new_value;

  while (q.size() > 0) {
    const auto current_loc = q.front();
    q.pop();
    const auto possible_neighbors =
        find_neighbors(grid, current_loc, old_value, new_value);
    for (auto const& neighbor : possible_neighbors) {
      const auto neighbor_x = static_cast<std::size_t>(neighbor[0]);
      const auto neighbor_y = static_cast<std::size_t>(neighbor[1]);
      grid[neighbor_x][neighbor_y] = new_value;
      q.push(neighbor);
    }
  }
}

void stack_fill(
    std::vector<std::vector<float>>& grid,
    CartesianIndex loc,
    float old_value,
    float new_value) {
  if (old_value == new_value) {
    return;
  }

  auto s = std::stack<CartesianIndex>{};
  s.push(loc);

  while (s.size() > 0) {
    const auto current_loc = s.top();
    s.pop();

    const auto x = static_cast<std::size_t>(current_loc[0]);
    const auto y = static_cast<std::size_t>(current_loc[1]);

    if (grid[x][y] == old_value) {
      grid[x][y] = new_value;
      const auto possible_neighbors =
          find_neighbors(grid, current_loc, old_value, new_value);
      for (auto const& neighbor : possible_neighbors) {
        s.push(neighbor);
      }
    }
  }
}

int main() {

  const std::vector<std::vector<float>> grid{
      {0, 0, 1, 0, 0},
      {0, 0, 1, 0, 0},
      {0, 0, 1, 0, 0},
      {0, 0, 1, 0, 0},
      {0, 0, 1, 0, 0}};

  const std::vector<std::vector<float>> solution_grid{
      {1, 1, 1, 0, 0},
      {1, 1, 1, 0, 0},
      {1, 1, 1, 0, 0},
      {1, 1, 1, 0, 0},
      {1, 1, 1, 0, 0}};

  const CartesianIndex start_loc{1, 1};

  auto test_grid = grid;
  recursive_fill(test_grid, start_loc, 0.0, 1.0);
  assert(test_grid == solution_grid);

  test_grid = grid;
  queue_fill(test_grid, start_loc, 0.0, 1.0);
  assert(test_grid == solution_grid);

  test_grid = grid;
  stack_fill(test_grid, start_loc, 0.0, 1.0);
  assert(test_grid == solution_grid);

  return EXIT_SUCCESS;
}