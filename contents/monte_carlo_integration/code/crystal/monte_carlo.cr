# Method to determine whether an x, y point is in the unit circle.
def in_circle(x_pos : Float64, y_pos : Float64)
  # Setting radius to 1 for unit circle
  radius = 1
  x_pos ** 2 + y_pos ** 2 < radius ** 2
end

# Method to integrate a unit circle to find pi via monte_carlo
def monte_carlo(n : Int)
  pi_count = 0
  n.times do
    point_x = rand
    point_y = rand

    if in_circle(point_x, point_y)
      pi_count += 1
    end
  end

  # This is using a quarter of the unit sphere in a 1x1 box.
  # The formula is pi = (box_length^2 / radius^2) * (pi_count / n), but we
  #     are only using the upper quadrant and the unit circle, so we can use
  #     4*pi_count/n instead
  4 * pi_count / n
end

pi_estimate = monte_carlo(10000000)
puts "The pi estimate is: #{pi_estimate}"
puts "Percent error is: #{100 * (pi_estimate - Math::PI).abs / Math::PI} %"
