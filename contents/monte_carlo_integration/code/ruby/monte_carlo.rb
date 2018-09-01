def in_circle(x, y, radius=1)
  # Check if coords are in circle via Pythagorean Thm
  return (x*x + y*y) < radius*radius
end

def monte_carlo(n_samples, radius=1)
  # estimate pi via monte carlo sampling
  in_circle_count = 0.0
  
  for _ in 0...n_samples
    # randomly choose coords within square
	x = rand()*radius
	y = rand()*radius
	if in_circle(x, y, radius)
	  in_circle_count += 1
	end
  end
  
  # circle area is pi*r^2 and rect area is 4r^2
  # ratio between the two is then pi/4 so multiply by 4 to get pi
  return 4 * (in_circle_count / n_samples)
  
end


# Main
pi_estimate = monte_carlo(100000)
percent_error = 100 * (pi_estimate - Math::PI).abs / Math::PI

puts "The estimate of pi is: #{pi_estimate.round(3)}"
puts "The percent error is: #{percent_error.round(3)}"

