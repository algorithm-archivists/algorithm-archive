local function in_circle(x, y)
	return x*x + y*y <= 1
end

function monte_carlo(nsamples)
	local count = 0
	
	for i = 1,nsamples do
		if in_circle(math.random(), math.random()) then
			count = count + 1
		end
	end
	
	return 4 * count/nsamples
end