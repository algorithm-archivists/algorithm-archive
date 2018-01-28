% Submitted by P. Mekhail
% Parameters to change
n = 400;        % Number of steps
x0 = 5;         % Ball starting height(in metres)
v0 = 0;         % Ball starting velocity (+ive is up)
dt = 0.01;      % Time step (in seconds)
eff = 0.4;      % Ball efficency when bouncing
A = @(x) -10;   % Acceleration as a function of position
bounce = 1;     % Do you want the ball to bounce?

% Making position and time vectors
x = zeros(n,1);
t = 0:dt:n*dt-dt;

% Setting the initial conditions
x(1) = x0;
x(2) = x0 + v0*dt + 0.5*A(x0)*dt^2;

% Runnin Verlet Integration
for i = 2:n-1
    xnew = 2*x(i)-x(i-1)+A(x(i))*dt^2;
    if bounce
        if xnew > 0
            % If you haven't hit the ground keep going
            x(i+1) = xnew;
        else
            % If you have calculated velocity and invert its sign
            v = sqrt(eff)*(xnew-x(i-1))/(2*dt);
            x(i+1) = x(i) - v*dt + 0.5*A(x(i))*dt^2;
        end
    else
        x(i+1) = xnew;
    end
end
plot(t,x)
title('Ball''s Trajectory')
xlabel('Time (s)'); ylabel('Height (m)');

