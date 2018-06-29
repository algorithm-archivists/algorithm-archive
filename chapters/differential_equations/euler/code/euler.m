clc;clear;close all

% Author : vasilisk075

%==========================================================================
% Define Function
f =@(x) -3*x;

% Define Initial and Final time
tInit=0;
tLast=10;

% Define number of points
N=1e2;

% Set Initial Conditions
y(1)=1;
t(1)=tInit;

% Set tolerance
tol=1e-6;
%==========================================================================

dt=(tLast-tInit)/(N-1); % Calculate dt
i=1;                    % Initialize iter
succes=0;               % Initialize succes variable

% Loop over time
while succes==0
    
    t(i+1) = t(i) + dt;             % Calculate next time
    y(i+1) = y(i) + f( y(i) )*dt;   % Update solution
    
    % Calculate differce between 2 time steps
    error=abs(( y(i+1) - y(i) )/dt); 
    
    % If difference is small enough or max time is reached stop
    if (error<tol) || (t(i+1)>=tLast)
        succes=1;
        break
    end
    
    % Update counter
    i=i+1;
end

% Plot numerical solution
plot(t,y)

% Create analytical solution
g=@(x) exp(-3*x);
z=g(t);

% Plot analytical solution on the same graph
hold on
plot(t,z,'--')

% Set axis, title and legend
xlabel('t');ylabel('y(t)');
title('Analytical VS Numerical Solution')
grid
legend('Numerical','Analytical')
