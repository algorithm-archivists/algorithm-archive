clc;clear;close all

%==========================================================================
% Define Function
f =@(x) -3*x;

% Define Initial and Final time
tInit=0;
tLast=5;

% Define number of points
N=1e2;

% Set Initial Conditions
yInit=1;
%==========================================================================

dt=(tLast-tInit)/(N-1); % Calculate dt
t=[tInit:dt:tLast];     % Preallocate time array
y=zeros(1,length(t));   % Preallocate solution array
y(1)=yInit;             % Impose Initial Conditions

% Loop over time
for i=1:length(t)-1
    
    t(i+1) = t(i) + dt;             % Calculate next time 
    y(i+1) = y(i) + f( y(i) )*dt;   % Update solution
    
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
