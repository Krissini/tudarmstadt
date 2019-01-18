clear 
clc
% y = zeros(1, 5);
for x = 1 : 5
   y(x) = x * 2;
   X = y(x);
   
   [output(x)] = function_example_1(X)
   
end

y