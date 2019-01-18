function velB = worldToBody(velW,pose)
% WORLD TO BODY Converts velocity from world to body coordinates
%
% Copyright 2018 The MathWorks, Inc.

    theta = pose(3);
    velB = [cos(theta) sin(theta) 0;-sin(theta) cos(theta) 0;0 0 1]*velW;

end
