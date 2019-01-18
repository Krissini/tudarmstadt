function velW = bodyToWorld(velB,pose)
% BODYTOWORLD Converts velocity from body to world coordinates
%
% Copyright 2018 The MathWorks, Inc.

    theta = pose(3);
    velW = [cos(theta) -sin(theta) 0; sin(theta) cos(theta) 0;0 0 1]*velB;

end
