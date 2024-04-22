% set random seed
rng(27);

% Polya urn
clear;
runs = 30;
draws = 100;
data = zeros(runs,draws);
for run = 1:runs
    balls = [-1 1];
    for draw = 1:draws
        balls = [balls balls(1,ceil(rand * length(balls)))];
    end
    data(run,1:draws) = cumsum(balls(3:end));
end
% writematrix(data,'Polya_urn.csv');

% Polya urn twin
clear;
runs = 30;
talent = rand(runs,1);
draws = 100;
data = cumsum((2 * (rand(runs,draws) < talent) - 1)')';
% writematrix(data,'Polya_urn_twin.csv');

% Q model
clear;
runs = 30;
talent = randn(runs,1) / sqrt(5);
draws = 100;
data = talent + randn(runs,draws);
writematrix(cumsum(exp(data)')','Q_model.csv');

% Q model twin
clear;
runs = 30;
draws = 100;
a = 0;
b = 1;
c = 5;
for run = 1:runs
    data(run,1) = (b + b / c) * randn + a;
    for draw = 2:draws
        data(run,draw) = (b * (1 + 1 / (draw - 1 + c))) * randn + (c * a + sum(data(run,1:draw - 1))) / (draw + c);
    end
end
writematrix(cumsum(exp(data)')','Q_model_twin.csv');

% SD model
clear;
runs = 30;
talent = randn(runs,1);
draws = 100;
data = talent + randn(runs,draws) / 10;
% writematrix(data,'SD_model.csv');

% SD model twin
clear;
runs = 30;
draws = 100;
a = 0;
b = 1;
c = 1;
for run = 1:runs
    data(run,1) = (b + b / c) * randn + a;
    for draw = 2:draws
        data(run,draw) = (b * (1 + 1 / (draw - 1 + c))) * randn + (c * a + sum(data(run,1:draw - 1))) / (draw + c);
    end
end
% writematrix(data,'SD_model_twin.csv');

% Contagious Poisson
clear;
runs = 30;
draws = 100;
a = .3;
b = a;
t(1:runs,1:draws) = 0;
for draw = 1:draws
    for run = 1:runs
        t(run,draw) = -log(rand) / (a + b * (draw-1));
    end
end
for episode = 1:10
    data(1:runs,episode) = sum((cumsum(t')' <= episode)')';
end
% writematrix(data,'Contagious_Poisson.csv');

% Contagious Poisson twin
% note: I impose b = a, so that I can use the exponential distribution as
% special case of gamma -- otherwise I need rndgam for random number
% generation which is a Matlab package i dont have right now
clear;
runs = 30;
a = .3;
b = a;
talent = -log(rand(runs,1)) / (b^-1);
draws = 100;
t(1:runs,1:draws) = -log(rand(runs,draws)) ./ talent;
for episode = 1:10
    data(1:runs,episode) = sum((cumsum(t')' <= ((exp(b * episode) - 1) / b))')';
end
% writematrix(data,'Contagious_Poisson_twin.csv');