A = 1 : 10
B = randi([1 100], 1 , length(A))
C = randi([1 100], 1 , length(A))
D = randi([1 100], 1 , length(A))

figure(1)
grid on; box on;
hold on
bar(A, B, 'w')
stem(A, C, '-xm', 'linewidth', 2)
plot(A, D, '-ob', 'linewidth', 2)

legend('Data B', 'Data C', 'Data D')
xlabel('Value of A')
ylabel('Value of data')
axis([0 11 0 110])
title('Test figure')

