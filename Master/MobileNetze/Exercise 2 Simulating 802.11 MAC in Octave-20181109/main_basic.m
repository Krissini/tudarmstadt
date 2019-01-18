% System parameters are based on Bianchi's paper: Performance Analysis of the IEEE 802.11 Distributed Coordination Function

% das kann schon: outout geben. die "5" links it wenn ich data sende. Y-Achse ist simulation time

clc
clear
close

%% Student ID (Only input the last 2 digits)
% Name : 
% Matriculation No. : 

%% Input parameters

% Adjustable parameters 
input.no_nodes = 1;                                              % Number of wifi-nodes 
input.tx_prob = 0.2;                                             % Transmission probability
input.packet_payload = 4000;                                     % packet size in bits
input.cw_vec = [8 16 32 64 128 256 512 1024];                    % Vector for the contention window
input.simulation_time = 50e-3;                                   % Simulation time in second

% Fixed parameters
[input, id, state] = function_input_data(input);                 % Defined the input parameteres : Do not change this file! 

%% Initial access
% Determine the time at which a node access the channel
s = 1;
input.access = rand( 1, input.no_nodes);
for n = 1 : input.no_nodes
   while input.access(n) > input.tx_prob
      s = s + 1;
      input.access(n) = rand;
   end
   input.access_slot(n) = s;
end

% Simulation events
data.event(:, id.time) = input.access_slot .* input.slot_time;   % access time
data.event(:, id.node) = 1:input.no_nodes;                       % nodes index
data.event(:, id.state) = state.payload;                         % nodes' state
data.event(:, id.cw) = input.cw_cnt;                             % nodes' contention window
data.event = sortrows(data.event, 1);                            % Sorting events such that the one with the earliest time occur first
data.event_time = data.event(1,1);                               % The time of the first event
data.event_bank = [];

%% Start: Event-based simulator
while data.event_time < input.simulation_time
   
   current_event = data.event(1, :);
   n = current_event(id.node);
   new_event = current_event;
   
   switch current_event(id.state)   
      
      case state.payload
         new_event(id.time) = current_event(id.time) + input.payload_time;
         new_event(id.state) = state.payload_sifs;
         
      case state.payload_sifs
         new_event(id.time) = current_event(id.time) + input.sifs_time;
         new_event(id.state) = state.ack;
         
      case state.ack
         new_event(id.time) = current_event(id.time) + input.ack_time;
         new_event(id.state) = state.difs;
         
      case state.difs
         backoff_time = randi ( min(input.cw_vec) ) * input.slot_time;
         % When will it access the channel again?
         node_access = 0;
         while node_access > input.tx_prob
            s = s + 1;
            node_access = rand;
         end
         new_event(id.time) = current_event(id.time) + s*input.slot_time + input.difs_time + backoff_time;
         new_event(id.state) = state.payload;
         new_event(id.cw) = 1;
         
   end
   
   data.event(1,:) = [];
   data.event = sortrows( [data.event; new_event], 1);
   data_event = data.event;
   data.event_bank = [data.event_bank; current_event];
   data.event_time = data.event(1);
   disp_event = data.event;
   
end

%% Plotting graphs
node_event = find(data.event_bank(:, 2) == 1);
figure(1)
hold on
box on; grid on
stem(data.event_bank(node_event,1), data.event_bank(node_event,3), ':b*', 'LineWidth', 1, 'MarkerSize', 6)

axis([0 max(data.event_bank(:,1)) + 10*input.slot_time 0 9])
legend('Node 1')
xlabel('Simulation time (sec)')
ylabel('State')

ytick = [5:8];
set(gca, 'ytick', ytick);
yticklabel = ["Payload";"Payload SIFS"; "ACK"; "DIFS"];
set(gca, 'yticklabel', yticklabel)
































