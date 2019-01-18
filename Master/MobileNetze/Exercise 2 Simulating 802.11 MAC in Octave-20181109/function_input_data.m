function [input, id, state] = function_input_data(input)

% Packet and frame sizes
input.mac_header = 272;       % MAC header size in bits
input.phy_header = 128;       % PHY header size in bits
input.ack_frame = 112 + input.phy_header;    % ACK frame size in bits
input.rts_frame = 160 + input.phy_header;    % RTS frame size in bits
input.cts_frame = 112 + input.phy_header;    % CTS frame size in bits

% Timing parameter
input.channel_bitrate = 1e6;                             	% Channel rate in bits-per-sec
input.slot_time = 50e-6;      % Slot time is 50 us
input.ack_time = ceil( (input.ack_frame / input.channel_bitrate) / input.slot_time) * input.slot_time;   % ACK frame time in sec
input.rts_time = ceil( (input.rts_frame / input.channel_bitrate) / input.slot_time) * input.slot_time;   % RTS frame time in sec
input.cts_time = ceil( (input.cts_frame / input.channel_bitrate) / input.slot_time) * input.slot_time;   % CTS frame time in sec
input.payload_time = ceil( ( (input.packet_payload + input.mac_header + input.phy_header) / input.channel_bitrate) / input.slot_time) * input.slot_time; 
input.sifs_time = ceil( 28e-6 / input.slot_time) * input.slot_time;      % SIFS time
input.difs_time = ceil( 128e-6 / input.slot_time) * input.slot_time;     % DIFS time
input.max_cw = length(input.cw_vec);
input.cw_cnt = zeros(1, input.no_nodes);                                 % Contention window count

input.tx_time = input.rts_time + input.sifs_time + input.cts_time + input.sifs_time + input.payload_time + input.sifs_time + input.ack_time;


%% Initiate event id to be saved
id.time = 1;
id.node = 2;
id.state = 3;
id.cw = 4;

state.rts = 1;
state.rts_sifs = 2;
state.cts = 3;
state.cts_sifs = 4;
state.payload = 5;
state.payload_sifs = 6;
state.ack = 7;
state.difs = 8;


