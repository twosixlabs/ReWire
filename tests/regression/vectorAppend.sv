module top_level (input logic [63:0] __in0,
  input logic [63:0] __in1,
  output logic [63:0] __out0);
  logic [127:0] zll_main_loop2_in;
  logic [127:0] zll_main_compute1_in;
  logic [127:0] zll_main_compute2_in;
  logic [63:0] id_in;
  logic [31:0] reverse_in;
  logic [63:0] id_inR1;
  logic [64:0] zll_main_loop1_in;
  logic [64:0] zll_main_loop_in;
  logic [0:0] __continue;
  assign zll_main_loop2_in = {__in0, __in1};
  assign zll_main_compute1_in = zll_main_loop2_in[127:0];
  assign zll_main_compute2_in = zll_main_compute1_in[127:0];
  assign id_in = zll_main_compute2_in[127:64];
  assign reverse_in = id_in[63:32];
  assign id_inR1 = zll_main_compute2_in[63:0];
  assign zll_main_loop1_in = {1'h0, {{reverse_in[7:0], reverse_in[15:8], reverse_in[23:16], reverse_in[31:24]}, 8'h0, id_inR1[23:0]}};
  assign zll_main_loop_in = zll_main_loop1_in[64:0];
  assign {__continue, __out0} = {1'h1, zll_main_loop_in[63:0]};
endmodule