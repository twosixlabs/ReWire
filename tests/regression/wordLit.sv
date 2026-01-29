module top_level (input logic [7:0] __in0,
  output logic [15:0] __out0);
  logic [7:0] zll_main_loop2_in;
  logic [7:0] resize_in;
  logic [31:0] binop_in;
  logic [16:0] zll_main_loop_in;
  logic [16:0] zll_main_loop1_in;
  logic [0:0] __padding;
  assign zll_main_loop2_in = __in0;
  assign resize_in = zll_main_loop2_in[7:0];
  assign binop_in = {16'(resize_in[7:0]), 16'h1};
  assign zll_main_loop_in = {1'h0, binop_in[31:16] ^ binop_in[15:0]};
  assign zll_main_loop1_in = zll_main_loop_in[16:0];
  assign {__padding, __out0} = {1'h1, zll_main_loop1_in[15:0]};
endmodule