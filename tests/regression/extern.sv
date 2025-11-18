module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] zll_main_loop2_in;
  logic [15:0] mymod_in;
  logic [7:0] extres;
  logic [8:0] zll_main_loop_in;
  logic [8:0] zll_main_loop3_in;
  logic [0:0] __continue;
  assign zll_main_loop2_in = __in0;
  assign mymod_in = zll_main_loop2_in[15:0];
  mymod  inst (.clk(clk), .rst(rst), .x(mymod_in[15:0]), .out(extres[7:0]));
  assign zll_main_loop_in = {1'h0, extres};
  assign zll_main_loop3_in = zll_main_loop_in[8:0];
  assign {__continue, __out0} = {1'h1, zll_main_loop3_in[7:0]};
endmodule