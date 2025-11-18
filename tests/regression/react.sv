module top_level (input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] zll_main_loop_in;
  logic [0:0] rewire_prelude_not_in;
  logic [1:0] zll_rewire_prelude_not1_in;
  logic [0:0] lit_in;
  logic [1:0] zll_main_loop1_in;
  logic [1:0] zll_main_loop2_in;
  logic [0:0] __continue;
  assign zll_main_loop_in = __in0;
  assign rewire_prelude_not_in = zll_main_loop_in[0];
  assign zll_rewire_prelude_not1_in = {rewire_prelude_not_in[0], rewire_prelude_not_in[0]};
  assign lit_in = zll_rewire_prelude_not1_in[0];
  assign zll_main_loop1_in = {1'h0, (lit_in[0] == 1'h1) ? 1'h0 : 1'h1};
  assign zll_main_loop2_in = zll_main_loop1_in[1:0];
  assign {__continue, __out0} = {1'h1, zll_main_loop2_in[0]};
endmodule