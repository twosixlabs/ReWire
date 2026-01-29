module top_level (input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [0:0] rewire_prelude_not1_in;
  logic [1:0] zll_rewire_prelude_not_in;
  logic [0:0] lit_in;
  assign rewire_prelude_not1_in = __in0;
  assign zll_rewire_prelude_not_in = {rewire_prelude_not1_in[0], rewire_prelude_not1_in[0]};
  assign lit_in = zll_rewire_prelude_not_in[0];
  assign __out0 = (lit_in[0] == 1'h1) ? 1'h0 : 1'h1;
endmodule