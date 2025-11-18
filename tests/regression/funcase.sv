module top_level (input logic [1:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] zll_main_proc4_in;
  logic [3:0] zll_main_proc_in;
  logic [3:0] zll_main_proc2_in;
  logic [1:0] zll_main_proc7_in;
  logic [1:0] lit_in;
  logic [1:0] lit_inR1;
  logic [1:0] lit_inR2;
  logic [0:0] __continue;
  assign zll_main_proc4_in = __in0;
  assign zll_main_proc_in = {zll_main_proc4_in[1:0], zll_main_proc4_in[1:0]};
  assign zll_main_proc2_in = {zll_main_proc_in[3:2], zll_main_proc_in[3:2]};
  assign zll_main_proc7_in = zll_main_proc2_in[3:2];
  assign lit_in = zll_main_proc7_in[1:0];
  assign lit_inR1 = zll_main_proc2_in[1:0];
  assign lit_inR2 = zll_main_proc_in[1:0];
  assign {__continue, __out0} = (lit_inR2[1:0] == 2'h0) ? 1'h0 : ((lit_inR1[1:0] == 2'h1) ? 1'h0 : ((lit_in[1:0] == 2'h2) ? 1'h0 : 1'h1));
endmodule