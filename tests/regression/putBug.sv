module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] zll_pure_dispatch1_in;
  logic [2:0] zll_main_convtest10_in;
  logic [0:0] zll_main_convtest_in;
  logic [4:0] zll_main_convtest15_in;
  logic [4:0] zll_main_convtest14_in;
  logic [1:0] zll_main_convtest7_in;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  assign zll_pure_dispatch1_in = {__in0, {__resumption_tag, __st0}};
  assign zll_main_convtest10_in = {zll_pure_dispatch1_in[1], zll_pure_dispatch1_in[2], zll_pure_dispatch1_in[0]};
  assign zll_main_convtest_in = zll_main_convtest10_in[2];
  assign zll_main_convtest15_in = {zll_main_convtest10_in[2], {3'h0, zll_main_convtest_in[0]}};
  assign zll_main_convtest14_in = {zll_main_convtest15_in[4], zll_main_convtest15_in[3:0]};
  assign zll_main_convtest7_in = {zll_main_convtest14_in[4], zll_main_convtest14_in[0]};
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = {1'h1, zll_main_convtest7_in[1], zll_main_convtest7_in[1], zll_main_convtest7_in[0]};
  initial {__resumption_tag, __st0} <= 2'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 2'h0;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule