module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] zll_pure_dispatch_in;
  logic [3:0] zll_pure_dispatch_out;
  logic [2:0] zll_pure_dispatch_inR1;
  logic [3:0] zll_pure_dispatch_outR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  assign zll_pure_dispatch_in = {__in0, {__resumption_tag, __st0}};
  ZLL_Pure_dispatch  inst (zll_pure_dispatch_in[2], zll_pure_dispatch_in[0], zll_pure_dispatch_out);
  assign zll_pure_dispatch_inR1 = {__in0, {__resumption_tag, __st0}};
  ZLL_Pure_dispatch  instR1 (zll_pure_dispatch_inR1[2], zll_pure_dispatch_inR1[0], zll_pure_dispatch_outR1);
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = (zll_pure_dispatch_inR1[1] == 1'h1) ? zll_pure_dispatch_outR1 : zll_pure_dispatch_out;
  initial {__resumption_tag, __st0} <= 2'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 2'h0;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module ZLL_Pure_dispatch (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [3:0] res);
  logic [1:0] lit_in;
  assign lit_in = {arg0, arg1};
  assign res = 4'ha;
endmodule