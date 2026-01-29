module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] zll_main_sig12_in;
  logic [17:0] zll_main_sig_in;
  logic [16:0] zll_main_sig7_in;
  logic [16:0] zll_main_sig6_in;
  logic [15:0] main_sig_in;
  logic [25:0] main_sig_out;
  logic [16:0] zll_main_sig1_in;
  logic [15:0] main_incr_in;
  logic [23:0] zll_main_incr37_in;
  logic [25:0] zll_main_incr37_out;
  logic [25:0] zll_main_incr13_in;
  logic [25:0] zll_main_incr36_in;
  logic [23:0] zll_main_incr29_in;
  logic [23:0] zll_main_incr37_inR1;
  logic [25:0] zll_main_incr37_outR1;
  logic [33:0] zll_main_incr33_in;
  logic [33:0] zll_main_incr19_in;
  logic [31:0] zll_main_incr11_in;
  logic [31:0] zll_main_incr6_in;
  logic [31:0] zll_main_incr16_in;
  logic [15:0] zll_main_incr32_in;
  logic [25:0] zll_main_incr32_out;
  logic [41:0] zll_main_incr22_in;
  logic [41:0] zll_main_incr5_in;
  logic [31:0] zll_main_incr3_in;
  logic [31:0] zll_main_incr20_in;
  logic [15:0] binop_in;
  logic [15:0] zll_main_incr32_inR1;
  logic [25:0] zll_main_incr32_outR1;
  logic [25:0] zll_main_incr14_in;
  logic [25:0] zll_main_incr30_in;
  logic [15:0] main_sig_inR1;
  logic [25:0] main_sig_outR1;
  logic [1:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st1;
  logic [7:0] __st0_next;
  logic [7:0] __st1_next;
  assign zll_main_sig12_in = {__in0, {__st0, __st1}};
  assign zll_main_sig_in = {zll_main_sig12_in[16], zll_main_sig12_in[16], zll_main_sig12_in[15:8], zll_main_sig12_in[7:0]};
  assign zll_main_sig7_in = {zll_main_sig_in[17], zll_main_sig_in[15:8], zll_main_sig_in[7:0]};
  assign zll_main_sig6_in = {zll_main_sig7_in[7:0], zll_main_sig7_in[15:8], zll_main_sig7_in[16]};
  assign main_sig_in = {zll_main_sig6_in[8:1], zll_main_sig6_in[16:9]};
  Main_sig  inst (main_sig_in[15:8], main_sig_in[7:0], main_sig_out);
  assign zll_main_sig1_in = {zll_main_sig_in[7:0], zll_main_sig_in[15:8], zll_main_sig_in[16]};
  assign main_incr_in = {zll_main_sig1_in[8:1], zll_main_sig1_in[16:9]};
  assign zll_main_incr37_in = {main_incr_in[15:8], main_incr_in[15:8], main_incr_in[7:0]};
  ZLL_Main_incr37  instR1 (zll_main_incr37_in[23:0], zll_main_incr37_out);
  assign zll_main_incr13_in = zll_main_incr37_out;
  assign zll_main_incr36_in = zll_main_incr13_in[25:0];
  assign zll_main_incr29_in = {zll_main_incr36_in[23:16], zll_main_incr36_in[15:8], zll_main_incr36_in[7:0]};
  assign zll_main_incr37_inR1 = {zll_main_incr29_in[7:0], zll_main_incr29_in[15:8], zll_main_incr29_in[7:0]};
  ZLL_Main_incr37  instR2 (zll_main_incr37_inR1[23:0], zll_main_incr37_outR1);
  assign zll_main_incr33_in = {zll_main_incr29_in[23:16], zll_main_incr37_outR1};
  assign zll_main_incr19_in = {zll_main_incr33_in[33:26], zll_main_incr33_in[25:0]};
  assign zll_main_incr11_in = {zll_main_incr19_in[33:26], zll_main_incr19_in[23:16], zll_main_incr19_in[15:8], zll_main_incr19_in[7:0]};
  assign zll_main_incr6_in = {zll_main_incr11_in[23:16], zll_main_incr11_in[31:24], zll_main_incr11_in[15:8], zll_main_incr11_in[7:0]};
  assign zll_main_incr16_in = {zll_main_incr6_in[23:16], zll_main_incr6_in[31:24], zll_main_incr6_in[15:8], zll_main_incr6_in[7:0]};
  assign zll_main_incr32_in = {zll_main_incr16_in[23:16], zll_main_incr16_in[7:0]};
  ZLL_Main_incr32  instR3 (zll_main_incr32_in[15:0], zll_main_incr32_out);
  assign zll_main_incr22_in = {zll_main_incr16_in[23:16], zll_main_incr16_in[31:24], zll_main_incr32_out};
  assign zll_main_incr5_in = {zll_main_incr22_in[41:34], zll_main_incr22_in[33:26], zll_main_incr22_in[25:0]};
  assign zll_main_incr3_in = {zll_main_incr5_in[41:34], zll_main_incr5_in[33:26], zll_main_incr5_in[15:8], zll_main_incr5_in[7:0]};
  assign zll_main_incr20_in = {zll_main_incr3_in[23:16], zll_main_incr3_in[31:24], zll_main_incr3_in[15:8], zll_main_incr3_in[7:0]};
  assign binop_in = {zll_main_incr20_in[31:24], zll_main_incr20_in[23:16]};
  assign zll_main_incr32_inR1 = {zll_main_incr20_in[15:8], binop_in[15:8] + binop_in[7:0]};
  ZLL_Main_incr32  instR4 (zll_main_incr32_inR1[15:0], zll_main_incr32_outR1);
  assign zll_main_incr14_in = zll_main_incr32_outR1;
  assign zll_main_incr30_in = zll_main_incr14_in[25:0];
  assign main_sig_inR1 = {zll_main_incr30_in[15:8], zll_main_incr30_in[7:0]};
  Main_sig  instR5 (main_sig_inR1[15:8], main_sig_inR1[7:0], main_sig_outR1);
  assign {__padding, __out0, __st0_next, __st1_next} = (zll_main_sig1_in[0] == 1'h1) ? main_sig_outR1 : main_sig_out;
  initial {__st0, __st1} = 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 16'h1;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_incr37 (input logic [23:0] arg0,
  output logic [25:0] res);
  logic [23:0] zll_main_incr38_in;
  assign zll_main_incr38_in = arg0;
  assign res = {2'h0, zll_main_incr38_in[23:16], zll_main_incr38_in[15:8], zll_main_incr38_in[7:0]};
endmodule

module ZLL_Main_incr32 (input logic [15:0] arg0,
  output logic [25:0] res);
  logic [15:0] zll_main_incr17_in;
  assign zll_main_incr17_in = arg0;
  assign res = {10'h100, zll_main_incr17_in[15:8], zll_main_incr17_in[7:0]};
endmodule

module Main_sig (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  logic [23:0] zll_main_incr37_in;
  logic [25:0] zll_main_incr37_out;
  logic [25:0] zll_main_sig11_in;
  logic [25:0] zll_main_sig4_in;
  logic [23:0] zll_main_sig3_in;
  assign zll_main_incr37_in = {arg0, arg0, arg1};
  ZLL_Main_incr37  inst (zll_main_incr37_in[23:0], zll_main_incr37_out);
  assign zll_main_sig11_in = zll_main_incr37_out;
  assign zll_main_sig4_in = zll_main_sig11_in[25:0];
  assign zll_main_sig3_in = {zll_main_sig4_in[23:16], zll_main_sig4_in[15:8], zll_main_sig4_in[7:0]};
  assign res = {2'h2, zll_main_sig3_in[23:16], zll_main_sig3_in[15:8], zll_main_sig3_in[7:0]};
endmodule