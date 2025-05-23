module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] zll_main_sig12_in;
  logic [17:0] zll_main_sig9_in;
  logic [16:0] zll_main_sig7_in;
  logic [16:0] zll_main_sig_in;
  logic [15:0] main_incr_in;
  logic [23:0] zll_main_incr35_in;
  logic [25:0] zll_main_incr35_out;
  logic [25:0] zll_main_incr29_in;
  logic [25:0] zll_main_incr26_in;
  logic [23:0] zll_main_incr38_in;
  logic [23:0] zll_main_incr35_inR1;
  logic [25:0] zll_main_incr35_outR1;
  logic [33:0] zll_main_incr19_in;
  logic [33:0] zll_main_incr36_in;
  logic [31:0] zll_main_incr17_in;
  logic [15:0] zll_main_incr16_in;
  logic [25:0] zll_main_incr16_out;
  logic [41:0] zll_main_incr11_in;
  logic [41:0] zll_main_incr32_in;
  logic [31:0] zll_main_incr13_in;
  logic [31:0] zll_main_incr30_in;
  logic [15:0] binop_in;
  logic [15:0] zll_main_incr16_inR1;
  logic [25:0] zll_main_incr16_outR1;
  logic [25:0] zll_main_incr6_in;
  logic [25:0] zll_main_incr31_in;
  logic [25:0] zll_main_incr31_out;
  logic [16:0] zll_main_incr31_inR1;
  logic [25:0] zll_main_incr31_outR1;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st1;
  logic [7:0] __st0_next;
  logic [7:0] __st1_next;
  assign zll_main_sig12_in = {__in0, {__st0, __st1}};
  assign zll_main_sig9_in = {zll_main_sig12_in[16], zll_main_sig12_in[16], zll_main_sig12_in[15:8], zll_main_sig12_in[7:0]};
  assign zll_main_sig7_in = {zll_main_sig9_in[17], zll_main_sig9_in[15:8], zll_main_sig9_in[7:0]};
  assign zll_main_sig_in = {zll_main_sig7_in[7:0], zll_main_sig7_in[15:8], zll_main_sig7_in[16]};
  assign main_incr_in = {zll_main_sig_in[8:1], zll_main_sig_in[16:9]};
  assign zll_main_incr35_in = {main_incr_in[15:8], main_incr_in[15:8], main_incr_in[7:0]};
  ZLL_Main_incr35  inst (zll_main_incr35_in[23:0], zll_main_incr35_out);
  assign zll_main_incr29_in = zll_main_incr35_out;
  assign zll_main_incr26_in = zll_main_incr29_in[25:0];
  assign zll_main_incr38_in = {zll_main_incr26_in[23:16], zll_main_incr26_in[15:8], zll_main_incr26_in[7:0]};
  assign zll_main_incr35_inR1 = {zll_main_incr38_in[7:0], zll_main_incr38_in[15:8], zll_main_incr38_in[7:0]};
  ZLL_Main_incr35  instR1 (zll_main_incr35_inR1[23:0], zll_main_incr35_outR1);
  assign zll_main_incr19_in = {zll_main_incr38_in[23:16], zll_main_incr35_outR1};
  assign zll_main_incr36_in = {zll_main_incr19_in[33:26], zll_main_incr19_in[25:0]};
  assign zll_main_incr17_in = {zll_main_incr36_in[33:26], zll_main_incr36_in[23:16], zll_main_incr36_in[15:8], zll_main_incr36_in[7:0]};
  assign zll_main_incr16_in = {zll_main_incr17_in[23:16], zll_main_incr17_in[7:0]};
  ZLL_Main_incr16  instR2 (zll_main_incr16_in[15:0], zll_main_incr16_out);
  assign zll_main_incr11_in = {zll_main_incr17_in[23:16], zll_main_incr17_in[31:24], zll_main_incr16_out};
  assign zll_main_incr32_in = {zll_main_incr11_in[41:34], zll_main_incr11_in[33:26], zll_main_incr11_in[25:0]};
  assign zll_main_incr13_in = {zll_main_incr32_in[41:34], zll_main_incr32_in[33:26], zll_main_incr32_in[15:8], zll_main_incr32_in[7:0]};
  assign zll_main_incr30_in = {zll_main_incr13_in[23:16], zll_main_incr13_in[31:24], zll_main_incr13_in[15:8], zll_main_incr13_in[7:0]};
  assign binop_in = {zll_main_incr30_in[31:24], zll_main_incr30_in[23:16]};
  assign zll_main_incr16_inR1 = {zll_main_incr30_in[15:8], binop_in[15:8] + binop_in[7:0]};
  ZLL_Main_incr16  instR3 (zll_main_incr16_inR1[15:0], zll_main_incr16_outR1);
  assign zll_main_incr6_in = zll_main_incr16_outR1;
  assign zll_main_incr31_in = zll_main_incr6_in[25:0];
  ZLL_Main_incr31  instR4 (zll_main_incr31_in[15:8], zll_main_incr31_in[7:0], zll_main_incr31_out);
  assign zll_main_incr31_inR1 = {zll_main_sig9_in[15:8], zll_main_sig9_in[7:0], zll_main_sig9_in[16]};
  ZLL_Main_incr31  instR5 (zll_main_incr31_inR1[16:9], zll_main_incr31_inR1[8:1], zll_main_incr31_outR1);
  assign {__continue, __padding, __out0, __st0_next, __st1_next} = (zll_main_incr31_inR1[0] == 1'h1) ? zll_main_incr31_outR1 : zll_main_incr31_out;
  initial {__st0, __st1} <= 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 16'h1;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_incr35 (input logic [23:0] arg0,
  output logic [25:0] res);
  logic [23:0] zll_main_incr22_in;
  logic [23:0] zll_main_incr15_in;
  assign zll_main_incr22_in = arg0;
  assign zll_main_incr15_in = {zll_main_incr22_in[15:8], zll_main_incr22_in[23:16], zll_main_incr22_in[7:0]};
  assign res = {2'h0, zll_main_incr15_in[15:8], zll_main_incr15_in[23:16], zll_main_incr15_in[7:0]};
endmodule

module ZLL_Main_incr31 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  logic [15:0] main_sig_in;
  logic [23:0] zll_main_sig5_in;
  logic [23:0] zll_main_sig11_in;
  logic [25:0] zll_main_sig2_in;
  logic [25:0] zll_main_sig10_in;
  logic [23:0] zll_main_sig6_in;
  assign main_sig_in = {arg0, arg1};
  assign zll_main_sig5_in = {main_sig_in[15:8], main_sig_in[15:8], main_sig_in[7:0]};
  assign zll_main_sig11_in = zll_main_sig5_in[23:0];
  assign zll_main_sig2_in = {2'h0, zll_main_sig11_in[23:16], zll_main_sig11_in[15:8], zll_main_sig11_in[7:0]};
  assign zll_main_sig10_in = zll_main_sig2_in[25:0];
  assign zll_main_sig6_in = {zll_main_sig10_in[23:16], zll_main_sig10_in[15:8], zll_main_sig10_in[7:0]};
  assign res = {2'h2, zll_main_sig6_in[23:16], zll_main_sig6_in[15:8], zll_main_sig6_in[7:0]};
endmodule

module ZLL_Main_incr16 (input logic [15:0] arg0,
  output logic [25:0] res);
  logic [15:0] zll_main_incr37_in;
  assign zll_main_incr37_in = arg0;
  assign res = {10'h100, zll_main_incr37_in[15:8], zll_main_incr37_in[7:0]};
endmodule