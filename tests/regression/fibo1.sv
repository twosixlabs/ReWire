module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] zll_main_sig9_in;
  logic [16:0] zll_main_sig3_in;
  logic [15:0] main_incr_in;
  logic [23:0] zll_main_incr28_in;
  logic [25:0] zll_main_incr28_out;
  logic [25:0] zll_main_incr12_in;
  logic [25:0] zll_main_incr3_in;
  logic [23:0] zll_main_incr20_in;
  logic [23:0] zll_main_incr7_in;
  logic [23:0] zll_main_incr24_in;
  logic [33:0] zll_main_incr16_in;
  logic [33:0] zll_main_incr5_in;
  logic [31:0] zll_main_incr30_in;
  logic [31:0] zll_main_incr11_in;
  logic [31:0] zll_main_incr22_in;
  logic [15:0] zll_main_incr14_in;
  logic [25:0] zll_main_incr14_out;
  logic [41:0] zll_main_incr33_in;
  logic [41:0] zll_main_incr23_in;
  logic [31:0] zll_main_incr31_in;
  logic [31:0] zll_main_incr21_in;
  logic [15:0] binop_in;
  logic [15:0] zll_main_incr14_inR1;
  logic [25:0] zll_main_incr14_outR1;
  logic [25:0] zll_main_incr8_in;
  logic [25:0] zll_main_begin14_in;
  logic [25:0] zll_main_begin14_out;
  logic [16:0] zll_main_begin14_inR1;
  logic [25:0] zll_main_begin14_outR1;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st1;
  logic [7:0] __st0_next;
  logic [7:0] __st1_next;
  assign zll_main_sig9_in = {__in0, {__st0, __st1}};
  assign zll_main_sig3_in = {zll_main_sig9_in[15:8], zll_main_sig9_in[7:0], zll_main_sig9_in[16]};
  assign main_incr_in = {zll_main_sig3_in[16:9], zll_main_sig3_in[8:1]};
  assign zll_main_incr28_in = {main_incr_in[15:8], main_incr_in[15:8], main_incr_in[7:0]};
  ZLL_Main_incr28  inst (zll_main_incr28_in[23:0], zll_main_incr28_out);
  assign zll_main_incr12_in = zll_main_incr28_out;
  assign zll_main_incr3_in = zll_main_incr12_in[25:0];
  assign zll_main_incr20_in = {zll_main_incr3_in[23:16], zll_main_incr3_in[15:8], zll_main_incr3_in[7:0]};
  assign zll_main_incr7_in = {zll_main_incr20_in[7:0], zll_main_incr20_in[15:8], zll_main_incr20_in[7:0]};
  assign zll_main_incr24_in = zll_main_incr7_in[23:0];
  assign zll_main_incr16_in = {zll_main_incr20_in[23:16], {2'h0, zll_main_incr24_in[23:16], zll_main_incr24_in[15:8], zll_main_incr24_in[7:0]}};
  assign zll_main_incr5_in = {zll_main_incr16_in[33:26], zll_main_incr16_in[25:0]};
  assign zll_main_incr30_in = {zll_main_incr5_in[33:26], zll_main_incr5_in[23:16], zll_main_incr5_in[15:8], zll_main_incr5_in[7:0]};
  assign zll_main_incr11_in = {zll_main_incr30_in[23:16], zll_main_incr30_in[31:24], zll_main_incr30_in[15:8], zll_main_incr30_in[7:0]};
  assign zll_main_incr22_in = {zll_main_incr11_in[23:16], zll_main_incr11_in[31:24], zll_main_incr11_in[15:8], zll_main_incr11_in[7:0]};
  assign zll_main_incr14_in = {zll_main_incr22_in[23:16], zll_main_incr22_in[7:0]};
  ZLL_Main_incr14  instR1 (zll_main_incr14_in[15:0], zll_main_incr14_out);
  assign zll_main_incr33_in = {zll_main_incr22_in[23:16], zll_main_incr22_in[31:24], zll_main_incr14_out};
  assign zll_main_incr23_in = {zll_main_incr33_in[41:34], zll_main_incr33_in[33:26], zll_main_incr33_in[25:0]};
  assign zll_main_incr31_in = {zll_main_incr23_in[41:34], zll_main_incr23_in[33:26], zll_main_incr23_in[15:8], zll_main_incr23_in[7:0]};
  assign zll_main_incr21_in = {zll_main_incr31_in[23:16], zll_main_incr31_in[31:24], zll_main_incr31_in[15:8], zll_main_incr31_in[7:0]};
  assign binop_in = {zll_main_incr21_in[31:24], zll_main_incr21_in[23:16]};
  assign zll_main_incr14_inR1 = {zll_main_incr21_in[15:8], binop_in[15:8] + binop_in[7:0]};
  ZLL_Main_incr14  instR2 (zll_main_incr14_inR1[15:0], zll_main_incr14_outR1);
  assign zll_main_incr8_in = zll_main_incr14_outR1;
  assign zll_main_begin14_in = zll_main_incr8_in[25:0];
  ZLL_Main_begin14  instR3 (zll_main_begin14_in[15:8], zll_main_begin14_in[7:0], zll_main_begin14_out);
  assign zll_main_begin14_inR1 = {zll_main_sig9_in[15:8], zll_main_sig9_in[7:0], zll_main_sig9_in[16]};
  ZLL_Main_begin14  instR4 (zll_main_begin14_inR1[16:9], zll_main_begin14_inR1[8:1], zll_main_begin14_outR1);
  assign {__continue, __padding, __out0, __st0_next, __st1_next} = (zll_main_begin14_inR1[0] == 1'h1) ? zll_main_begin14_outR1 : zll_main_begin14_out;
  initial {__st0, __st1} <= 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 16'h1;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_incr28 (input logic [23:0] arg0,
  output logic [25:0] res);
  logic [23:0] zll_main_incr27_in;
  logic [23:0] zll_main_incr32_in;
  assign zll_main_incr27_in = arg0;
  assign zll_main_incr32_in = {zll_main_incr27_in[15:8], zll_main_incr27_in[23:16], zll_main_incr27_in[7:0]};
  assign res = {2'h0, zll_main_incr32_in[15:8], zll_main_incr32_in[23:16], zll_main_incr32_in[7:0]};
endmodule

module ZLL_Main_begin14 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  logic [15:0] main_sig_in;
  logic [23:0] zll_main_incr28_in;
  logic [25:0] zll_main_incr28_out;
  logic [25:0] zll_main_sig4_in;
  logic [25:0] zll_main_sig8_in;
  logic [23:0] zll_main_sig2_in;
  assign main_sig_in = {arg0, arg1};
  assign zll_main_incr28_in = {main_sig_in[15:8], main_sig_in[15:8], main_sig_in[7:0]};
  ZLL_Main_incr28  inst (zll_main_incr28_in[23:0], zll_main_incr28_out);
  assign zll_main_sig4_in = zll_main_incr28_out;
  assign zll_main_sig8_in = zll_main_sig4_in[25:0];
  assign zll_main_sig2_in = {zll_main_sig8_in[23:16], zll_main_sig8_in[15:8], zll_main_sig8_in[7:0]};
  assign res = {2'h2, zll_main_sig2_in[23:16], zll_main_sig2_in[15:8], zll_main_sig2_in[7:0]};
endmodule

module ZLL_Main_incr14 (input logic [15:0] arg0,
  output logic [25:0] res);
  logic [15:0] zll_main_begin11_in;
  assign zll_main_begin11_in = arg0;
  assign res = {10'h100, zll_main_begin11_in[15:8], zll_main_begin11_in[7:0]};
endmodule