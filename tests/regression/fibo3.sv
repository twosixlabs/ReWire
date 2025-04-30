module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] zll_main_sig1_in;
  logic [17:0] zll_main_sig8_in;
  logic [16:0] zll_main_sig2_in;
  logic [16:0] zll_main_sig6_in;
  logic [15:0] main_incr_in;
  logic [15:0] main_first_in;
  logic [23:0] main_first_out;
  logic [23:0] zll_main_incr12_in;
  logic [25:0] zll_main_incr12_out;
  logic [25:0] zll_main_incr20_in;
  logic [25:0] zll_main_incr_in;
  logic [23:0] zll_main_incr4_in;
  logic [15:0] main_second_in;
  logic [31:0] zll_main_second_in;
  logic [31:0] zll_main_second4_in;
  logic [15:0] zll_main_second3_in;
  logic [15:0] resize_in;
  logic [255:0] binop_in;
  logic [127:0] resize_inR1;
  logic [23:0] zll_main_incr12_inR1;
  logic [25:0] zll_main_incr12_outR1;
  logic [33:0] zll_main_incr15_in;
  logic [33:0] zll_main_incr19_in;
  logic [31:0] zll_main_incr9_in;
  logic [15:0] binop_inR1;
  logic [15:0] zll_main_incr13_in;
  logic [25:0] zll_main_incr10_in;
  logic [25:0] zll_main_sig10_in;
  logic [25:0] zll_main_sig10_out;
  logic [16:0] zll_main_sig10_inR1;
  logic [25:0] zll_main_sig10_outR1;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [15:0] __st0;
  logic [15:0] __st0_next;
  assign zll_main_sig1_in = {__in0, __st0};
  assign zll_main_sig8_in = {zll_main_sig1_in[16], zll_main_sig1_in[16], zll_main_sig1_in[15:0]};
  assign zll_main_sig2_in = {zll_main_sig8_in[17], zll_main_sig8_in[15:0]};
  assign zll_main_sig6_in = {zll_main_sig2_in[15:0], zll_main_sig2_in[16]};
  assign main_incr_in = zll_main_sig6_in[16:1];
  assign main_first_in = main_incr_in[15:0];
  Main_first  inst (main_first_in[15:0], main_first_out);
  assign zll_main_incr12_in = main_first_out;
  ZLL_Main_incr12  instR1 (zll_main_incr12_in[23:0], zll_main_incr12_out);
  assign zll_main_incr20_in = zll_main_incr12_out;
  assign zll_main_incr_in = zll_main_incr20_in[25:0];
  assign zll_main_incr4_in = {zll_main_incr_in[23:16], zll_main_incr_in[15:0]};
  assign main_second_in = zll_main_incr4_in[15:0];
  assign zll_main_second_in = {main_second_in[15:0], main_second_in[15:0]};
  assign zll_main_second4_in = zll_main_second_in[31:0];
  assign zll_main_second3_in = zll_main_second4_in[31:16];
  assign resize_in = zll_main_second3_in[15:0];
  assign binop_in = {128'(resize_in[15:0]), {8'h80{1'h0}}};
  assign resize_inR1 = binop_in[255:128] >> binop_in[127:0];
  assign zll_main_incr12_inR1 = {resize_inR1[7:0], zll_main_second4_in[15:0]};
  ZLL_Main_incr12  instR2 (zll_main_incr12_inR1[23:0], zll_main_incr12_outR1);
  assign zll_main_incr15_in = {zll_main_incr4_in[23:16], zll_main_incr12_outR1};
  assign zll_main_incr19_in = {zll_main_incr15_in[33:26], zll_main_incr15_in[25:0]};
  assign zll_main_incr9_in = {zll_main_incr19_in[33:26], zll_main_incr19_in[23:16], zll_main_incr19_in[15:0]};
  assign binop_inR1 = {zll_main_incr9_in[31:24], zll_main_incr9_in[23:16]};
  assign zll_main_incr13_in = {zll_main_incr9_in[23:16], binop_inR1[15:8] + binop_inR1[7:0]};
  assign zll_main_incr10_in = {10'h100, zll_main_incr13_in[15:0]};
  assign zll_main_sig10_in = zll_main_incr10_in[25:0];
  ZLL_Main_sig10  instR3 (zll_main_sig10_in[15:0], zll_main_sig10_out);
  assign zll_main_sig10_inR1 = {zll_main_sig8_in[15:0], zll_main_sig8_in[16]};
  ZLL_Main_sig10  instR4 (zll_main_sig10_inR1[16:1], zll_main_sig10_outR1);
  assign {__continue, __padding, __out0, __st0_next} = (zll_main_sig10_inR1[0] == 1'h1) ? zll_main_sig10_outR1 : zll_main_sig10_out;
  initial __st0 <= 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 16'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module ZLL_Main_sig10 (input logic [15:0] arg0,
  output logic [25:0] res);
  logic [15:0] main_sig_in;
  logic [15:0] main_first_in;
  logic [23:0] main_first_out;
  logic [23:0] zll_main_incr12_in;
  logic [25:0] zll_main_incr12_out;
  logic [25:0] zll_main_sig9_in;
  logic [25:0] zll_main_sig_in;
  logic [23:0] zll_main_sig7_in;
  assign main_sig_in = arg0;
  assign main_first_in = main_sig_in[15:0];
  Main_first  inst (main_first_in[15:0], main_first_out);
  assign zll_main_incr12_in = main_first_out;
  ZLL_Main_incr12  instR1 (zll_main_incr12_in[23:0], zll_main_incr12_out);
  assign zll_main_sig9_in = zll_main_incr12_out;
  assign zll_main_sig_in = zll_main_sig9_in[25:0];
  assign zll_main_sig7_in = {zll_main_sig_in[23:16], zll_main_sig_in[15:0]};
  assign res = {2'h2, zll_main_sig7_in[23:16], zll_main_sig7_in[15:0]};
endmodule

module ZLL_Main_incr12 (input logic [23:0] arg0,
  output logic [25:0] res);
  logic [23:0] zll_main_incr14_in;
  assign zll_main_incr14_in = arg0;
  assign res = {2'h0, zll_main_incr14_in[23:16], zll_main_incr14_in[15:0]};
endmodule

module Main_first (input logic [15:0] arg0,
  output logic [23:0] res);
  logic [31:0] zll_main_first2_in;
  logic [31:0] zll_main_first3_in;
  logic [15:0] zll_main_first_in;
  logic [15:0] id_in;
  assign zll_main_first2_in = {arg0, arg0};
  assign zll_main_first3_in = zll_main_first2_in[31:0];
  assign zll_main_first_in = zll_main_first3_in[31:16];
  assign id_in = zll_main_first_in[15:0];
  assign res = {id_in[15:8], zll_main_first3_in[15:0]};
endmodule