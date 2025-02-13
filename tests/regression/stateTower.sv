module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] main_sig_in;
  logic [0:0] rewire_prelude_not_in;
  logic [0:0] rewire_prelude_not_out;
  logic [0:0] rewire_prelude_not_inR1;
  logic [0:0] rewire_prelude_not_outR1;
  logic [2:0] zll_main_sig113_in;
  logic [1:0] zll_main_sig15_in;
  logic [4:0] zll_main_sig101_in;
  logic [4:0] zll_main_sig99_in;
  logic [1:0] zll_main_sig6_in;
  logic [2:0] zll_main_sig54_in;
  logic [2:0] zll_main_sig52_in;
  logic [2:0] zll_main_sig51_in;
  logic [4:0] zll_main_sig50_in;
  logic [4:0] zll_main_sig48_in;
  logic [2:0] zll_main_sig_in;
  logic [4:0] zll_main_sig_out;
  logic [2:0] zll_main_sig112_in;
  logic [1:0] zll_main_sig14_in;
  logic [2:0] zll_main_sig98_in;
  logic [2:0] zll_main_sig21_in;
  logic [4:0] zll_main_sig21_out;
  logic [4:0] zll_main_sig94_in;
  logic [4:0] zll_main_sig92_in;
  logic [2:0] zll_main_sig4_in;
  logic [2:0] zll_main_sig47_in;
  logic [2:0] zll_main_sig21_inR1;
  logic [4:0] zll_main_sig21_outR1;
  logic [5:0] zll_main_sig43_in;
  logic [5:0] zll_main_sig42_in;
  logic [3:0] zll_main_sig41_in;
  logic [3:0] zll_main_sig40_in;
  logic [3:0] zll_main_sig3_in;
  logic [1:0] zll_main_sig39_in;
  logic [1:0] zll_main_sig28_in;
  logic [4:0] zll_main_sig28_out;
  logic [6:0] zll_main_sig35_in;
  logic [6:0] zll_main_sig33_in;
  logic [3:0] zll_main_sig32_in;
  logic [3:0] zll_main_sig2_in;
  logic [1:0] binop_in;
  logic [0:0] msbit_in;
  logic [1:0] zll_main_sig31_in;
  logic [1:0] zll_main_sig28_inR1;
  logic [4:0] zll_main_sig28_outR1;
  logic [4:0] zll_main_sig27_in;
  logic [4:0] zll_main_sig25_in;
  logic [1:0] zll_main_sig1_in;
  logic [2:0] zll_main_sig24_in;
  logic [2:0] zll_main_sig21_inR2;
  logic [4:0] zll_main_sig21_outR2;
  logic [4:0] zll_main_sig20_in;
  logic [4:0] zll_main_sig18_in;
  logic [2:0] zll_main_sig_inR1;
  logic [4:0] zll_main_sig_outR1;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [0:0] __st0;
  logic [0:0] __st1;
  logic [0:0] __st0_next;
  logic [0:0] __st1_next;
  assign main_sig_in = {__in0, {__st0, __st1}};
  assign rewire_prelude_not_in = main_sig_in[2];
  ReWire_Prelude_not  inst (rewire_prelude_not_in[0], rewire_prelude_not_out);
  assign rewire_prelude_not_inR1 = main_sig_in[2];
  ReWire_Prelude_not  instR1 (rewire_prelude_not_inR1[0], rewire_prelude_not_outR1);
  assign zll_main_sig113_in = {main_sig_in[1], main_sig_in[0], rewire_prelude_not_outR1};
  assign zll_main_sig15_in = {zll_main_sig113_in[2], zll_main_sig113_in[1]};
  assign zll_main_sig101_in = {3'h0, zll_main_sig15_in[1], zll_main_sig15_in[0]};
  assign zll_main_sig99_in = zll_main_sig101_in[4:0];
  assign zll_main_sig6_in = {zll_main_sig99_in[1], zll_main_sig99_in[0]};
  assign zll_main_sig54_in = {zll_main_sig6_in[1], zll_main_sig6_in[1], zll_main_sig6_in[0]};
  assign zll_main_sig52_in = zll_main_sig54_in[2:0];
  assign zll_main_sig51_in = {zll_main_sig52_in[1], zll_main_sig52_in[2], zll_main_sig52_in[0]};
  assign zll_main_sig50_in = {2'h1, zll_main_sig51_in[1], zll_main_sig51_in[2], zll_main_sig51_in[0]};
  assign zll_main_sig48_in = zll_main_sig50_in[4:0];
  assign zll_main_sig_in = {zll_main_sig48_in[2], zll_main_sig48_in[1], zll_main_sig48_in[0]};
  ZLL_Main_sig  instR2 (zll_main_sig_in[2], zll_main_sig_in[1], zll_main_sig_in[0], zll_main_sig_out);
  assign zll_main_sig112_in = {main_sig_in[1], main_sig_in[0], rewire_prelude_not_out};
  assign zll_main_sig14_in = {zll_main_sig112_in[2], zll_main_sig112_in[1]};
  assign zll_main_sig98_in = {zll_main_sig14_in[1], zll_main_sig14_in[1], zll_main_sig14_in[0]};
  assign zll_main_sig21_in = zll_main_sig98_in[2:0];
  ZLL_Main_sig21  instR3 (zll_main_sig21_in[2], zll_main_sig21_in[1], zll_main_sig21_in[0], zll_main_sig21_out);
  assign zll_main_sig94_in = zll_main_sig21_out;
  assign zll_main_sig92_in = zll_main_sig94_in[4:0];
  assign zll_main_sig4_in = {zll_main_sig92_in[2], zll_main_sig92_in[1], zll_main_sig92_in[0]};
  assign zll_main_sig47_in = {zll_main_sig4_in[0], zll_main_sig4_in[1], zll_main_sig4_in[0]};
  assign zll_main_sig21_inR1 = zll_main_sig47_in[2:0];
  ZLL_Main_sig21  instR4 (zll_main_sig21_inR1[2], zll_main_sig21_inR1[1], zll_main_sig21_inR1[0], zll_main_sig21_outR1);
  assign zll_main_sig43_in = {zll_main_sig4_in[2], zll_main_sig21_outR1};
  assign zll_main_sig42_in = {zll_main_sig43_in[5], zll_main_sig43_in[4:0]};
  assign zll_main_sig41_in = {zll_main_sig42_in[5], zll_main_sig42_in[2], zll_main_sig42_in[1], zll_main_sig42_in[0]};
  assign zll_main_sig40_in = {zll_main_sig41_in[2], zll_main_sig41_in[3], zll_main_sig41_in[1], zll_main_sig41_in[0]};
  assign zll_main_sig3_in = {zll_main_sig40_in[2], zll_main_sig40_in[3], zll_main_sig40_in[1], zll_main_sig40_in[0]};
  assign zll_main_sig39_in = {zll_main_sig3_in[2], zll_main_sig3_in[0]};
  assign zll_main_sig28_in = zll_main_sig39_in[1:0];
  ZLL_Main_sig28  instR5 (zll_main_sig28_in[1], zll_main_sig28_in[0], zll_main_sig28_out);
  assign zll_main_sig35_in = {zll_main_sig3_in[2], zll_main_sig3_in[3], zll_main_sig28_out};
  assign zll_main_sig33_in = {zll_main_sig35_in[6], zll_main_sig35_in[5], zll_main_sig35_in[4:0]};
  assign zll_main_sig32_in = {zll_main_sig33_in[6], zll_main_sig33_in[5], zll_main_sig33_in[1], zll_main_sig33_in[0]};
  assign zll_main_sig2_in = {zll_main_sig32_in[2], zll_main_sig32_in[3], zll_main_sig32_in[1], zll_main_sig32_in[0]};
  assign binop_in = {zll_main_sig2_in[3], zll_main_sig2_in[2]};
  assign msbit_in = binop_in[1] ^ binop_in[0];
  assign zll_main_sig31_in = {zll_main_sig2_in[1], msbit_in[0]};
  assign zll_main_sig28_inR1 = zll_main_sig31_in[1:0];
  ZLL_Main_sig28  instR6 (zll_main_sig28_inR1[1], zll_main_sig28_inR1[0], zll_main_sig28_outR1);
  assign zll_main_sig27_in = zll_main_sig28_outR1;
  assign zll_main_sig25_in = zll_main_sig27_in[4:0];
  assign zll_main_sig1_in = {zll_main_sig25_in[1], zll_main_sig25_in[0]};
  assign zll_main_sig24_in = {zll_main_sig1_in[1], zll_main_sig1_in[1], zll_main_sig1_in[0]};
  assign zll_main_sig21_inR2 = zll_main_sig24_in[2:0];
  ZLL_Main_sig21  instR7 (zll_main_sig21_inR2[2], zll_main_sig21_inR2[1], zll_main_sig21_inR2[0], zll_main_sig21_outR2);
  assign zll_main_sig20_in = zll_main_sig21_outR2;
  assign zll_main_sig18_in = zll_main_sig20_in[4:0];
  assign zll_main_sig_inR1 = {zll_main_sig18_in[2], zll_main_sig18_in[1], zll_main_sig18_in[0]};
  ZLL_Main_sig  instR8 (zll_main_sig_inR1[2], zll_main_sig_inR1[1], zll_main_sig_inR1[0], zll_main_sig_outR1);
  assign {__continue, __padding, __out0, __st0_next, __st1_next} = (zll_main_sig112_in[0] == 1'h1) ? zll_main_sig_outR1 : zll_main_sig_out;
  initial {__st0, __st1} <= 2'h3;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 2'h3;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_sig (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  output logic [4:0] res);
  assign res = {2'h2, arg0, arg1, arg2};
endmodule

module ZLL_Main_sig21 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  output logic [4:0] res);
  assign res = {2'h1, arg0, arg1, arg2};
endmodule

module ZLL_Main_sig28 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [4:0] res);
  assign res = {3'h0, arg0, arg1};
endmodule

module ReWire_Prelude_not (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [0:0] lit_in;
  assign lit_in = arg0;
  assign res = (lit_in[0] == 1'h1) ? 1'h0 : 1'h1;
endmodule