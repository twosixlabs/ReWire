module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] zll_main_sig34_in;
  logic [0:0] rewire_prelude_not_in;
  logic [0:0] rewire_prelude_not_out;
  logic [0:0] rewire_prelude_not_inR1;
  logic [0:0] rewire_prelude_not_outR1;
  logic [2:0] zll_main_sig29_in;
  logic [1:0] zll_main_sig23_in;
  logic [4:0] zll_main_sig24_in;
  logic [4:0] zll_main_sig24_out;
  logic [2:0] zll_main_sig39_in;
  logic [1:0] zll_main_sig46_in;
  logic [2:0] zll_main_sig8_in;
  logic [2:0] zll_main_sig45_in;
  logic [4:0] zll_main_sig16_in;
  logic [4:0] zll_main_sig36_in;
  logic [2:0] zll_main_sig41_in;
  logic [2:0] zll_main_sig35_in;
  logic [4:0] zll_main_sig35_out;
  logic [5:0] zll_main_sig14_in;
  logic [5:0] zll_main_sig11_in;
  logic [3:0] zll_main_sig38_in;
  logic [3:0] zll_main_sig26_in;
  logic [3:0] zll_main_sig47_in;
  logic [1:0] zll_main_sig15_in;
  logic [4:0] zll_main_sig15_out;
  logic [6:0] zll_main_sig49_in;
  logic [6:0] zll_main_sig32_in;
  logic [3:0] zll_main_sig5_in;
  logic [3:0] zll_main_sig3_in;
  logic [1:0] zll_main_incr3_in;
  logic [1:0] zll_main_incr2_in;
  logic [1:0] zll_main_incr1_in;
  logic [1:0] binop_in;
  logic [0:0] msbit_in;
  logic [1:0] zll_main_sig15_inR1;
  logic [4:0] zll_main_sig15_outR1;
  logic [4:0] zll_main_sig24_inR1;
  logic [4:0] zll_main_sig24_outR1;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [0:0] __st0;
  logic [0:0] __st1;
  logic [0:0] __st0_next;
  logic [0:0] __st1_next;
  assign zll_main_sig34_in = {__in0, {__st0, __st1}};
  assign rewire_prelude_not_in = zll_main_sig34_in[2];
  ReWire_Prelude_not  inst (rewire_prelude_not_in[0], rewire_prelude_not_out);
  assign rewire_prelude_not_inR1 = zll_main_sig34_in[2];
  ReWire_Prelude_not  instR1 (rewire_prelude_not_inR1[0], rewire_prelude_not_outR1);
  assign zll_main_sig29_in = {zll_main_sig34_in[0], zll_main_sig34_in[1], rewire_prelude_not_outR1};
  assign zll_main_sig23_in = {zll_main_sig29_in[1], zll_main_sig29_in[2]};
  assign zll_main_sig24_in = {3'h2, zll_main_sig23_in[1], zll_main_sig23_in[0]};
  ZLL_Main_sig24  instR2 (zll_main_sig24_in[4:0], zll_main_sig24_out);
  assign zll_main_sig39_in = {zll_main_sig34_in[0], zll_main_sig34_in[1], rewire_prelude_not_out};
  assign zll_main_sig46_in = {zll_main_sig39_in[1], zll_main_sig39_in[2]};
  assign zll_main_sig8_in = {zll_main_sig46_in[1], zll_main_sig46_in[1], zll_main_sig46_in[0]};
  assign zll_main_sig45_in = zll_main_sig8_in[2:0];
  assign zll_main_sig16_in = {2'h0, zll_main_sig45_in[2], zll_main_sig45_in[1], zll_main_sig45_in[0]};
  assign zll_main_sig36_in = zll_main_sig16_in[4:0];
  assign zll_main_sig41_in = {zll_main_sig36_in[2], zll_main_sig36_in[1], zll_main_sig36_in[0]};
  assign zll_main_sig35_in = {zll_main_sig41_in[0], zll_main_sig41_in[1], zll_main_sig41_in[0]};
  ZLL_Main_sig35  instR3 (zll_main_sig35_in[2:0], zll_main_sig35_out);
  assign zll_main_sig14_in = {zll_main_sig41_in[2], zll_main_sig35_out};
  assign zll_main_sig11_in = {zll_main_sig14_in[5], zll_main_sig14_in[4:0]};
  assign zll_main_sig38_in = {zll_main_sig11_in[5], zll_main_sig11_in[2], zll_main_sig11_in[1], zll_main_sig11_in[0]};
  assign zll_main_sig26_in = {zll_main_sig38_in[2], zll_main_sig38_in[3], zll_main_sig38_in[1], zll_main_sig38_in[0]};
  assign zll_main_sig47_in = {zll_main_sig26_in[2], zll_main_sig26_in[3], zll_main_sig26_in[1], zll_main_sig26_in[0]};
  assign zll_main_sig15_in = {zll_main_sig47_in[2], zll_main_sig47_in[0]};
  ZLL_Main_sig15  instR4 (zll_main_sig15_in[1:0], zll_main_sig15_out);
  assign zll_main_sig49_in = {zll_main_sig47_in[2], zll_main_sig47_in[3], zll_main_sig15_out};
  assign zll_main_sig32_in = {zll_main_sig49_in[6], zll_main_sig49_in[5], zll_main_sig49_in[4:0]};
  assign zll_main_sig5_in = {zll_main_sig32_in[6], zll_main_sig32_in[5], zll_main_sig32_in[1], zll_main_sig32_in[0]};
  assign zll_main_sig3_in = {zll_main_sig5_in[2], zll_main_sig5_in[3], zll_main_sig5_in[1], zll_main_sig5_in[0]};
  assign zll_main_incr3_in = {zll_main_sig3_in[3], zll_main_sig3_in[2]};
  assign zll_main_incr2_in = {zll_main_incr3_in[1], zll_main_incr3_in[0]};
  assign zll_main_incr1_in = zll_main_incr2_in[1:0];
  assign binop_in = {zll_main_incr1_in[1], zll_main_incr1_in[0]};
  assign msbit_in = binop_in[1] ^ binop_in[0];
  assign zll_main_sig15_inR1 = {zll_main_sig3_in[1], msbit_in[0]};
  ZLL_Main_sig15  instR5 (zll_main_sig15_inR1[1:0], zll_main_sig15_outR1);
  assign zll_main_sig24_inR1 = zll_main_sig15_outR1;
  ZLL_Main_sig24  instR6 (zll_main_sig24_inR1[4:0], zll_main_sig24_outR1);
  assign {__continue, __padding, __out0, __st0_next, __st1_next} = (zll_main_sig39_in[0] == 1'h1) ? zll_main_sig24_outR1 : zll_main_sig24_out;
  initial {__st0, __st1} <= 2'h3;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 2'h3;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module ReWire_Prelude_not (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [1:0] zll_rewire_prelude_not1_in;
  logic [0:0] lit_in;
  assign zll_rewire_prelude_not1_in = {arg0, arg0};
  assign lit_in = zll_rewire_prelude_not1_in[0];
  assign res = (lit_in[0] == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_sig35 (input logic [2:0] arg0,
  output logic [4:0] res);
  logic [2:0] zll_main_sig30_in;
  logic [2:0] zll_main_sig50_in;
  assign zll_main_sig30_in = arg0;
  assign zll_main_sig50_in = {zll_main_sig30_in[1], zll_main_sig30_in[2], zll_main_sig30_in[0]};
  assign res = {2'h0, zll_main_sig50_in[1], zll_main_sig50_in[2], zll_main_sig50_in[0]};
endmodule

module ZLL_Main_sig24 (input logic [4:0] arg0,
  output logic [4:0] res);
  logic [4:0] zll_main_sig43_in;
  logic [1:0] zll_main_sig1_in;
  logic [2:0] zll_main_sig35_in;
  logic [4:0] zll_main_sig35_out;
  logic [4:0] zll_main_sig7_in;
  logic [4:0] zll_main_sig17_in;
  logic [2:0] zll_main_sig40_in;
  assign zll_main_sig43_in = arg0;
  assign zll_main_sig1_in = {zll_main_sig43_in[1], zll_main_sig43_in[0]};
  assign zll_main_sig35_in = {zll_main_sig1_in[1], zll_main_sig1_in[1], zll_main_sig1_in[0]};
  ZLL_Main_sig35  inst (zll_main_sig35_in[2:0], zll_main_sig35_out);
  assign zll_main_sig7_in = zll_main_sig35_out;
  assign zll_main_sig17_in = zll_main_sig7_in[4:0];
  assign zll_main_sig40_in = {zll_main_sig17_in[2], zll_main_sig17_in[1], zll_main_sig17_in[0]};
  assign res = {2'h2, zll_main_sig40_in[2], zll_main_sig40_in[1], zll_main_sig40_in[0]};
endmodule

module ZLL_Main_sig15 (input logic [1:0] arg0,
  output logic [4:0] res);
  logic [1:0] zll_main_sig27_in;
  assign zll_main_sig27_in = arg0;
  assign res = {3'h2, zll_main_sig27_in[1], zll_main_sig27_in[0]};
endmodule