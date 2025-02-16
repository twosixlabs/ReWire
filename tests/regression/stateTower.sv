module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] main_sig_in;
  logic [0:0] rewire_prelude_not1_in;
  logic [0:0] rewire_prelude_not1_out;
  logic [0:0] rewire_prelude_not1_inR1;
  logic [0:0] rewire_prelude_not1_outR1;
  logic [2:0] zll_main_sig70_in;
  logic [1:0] zll_main_sig67_in;
  logic [4:0] zll_main_sig82_in;
  logic [4:0] zll_main_sig63_in;
  logic [1:0] zll_main_sig49_in;
  logic [2:0] zll_main_sig36_in;
  logic [4:0] zll_main_sig36_out;
  logic [4:0] zll_main_sig100_in;
  logic [4:0] zll_main_sig100_out;
  logic [2:0] zll_main_sig28_in;
  logic [1:0] zll_main_sig6_in;
  logic [2:0] zll_main_sig113_in;
  logic [4:0] zll_main_sig113_out;
  logic [4:0] zll_main_sig_in;
  logic [4:0] zll_main_sig99_in;
  logic [2:0] zll_main_sig89_in;
  logic [2:0] zll_main_sig36_inR1;
  logic [4:0] zll_main_sig36_outR1;
  logic [5:0] zll_main_sig75_in;
  logic [5:0] zll_main_sig84_in;
  logic [3:0] zll_main_sig33_in;
  logic [1:0] zll_main_sig65_in;
  logic [4:0] zll_main_sig65_out;
  logic [6:0] zll_main_sig94_in;
  logic [6:0] zll_main_sig112_in;
  logic [3:0] zll_main_sig24_in;
  logic [1:0] binop_in;
  logic [0:0] msbit_in;
  logic [1:0] zll_main_sig65_inR1;
  logic [4:0] zll_main_sig65_outR1;
  logic [4:0] zll_main_sig56_in;
  logic [4:0] zll_main_sig64_in;
  logic [1:0] zll_main_sig102_in;
  logic [2:0] zll_main_sig113_inR1;
  logic [4:0] zll_main_sig113_outR1;
  logic [4:0] zll_main_sig100_inR1;
  logic [4:0] zll_main_sig100_outR1;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [0:0] __st0;
  logic [0:0] __st1;
  logic [0:0] __st0_next;
  logic [0:0] __st1_next;
  assign main_sig_in = {__in0, {__st0, __st1}};
  assign rewire_prelude_not1_in = main_sig_in[2];
  ReWire_Prelude_not1  inst (rewire_prelude_not1_in[0], rewire_prelude_not1_out);
  assign rewire_prelude_not1_inR1 = main_sig_in[2];
  ReWire_Prelude_not1  instR1 (rewire_prelude_not1_inR1[0], rewire_prelude_not1_outR1);
  assign zll_main_sig70_in = {main_sig_in[1], main_sig_in[0], rewire_prelude_not1_outR1};
  assign zll_main_sig67_in = {zll_main_sig70_in[2], zll_main_sig70_in[1]};
  assign zll_main_sig82_in = {3'h0, zll_main_sig67_in[1], zll_main_sig67_in[0]};
  assign zll_main_sig63_in = zll_main_sig82_in[4:0];
  assign zll_main_sig49_in = {zll_main_sig63_in[1], zll_main_sig63_in[0]};
  assign zll_main_sig36_in = {zll_main_sig49_in[1], zll_main_sig49_in[1], zll_main_sig49_in[0]};
  ZLL_Main_sig36  instR2 (zll_main_sig36_in[2:0], zll_main_sig36_out);
  assign zll_main_sig100_in = zll_main_sig36_out;
  ZLL_Main_sig100  instR3 (zll_main_sig100_in[4:0], zll_main_sig100_out);
  assign zll_main_sig28_in = {main_sig_in[1], main_sig_in[0], rewire_prelude_not1_out};
  assign zll_main_sig6_in = {zll_main_sig28_in[2], zll_main_sig28_in[1]};
  assign zll_main_sig113_in = {zll_main_sig6_in[1], zll_main_sig6_in[1], zll_main_sig6_in[0]};
  ZLL_Main_sig113  instR4 (zll_main_sig113_in[2:0], zll_main_sig113_out);
  assign zll_main_sig_in = zll_main_sig113_out;
  assign zll_main_sig99_in = zll_main_sig_in[4:0];
  assign zll_main_sig89_in = {zll_main_sig99_in[2], zll_main_sig99_in[1], zll_main_sig99_in[0]};
  assign zll_main_sig36_inR1 = {zll_main_sig89_in[0], zll_main_sig89_in[1], zll_main_sig89_in[0]};
  ZLL_Main_sig36  instR5 (zll_main_sig36_inR1[2:0], zll_main_sig36_outR1);
  assign zll_main_sig75_in = {zll_main_sig89_in[2], zll_main_sig36_outR1};
  assign zll_main_sig84_in = {zll_main_sig75_in[5], zll_main_sig75_in[4:0]};
  assign zll_main_sig33_in = {zll_main_sig84_in[5], zll_main_sig84_in[2], zll_main_sig84_in[1], zll_main_sig84_in[0]};
  assign zll_main_sig65_in = {zll_main_sig33_in[2], zll_main_sig33_in[0]};
  ZLL_Main_sig65  instR6 (zll_main_sig65_in[1:0], zll_main_sig65_out);
  assign zll_main_sig94_in = {zll_main_sig33_in[3], zll_main_sig33_in[2], zll_main_sig65_out};
  assign zll_main_sig112_in = {zll_main_sig94_in[6], zll_main_sig94_in[5], zll_main_sig94_in[4:0]};
  assign zll_main_sig24_in = {zll_main_sig112_in[6], zll_main_sig112_in[5], zll_main_sig112_in[1], zll_main_sig112_in[0]};
  assign binop_in = {zll_main_sig24_in[3], zll_main_sig24_in[2]};
  assign msbit_in = binop_in[1] ^ binop_in[0];
  assign zll_main_sig65_inR1 = {zll_main_sig24_in[1], msbit_in[0]};
  ZLL_Main_sig65  instR7 (zll_main_sig65_inR1[1:0], zll_main_sig65_outR1);
  assign zll_main_sig56_in = zll_main_sig65_outR1;
  assign zll_main_sig64_in = zll_main_sig56_in[4:0];
  assign zll_main_sig102_in = {zll_main_sig64_in[1], zll_main_sig64_in[0]};
  assign zll_main_sig113_inR1 = {zll_main_sig102_in[1], zll_main_sig102_in[1], zll_main_sig102_in[0]};
  ZLL_Main_sig113  instR8 (zll_main_sig113_inR1[2:0], zll_main_sig113_outR1);
  assign zll_main_sig100_inR1 = zll_main_sig113_outR1;
  ZLL_Main_sig100  instR9 (zll_main_sig100_inR1[4:0], zll_main_sig100_outR1);
  assign {__continue, __padding, __out0, __st0_next, __st1_next} = (zll_main_sig28_in[0] == 1'h1) ? zll_main_sig100_outR1 : zll_main_sig100_out;
  initial {__st0, __st1} <= 2'h3;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 2'h3;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_sig113 (input logic [2:0] arg0,
  output logic [4:0] res);
  logic [2:0] zll_main_sig78_in;
  logic [2:0] zll_main_sig83_in;
  assign zll_main_sig78_in = arg0;
  assign zll_main_sig83_in = {zll_main_sig78_in[1], zll_main_sig78_in[2], zll_main_sig78_in[0]};
  assign res = {2'h1, zll_main_sig83_in[1], zll_main_sig83_in[2], zll_main_sig83_in[0]};
endmodule

module ZLL_Main_sig100 (input logic [4:0] arg0,
  output logic [4:0] res);
  logic [4:0] zll_main_sig88_in;
  logic [2:0] zll_main_sig111_in;
  assign zll_main_sig88_in = arg0;
  assign zll_main_sig111_in = {zll_main_sig88_in[2], zll_main_sig88_in[1], zll_main_sig88_in[0]};
  assign res = {2'h2, zll_main_sig111_in[2], zll_main_sig111_in[1], zll_main_sig111_in[0]};
endmodule

module ReWire_Prelude_not1 (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [0:0] lit_in;
  assign lit_in = arg0;
  assign res = (lit_in[0] == 1'h1) ? 1'h0 : 1'h1;
endmodule

module ZLL_Main_sig65 (input logic [1:0] arg0,
  output logic [4:0] res);
  logic [1:0] zll_main_sig87_in;
  assign zll_main_sig87_in = arg0;
  assign res = {3'h0, zll_main_sig87_in[1], zll_main_sig87_in[0]};
endmodule

module ZLL_Main_sig36 (input logic [2:0] arg0,
  output logic [4:0] res);
  logic [2:0] zll_main_sig95_in;
  assign zll_main_sig95_in = arg0;
  assign res = {2'h1, zll_main_sig95_in[2], zll_main_sig95_in[1], zll_main_sig95_in[0]};
endmodule