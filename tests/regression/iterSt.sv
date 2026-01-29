module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] rewire_monad_iterst1_in;
  logic [2:0] zll_rewire_monad_iterst34_in;
  logic [2:0] zll_rewire_monad_iterst12_in;
  logic [1:0] main_f1_in;
  logic [1:0] zll_main_f4_in;
  logic [1:0] zll_main_f_in;
  logic [1:0] zll_main_f1_in;
  logic [1:0] zll_main_f6_in;
  logic [1:0] zll_main_f7_in;
  logic [1:0] binop_in;
  logic [0:0] msbit_in;
  logic [2:0] zll_rewire_monad_iterst4_in;
  logic [2:0] zll_rewire_monad_iterst26_in;
  logic [4:0] zll_rewire_monad_iterst_in;
  logic [4:0] zll_rewire_monad_iterst8_in;
  logic [2:0] zll_rewire_monad_iterst25_in;
  logic [2:0] zll_rewire_monad_iterst37_in;
  logic [2:0] zll_rewire_monad_iterst22_in;
  logic [0:0] zll_rewire_monad_iterst17_in;
  logic [5:0] zll_rewire_monad_iterst40_in;
  logic [5:0] zll_rewire_monad_iterst38_in;
  logic [1:0] zll_rewire_monad_iterst31_in;
  logic [2:0] __padding;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  assign rewire_monad_iterst1_in = {__in0, __st0};
  assign zll_rewire_monad_iterst34_in = {rewire_monad_iterst1_in[1], rewire_monad_iterst1_in[0], rewire_monad_iterst1_in[0]};
  assign zll_rewire_monad_iterst12_in = {zll_rewire_monad_iterst34_in[2], zll_rewire_monad_iterst34_in[1:0]};
  assign main_f1_in = {zll_rewire_monad_iterst12_in[2], zll_rewire_monad_iterst12_in[1]};
  assign zll_main_f4_in = {main_f1_in[1], main_f1_in[0]};
  assign zll_main_f_in = zll_main_f4_in[1:0];
  assign zll_main_f1_in = {zll_main_f_in[0], zll_main_f_in[1]};
  assign zll_main_f6_in = {zll_main_f1_in[1], zll_main_f1_in[0]};
  assign zll_main_f7_in = zll_main_f6_in[1:0];
  assign binop_in = {zll_main_f7_in[1], zll_main_f7_in[0]};
  assign msbit_in = binop_in[1] ^ binop_in[0];
  assign zll_rewire_monad_iterst4_in = {{msbit_in[0], zll_main_f_in[0]}, zll_rewire_monad_iterst12_in[0]};
  assign zll_rewire_monad_iterst26_in = zll_rewire_monad_iterst4_in[2:0];
  assign zll_rewire_monad_iterst_in = {2'h0, zll_rewire_monad_iterst26_in[2:1], zll_rewire_monad_iterst26_in[0]};
  assign zll_rewire_monad_iterst8_in = zll_rewire_monad_iterst_in[4:0];
  assign zll_rewire_monad_iterst25_in = {zll_rewire_monad_iterst8_in[2:1], zll_rewire_monad_iterst8_in[0]};
  assign zll_rewire_monad_iterst37_in = {zll_rewire_monad_iterst25_in[0], zll_rewire_monad_iterst25_in[2:1]};
  assign zll_rewire_monad_iterst22_in = {zll_rewire_monad_iterst37_in[1], zll_rewire_monad_iterst37_in[0], zll_rewire_monad_iterst37_in[2]};
  assign zll_rewire_monad_iterst17_in = zll_rewire_monad_iterst22_in[1];
  assign zll_rewire_monad_iterst40_in = {zll_rewire_monad_iterst22_in[2], {4'h4, zll_rewire_monad_iterst17_in[0]}};
  assign zll_rewire_monad_iterst38_in = {zll_rewire_monad_iterst40_in[5], zll_rewire_monad_iterst40_in[4:0]};
  assign zll_rewire_monad_iterst31_in = {zll_rewire_monad_iterst38_in[5], zll_rewire_monad_iterst38_in[0]};
  assign {__padding, __out0, __st0_next} = {3'h4, zll_rewire_monad_iterst31_in[1], zll_rewire_monad_iterst31_in[0]};
  initial __st0 = 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule