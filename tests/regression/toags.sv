module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [5:0] zll_pure_dispatch6_in;
  logic [3:0] zll_pure_dispatch7_in;
  logic [3:0] zll_main_go27_in;
  logic [6:0] zll_main_go27_out;
  logic [5:0] zll_pure_dispatch3_in;
  logic [2:0] zll_main_go4_in;
  logic [3:0] zll_main_go9_in;
  logic [2:0] zll_main_go1_in;
  logic [2:0] zll_main_go10_in;
  logic [2:0] zll_main_go11_in;
  logic [2:0] zll_main_go22_in;
  logic [7:0] zll_main_go13_in;
  logic [7:0] zll_main_go32_in;
  logic [3:0] zll_main_go27_inR1;
  logic [6:0] zll_main_go27_outR1;
  logic [3:0] zll_main_go5_in;
  logic [2:0] zll_main_go2_in;
  logic [2:0] zll_main_go20_in;
  logic [5:0] zll_pure_dispatch2_in;
  logic [3:0] zll_pure_dispatch5_in;
  logic [3:0] zll_main_go_in;
  logic [1:0] zll_main_go18_in;
  logic [1:0] zll_main_go29_in;
  logic [6:0] zll_main_go8_in;
  logic [6:0] zll_main_go17_in;
  logic [1:0] main_go_in;
  logic [0:0] __continue;
  logic [2:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __st1;
  logic [2:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  logic [0:0] __st1_next;
  assign zll_pure_dispatch6_in = {__in0, {__resumption_tag, __st0, __st1}};
  assign zll_pure_dispatch7_in = {zll_pure_dispatch6_in[5], zll_pure_dispatch6_in[2], zll_pure_dispatch6_in[1], zll_pure_dispatch6_in[0]};
  assign zll_main_go27_in = {zll_pure_dispatch7_in[2], zll_pure_dispatch7_in[3], zll_pure_dispatch7_in[1], zll_pure_dispatch7_in[0]};
  ZLL_Main_go27  inst (zll_main_go27_in[3], zll_main_go27_in[2], zll_main_go27_in[1], zll_main_go27_in[0], zll_main_go27_out);
  assign zll_pure_dispatch3_in = {__in0, {__resumption_tag, __st0, __st1}};
  assign zll_main_go4_in = {zll_pure_dispatch3_in[5], zll_pure_dispatch3_in[1], zll_pure_dispatch3_in[0]};
  assign zll_main_go9_in = {zll_main_go4_in[0], zll_main_go4_in[1], zll_main_go4_in[2], zll_main_go4_in[2]};
  assign zll_main_go1_in = {zll_main_go9_in[3], zll_main_go9_in[2], zll_main_go9_in[1]};
  assign zll_main_go10_in = {zll_main_go1_in[0], zll_main_go1_in[1], zll_main_go1_in[2]};
  assign zll_main_go11_in = {zll_main_go10_in[1], zll_main_go10_in[1], zll_main_go10_in[0]};
  assign zll_main_go22_in = zll_main_go11_in[2:0];
  assign zll_main_go13_in = {zll_main_go10_in[2], {4'h0, zll_main_go22_in[2], zll_main_go22_in[1], zll_main_go22_in[0]}};
  assign zll_main_go32_in = {zll_main_go13_in[7], zll_main_go13_in[6:0]};
  assign zll_main_go27_inR1 = {zll_main_go32_in[7], zll_main_go32_in[2], zll_main_go32_in[1], zll_main_go32_in[0]};
  ZLL_Main_go27  instR1 (zll_main_go27_inR1[3], zll_main_go27_inR1[2], zll_main_go27_inR1[1], zll_main_go27_inR1[0], zll_main_go27_outR1);
  assign zll_main_go5_in = {zll_main_go4_in[0], zll_main_go4_in[1], zll_main_go4_in[2], zll_main_go4_in[2]};
  assign zll_main_go2_in = {zll_main_go5_in[3], zll_main_go5_in[2], zll_main_go5_in[1]};
  assign zll_main_go20_in = {zll_main_go2_in[0], zll_main_go2_in[1], zll_main_go2_in[2]};
  assign zll_pure_dispatch2_in = {__in0, {__resumption_tag, __st0, __st1}};
  assign zll_pure_dispatch5_in = {zll_pure_dispatch2_in[5], zll_pure_dispatch2_in[2], zll_pure_dispatch2_in[1], zll_pure_dispatch2_in[0]};
  assign zll_main_go_in = {zll_pure_dispatch5_in[2], zll_pure_dispatch5_in[3], zll_pure_dispatch5_in[1], zll_pure_dispatch5_in[0]};
  assign zll_main_go18_in = {zll_main_go_in[1], zll_main_go_in[3]};
  assign zll_main_go29_in = zll_main_go18_in[1:0];
  assign zll_main_go8_in = {5'h2, zll_main_go29_in[1], zll_main_go29_in[0]};
  assign zll_main_go17_in = zll_main_go8_in[6:0];
  assign main_go_in = {zll_main_go17_in[1], zll_main_go17_in[0]};
  assign {__continue, __out0, __resumption_tag_next, __st0_next, __st1_next} = (zll_pure_dispatch2_in[4:3] == 2'h1) ? {5'h14, main_go_in[1], main_go_in[0]} : ((zll_pure_dispatch3_in[4:3] == 2'h2) ? ((zll_main_go5_in[0] == 1'h1) ? {4'h8, zll_main_go20_in[2], zll_main_go20_in[1], zll_main_go20_in[0]} : zll_main_go27_outR1) : zll_main_go27_out);
  initial {__resumption_tag, __st0, __st1} <= 5'h11;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 5'h11;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module ZLL_Main_go27 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  input logic [0:0] arg3,
  output logic [6:0] res);
  logic [2:0] zll_main_go30_in;
  logic [1:0] zll_main_go33_in;
  logic [1:0] zll_main_go14_in;
  logic [1:0] rewirezupreludezuzazazuin;
  logic [0:0] rewirezupreludezuzaza_out;
  logic [1:0] id_in;
  logic [1:0] rewirezupreludezuzazazuinR1;
  logic [0:0] rewirezupreludezuzaza_outR1;
  assign zll_main_go30_in = {arg0, arg1, arg1};
  assign zll_main_go33_in = {zll_main_go30_in[2], zll_main_go30_in[1]};
  assign zll_main_go14_in = {zll_main_go33_in[1], zll_main_go33_in[0]};
  assign rewirezupreludezuzazazuin = {zll_main_go14_in[1], zll_main_go14_in[1]};
  ReWirezuPreludezuzaza  inst (rewirezupreludezuzazazuin[1], rewirezupreludezuzazazuin[0], rewirezupreludezuzaza_out);
  assign id_in = {zll_main_go30_in[2], zll_main_go30_in[0]};
  assign rewirezupreludezuzazazuinR1 = {(id_in[0] == 1'h1) ? id_in[1] : rewirezupreludezuzaza_out, arg1};
  ReWirezuPreludezuzaza  instR1 (rewirezupreludezuzazazuinR1[1], rewirezupreludezuzazazuinR1[0], rewirezupreludezuzaza_outR1);
  assign res = {1'h1, rewirezupreludezuzaza_outR1, 2'h1, arg1, arg2, arg3};
endmodule

module ReWirezuPreludezuzaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [3:0] zzllzurewirezupreludezuzaza1zuin;
  logic [1:0] zzllzurewirezupreludezuzazazuin;
  logic [1:0] lit_in;
  logic [1:0] id_in;
  assign zzllzurewirezupreludezuzaza1zuin = {arg0, arg1, arg0, arg1};
  assign zzllzurewirezupreludezuzazazuin = {zzllzurewirezupreludezuzaza1zuin[3], zzllzurewirezupreludezuzaza1zuin[2]};
  assign lit_in = zzllzurewirezupreludezuzazazuin[1:0];
  assign id_in = zzllzurewirezupreludezuzaza1zuin[1:0];
  assign res = (id_in[1] == 1'h1) ? id_in[0] : 1'h0;
endmodule