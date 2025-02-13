module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [20:0] zll_pure_dispatch15_in;
  logic [16:0] zll_pure_dispatch14_in;
  logic [16:0] zll_pure_dispatch13_in;
  logic [16:0] zll_main_loop_in;
  logic [16:0] zll_main_loop21_in;
  logic [15:0] zll_main_loop2_in;
  logic [15:0] binop_in;
  logic [15:0] main_loop_in;
  logic [28:0] main_loop_out;
  logic [16:0] zll_main_loop20_in;
  logic [15:0] zll_main_loop1_in;
  logic [15:0] main_loop_inR1;
  logic [28:0] main_loop_outR1;
  logic [20:0] zll_pure_dispatch12_in;
  logic [0:0] zll_main_loop17_in;
  logic [0:0] lit_in;
  logic [20:0] zll_pure_dispatch11_in;
  logic [0:0] zll_main_loop14_in;
  logic [0:0] lit_inR1;
  logic [20:0] zll_pure_dispatch10_in;
  logic [0:0] zll_main_loop13_in;
  logic [0:0] lit_inR2;
  logic [20:0] zll_pure_dispatch9_in;
  logic [0:0] zll_main_loop11_in;
  logic [28:0] zll_main_loop11_out;
  logic [20:0] zll_pure_dispatch8_in;
  logic [0:0] zll_main_loop11_inR1;
  logic [28:0] zll_main_loop11_outR1;
  logic [20:0] zll_pure_dispatch7_in;
  logic [0:0] zll_main_loop8_in;
  logic [28:0] zll_main_loop8_out;
  logic [20:0] zll_pure_dispatch6_in;
  logic [0:0] zll_main_loop7_in;
  logic [28:0] zll_main_loop7_out;
  logic [20:0] zll_pure_dispatch5_in;
  logic [0:0] zll_main_loop8_inR1;
  logic [28:0] zll_main_loop8_outR1;
  logic [20:0] zll_pure_dispatch4_in;
  logic [0:0] zll_main_loop7_inR1;
  logic [28:0] zll_main_loop7_outR1;
  logic [20:0] zll_pure_dispatch3_in;
  logic [0:0] zll_main_loop5_in;
  logic [28:0] zll_main_loop5_out;
  logic [20:0] zll_pure_dispatch2_in;
  logic [0:0] zll_main_loop5_inR1;
  logic [28:0] zll_main_loop5_outR1;
  logic [20:0] zll_pure_dispatch1_in;
  logic [0:0] zll_main_loop3_in;
  logic [28:0] zll_main_loop3_out;
  logic [20:0] zll_pure_dispatch_in;
  logic [0:0] zll_main_loop3_inR1;
  logic [28:0] zll_main_loop3_outR1;
  logic [0:0] __continue;
  logic [19:0] __resumption_tag;
  logic [19:0] __resumption_tag_next;
  assign zll_pure_dispatch15_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch14_in = {zll_pure_dispatch15_in[20], zll_pure_dispatch15_in[15:8], zll_pure_dispatch15_in[7:0]};
  assign zll_pure_dispatch13_in = {zll_pure_dispatch14_in[15:8], zll_pure_dispatch14_in[16], zll_pure_dispatch14_in[7:0]};
  assign zll_main_loop_in = {zll_pure_dispatch13_in[16:9], zll_pure_dispatch13_in[7:0], zll_pure_dispatch13_in[8]};
  assign zll_main_loop21_in = {zll_main_loop_in[16:9], zll_main_loop_in[8:1], zll_main_loop_in[0]};
  assign zll_main_loop2_in = {zll_main_loop21_in[16:9], zll_main_loop21_in[8:1]};
  assign binop_in = {zll_main_loop2_in[7:0], zll_main_loop2_in[15:8]};
  assign main_loop_in = {zll_main_loop2_in[15:8], binop_in[15:8] + binop_in[7:0]};
  Main_loop  inst (main_loop_in[15:8], main_loop_in[7:0], main_loop_out);
  assign zll_main_loop20_in = {zll_main_loop_in[16:9], zll_main_loop_in[8:1], zll_main_loop_in[0]};
  assign zll_main_loop1_in = {zll_main_loop20_in[16:9], zll_main_loop20_in[8:1]};
  assign main_loop_inR1 = {zll_main_loop1_in[7:0], zll_main_loop1_in[15:8]};
  Main_loop  instR1 (main_loop_inR1[15:8], main_loop_inR1[7:0], main_loop_outR1);
  assign zll_pure_dispatch12_in = {__in0, __resumption_tag};
  assign zll_main_loop17_in = zll_pure_dispatch12_in[20];
  assign lit_in = zll_main_loop17_in[0];
  assign zll_pure_dispatch11_in = {__in0, __resumption_tag};
  assign zll_main_loop14_in = zll_pure_dispatch11_in[20];
  assign lit_inR1 = zll_main_loop14_in[0];
  assign zll_pure_dispatch10_in = {__in0, __resumption_tag};
  assign zll_main_loop13_in = zll_pure_dispatch10_in[20];
  assign lit_inR2 = zll_main_loop13_in[0];
  assign zll_pure_dispatch9_in = {__in0, __resumption_tag};
  assign zll_main_loop11_in = zll_pure_dispatch9_in[20];
  ZLL_Main_loop11  instR2 (zll_main_loop11_in[0], zll_main_loop11_out);
  assign zll_pure_dispatch8_in = {__in0, __resumption_tag};
  assign zll_main_loop11_inR1 = zll_pure_dispatch8_in[20];
  ZLL_Main_loop11  instR3 (zll_main_loop11_inR1[0], zll_main_loop11_outR1);
  assign zll_pure_dispatch7_in = {__in0, __resumption_tag};
  assign zll_main_loop8_in = zll_pure_dispatch7_in[20];
  ZLL_Main_loop8  instR4 (zll_main_loop8_in[0], zll_main_loop8_out);
  assign zll_pure_dispatch6_in = {__in0, __resumption_tag};
  assign zll_main_loop7_in = zll_pure_dispatch6_in[20];
  ZLL_Main_loop7  instR5 (zll_main_loop7_in[0], zll_main_loop7_out);
  assign zll_pure_dispatch5_in = {__in0, __resumption_tag};
  assign zll_main_loop8_inR1 = zll_pure_dispatch5_in[20];
  ZLL_Main_loop8  instR6 (zll_main_loop8_inR1[0], zll_main_loop8_outR1);
  assign zll_pure_dispatch4_in = {__in0, __resumption_tag};
  assign zll_main_loop7_inR1 = zll_pure_dispatch4_in[20];
  ZLL_Main_loop7  instR7 (zll_main_loop7_inR1[0], zll_main_loop7_outR1);
  assign zll_pure_dispatch3_in = {__in0, __resumption_tag};
  assign zll_main_loop5_in = zll_pure_dispatch3_in[20];
  ZLL_Main_loop5  instR8 (zll_main_loop5_in[0], zll_main_loop5_out);
  assign zll_pure_dispatch2_in = {__in0, __resumption_tag};
  assign zll_main_loop5_inR1 = zll_pure_dispatch2_in[20];
  ZLL_Main_loop5  instR9 (zll_main_loop5_inR1[0], zll_main_loop5_outR1);
  assign zll_pure_dispatch1_in = {__in0, __resumption_tag};
  assign zll_main_loop3_in = zll_pure_dispatch1_in[20];
  ZLL_Main_loop3  instR10 (zll_main_loop3_in[0], zll_main_loop3_out);
  assign zll_pure_dispatch_in = {__in0, __resumption_tag};
  assign zll_main_loop3_inR1 = zll_pure_dispatch_in[20];
  ZLL_Main_loop3  instR11 (zll_main_loop3_inR1[0], zll_main_loop3_outR1);
  assign {__continue, __out0, __resumption_tag_next} = (zll_pure_dispatch_in[19:16] == 4'h1) ? zll_main_loop3_outR1 : ((zll_pure_dispatch1_in[19:16] == 4'h2) ? zll_main_loop3_out : ((zll_pure_dispatch2_in[19:16] == 4'h3) ? zll_main_loop5_outR1 : ((zll_pure_dispatch3_in[19:16] == 4'h4) ? zll_main_loop5_out : ((zll_pure_dispatch4_in[19:16] == 4'h5) ? zll_main_loop7_outR1 : ((zll_pure_dispatch5_in[19:16] == 4'h6) ? zll_main_loop8_outR1 : ((zll_pure_dispatch6_in[19:16] == 4'h7) ? zll_main_loop7_out : ((zll_pure_dispatch7_in[19:16] == 4'h8) ? zll_main_loop8_out : ((zll_pure_dispatch8_in[19:16] == 4'h9) ? zll_main_loop11_outR1 : ((zll_pure_dispatch9_in[19:16] == 4'ha) ? zll_main_loop11_out : ((zll_pure_dispatch10_in[19:16] == 4'hb) ? ((lit_inR2[0] == 1'h1) ? 29'h105d0000 : 29'h108c0000) : ((zll_pure_dispatch11_in[19:16] == 4'hc) ? ((lit_inR1[0] == 1'h1) ? 29'h10800d08 : 29'h10d0150d) : ((zll_pure_dispatch12_in[19:16] == 4'hd) ? ((lit_in[0] == 1'h1) ? 29'h10500805 : 29'h10800d08) : ((zll_main_loop20_in[0] == 1'h1) ? main_loop_outR1 : main_loop_out)))))))))))));
  initial __resumption_tag <= 20'h10000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 20'h10000;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Main_loop3 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit_in;
  assign lit_in = arg0;
  assign res = (lit_in[0] == 1'h1) ? 29'h10020000 : 29'h10130000;
endmodule

module ZLL_Main_loop5 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit_in;
  assign lit_in = arg0;
  assign res = (lit_in[0] == 1'h1) ? 29'h10140000 : 29'h10150000;
endmodule

module ZLL_Main_loop7 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit_in;
  assign lit_in = arg0;
  assign res = (lit_in[0] == 1'h1) ? 29'h10170000 : 29'h10260000;
endmodule

module ZLL_Main_loop8 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit_in;
  assign lit_in = arg0;
  assign res = (lit_in[0] == 1'h1) ? 29'h10280000 : 29'h10390000;
endmodule

module ZLL_Main_loop11 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit_in;
  assign lit_in = arg0;
  assign res = (lit_in[0] == 1'h1) ? 29'h103a0000 : 29'h105b0000;
endmodule

module Main_loop (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [28:0] res);
  assign res = {1'h1, arg0, 4'h0, arg1, arg0};
endmodule