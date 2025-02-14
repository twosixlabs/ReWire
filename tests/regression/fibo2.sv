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
  logic [27:0] main_loop_out;
  logic [16:0] zll_main_loop20_in;
  logic [15:0] zll_main_loop1_in;
  logic [15:0] main_loop_inR1;
  logic [27:0] main_loop_outR1;
  logic [20:0] zll_pure_dispatch12_in;
  logic [0:0] zll_main_loop17_in;
  logic [0:0] lit_in;
  logic [20:0] zll_pure_dispatch11_in;
  logic [0:0] zll_main_loop14_in;
  logic [0:0] lit_inR1;
  logic [20:0] zll_pure_dispatch10_in;
  logic [0:0] zll_main_loop13_in;
  logic [0:0] lit_inR2;
  logic [20:0] zll_pure_dispatch8_in;
  logic [27:0] zll_pure_dispatch8_out;
  logic [20:0] zll_pure_dispatch8_inR1;
  logic [27:0] zll_pure_dispatch8_outR1;
  logic [20:0] zll_pure_dispatch5_in;
  logic [27:0] zll_pure_dispatch5_out;
  logic [20:0] zll_pure_dispatch4_in;
  logic [27:0] zll_pure_dispatch4_out;
  logic [20:0] zll_pure_dispatch5_inR1;
  logic [27:0] zll_pure_dispatch5_outR1;
  logic [20:0] zll_pure_dispatch4_inR1;
  logic [27:0] zll_pure_dispatch4_outR1;
  logic [20:0] zll_pure_dispatch2_in;
  logic [27:0] zll_pure_dispatch2_out;
  logic [20:0] zll_pure_dispatch2_inR1;
  logic [27:0] zll_pure_dispatch2_outR1;
  logic [20:0] zll_pure_dispatch_in;
  logic [27:0] zll_pure_dispatch_out;
  logic [20:0] zll_pure_dispatch_inR1;
  logic [27:0] zll_pure_dispatch_outR1;
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
  assign zll_pure_dispatch8_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch8  instR2 (zll_pure_dispatch8_in[20], zll_pure_dispatch8_out);
  assign zll_pure_dispatch8_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch8  instR3 (zll_pure_dispatch8_inR1[20], zll_pure_dispatch8_outR1);
  assign zll_pure_dispatch5_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch5  instR4 (zll_pure_dispatch5_in[20], zll_pure_dispatch5_out);
  assign zll_pure_dispatch4_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch4  instR5 (zll_pure_dispatch4_in[20], zll_pure_dispatch4_out);
  assign zll_pure_dispatch5_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch5  instR6 (zll_pure_dispatch5_inR1[20], zll_pure_dispatch5_outR1);
  assign zll_pure_dispatch4_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch4  instR7 (zll_pure_dispatch4_inR1[20], zll_pure_dispatch4_outR1);
  assign zll_pure_dispatch2_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch2  instR8 (zll_pure_dispatch2_in[20], zll_pure_dispatch2_out);
  assign zll_pure_dispatch2_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch2  instR9 (zll_pure_dispatch2_inR1[20], zll_pure_dispatch2_outR1);
  assign zll_pure_dispatch_in = {__in0, __resumption_tag};
  ZLL_Pure_dispatch  instR10 (zll_pure_dispatch_in[20], zll_pure_dispatch_out);
  assign zll_pure_dispatch_inR1 = {__in0, __resumption_tag};
  ZLL_Pure_dispatch  instR11 (zll_pure_dispatch_inR1[20], zll_pure_dispatch_outR1);
  assign {__continue, __out0, __resumption_tag_next} = (zll_pure_dispatch_inR1[19:16] == 4'h1) ? zll_pure_dispatch_outR1 : ((zll_pure_dispatch_in[19:16] == 4'h2) ? zll_pure_dispatch_out : ((zll_pure_dispatch2_inR1[19:16] == 4'h3) ? zll_pure_dispatch2_outR1 : ((zll_pure_dispatch2_in[19:16] == 4'h4) ? zll_pure_dispatch2_out : ((zll_pure_dispatch4_inR1[19:16] == 4'h5) ? zll_pure_dispatch4_outR1 : ((zll_pure_dispatch5_inR1[19:16] == 4'h6) ? zll_pure_dispatch5_outR1 : ((zll_pure_dispatch4_in[19:16] == 4'h7) ? zll_pure_dispatch4_out : ((zll_pure_dispatch5_in[19:16] == 4'h8) ? zll_pure_dispatch5_out : ((zll_pure_dispatch8_inR1[19:16] == 4'h9) ? zll_pure_dispatch8_outR1 : ((zll_pure_dispatch8_in[19:16] == 4'ha) ? zll_pure_dispatch8_out : ((zll_pure_dispatch10_in[19:16] == 4'hb) ? ((lit_inR2[0] == 1'h1) ? 28'h05d0000 : 28'h08c0000) : ((zll_pure_dispatch11_in[19:16] == 4'hc) ? ((lit_inR1[0] == 1'h1) ? 28'h0800d08 : 28'h0d0150d) : ((zll_pure_dispatch12_in[19:16] == 4'hd) ? ((lit_in[0] == 1'h1) ? 28'h0500805 : 28'h0800d08) : ((zll_main_loop20_in[0] == 1'h1) ? main_loop_outR1 : main_loop_out)))))))))))));
  initial __resumption_tag <= 20'h10000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 20'h10000;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ZLL_Pure_dispatch (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop3_in;
  logic [0:0] lit_in;
  assign zll_main_loop3_in = arg0;
  assign lit_in = zll_main_loop3_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h0020000 : 28'h0130000;
endmodule

module ZLL_Pure_dispatch2 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop5_in;
  logic [0:0] lit_in;
  assign zll_main_loop5_in = arg0;
  assign lit_in = zll_main_loop5_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h0140000 : 28'h0150000;
endmodule

module ZLL_Pure_dispatch4 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop7_in;
  logic [0:0] lit_in;
  assign zll_main_loop7_in = arg0;
  assign lit_in = zll_main_loop7_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h0170000 : 28'h0260000;
endmodule

module ZLL_Pure_dispatch5 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop8_in;
  logic [0:0] lit_in;
  assign zll_main_loop8_in = arg0;
  assign lit_in = zll_main_loop8_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h0280000 : 28'h0390000;
endmodule

module ZLL_Pure_dispatch8 (input logic [0:0] arg0,
  output logic [27:0] res);
  logic [0:0] zll_main_loop11_in;
  logic [0:0] lit_in;
  assign zll_main_loop11_in = arg0;
  assign lit_in = zll_main_loop11_in[0];
  assign res = (lit_in[0] == 1'h1) ? 28'h03a0000 : 28'h05b0000;
endmodule

module Main_loop (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [27:0] res);
  assign res = {arg0, 4'h0, arg1, arg0};
endmodule