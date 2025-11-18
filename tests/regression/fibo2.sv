module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] zll_pure_dispatch1_in;
  logic [16:0] zll_pure_dispatch_in;
  logic [16:0] zll_main_loop_in;
  logic [17:0] zll_main_loop7_in;
  logic [16:0] zll_main_loop3_in;
  logic [16:0] zll_main_loop10_in;
  logic [15:0] zll_main_loop6_in;
  logic [15:0] binop_in;
  logic [15:0] main_loop_in;
  logic [23:0] main_loop_out;
  logic [16:0] zll_main_loop5_in;
  logic [15:0] zll_main_loop2_in;
  logic [15:0] main_loop_inR1;
  logic [23:0] main_loop_outR1;
  logic [0:0] __continue;
  logic [15:0] __resumption_tag;
  logic [15:0] __resumption_tag_next;
  assign zll_pure_dispatch1_in = {__in0, __resumption_tag};
  assign zll_pure_dispatch_in = {zll_pure_dispatch1_in[15:8], zll_pure_dispatch1_in[16], zll_pure_dispatch1_in[7:0]};
  assign zll_main_loop_in = {zll_pure_dispatch_in[16:9], zll_pure_dispatch_in[7:0], zll_pure_dispatch_in[8]};
  assign zll_main_loop7_in = {zll_main_loop_in[16:9], zll_main_loop_in[8:1], zll_main_loop_in[0], zll_main_loop_in[0]};
  assign zll_main_loop3_in = {zll_main_loop7_in[17:10], zll_main_loop7_in[9:2], zll_main_loop7_in[1]};
  assign zll_main_loop10_in = {zll_main_loop3_in[16:9], zll_main_loop3_in[8:1], zll_main_loop3_in[0]};
  assign zll_main_loop6_in = {zll_main_loop10_in[16:9], zll_main_loop10_in[8:1]};
  assign binop_in = {zll_main_loop6_in[7:0], zll_main_loop6_in[15:8]};
  assign main_loop_in = {zll_main_loop6_in[15:8], binop_in[15:8] + binop_in[7:0]};
  Main_loop  inst (main_loop_in[15:8], main_loop_in[7:0], main_loop_out);
  assign zll_main_loop5_in = {zll_main_loop7_in[17:10], zll_main_loop7_in[9:2], zll_main_loop7_in[0]};
  assign zll_main_loop2_in = {zll_main_loop5_in[16:9], zll_main_loop5_in[8:1]};
  assign main_loop_inR1 = {zll_main_loop2_in[7:0], zll_main_loop2_in[15:8]};
  Main_loop  instR1 (main_loop_inR1[15:8], main_loop_inR1[7:0], main_loop_outR1);
  assign {__continue, __out0, __resumption_tag_next} = (zll_main_loop5_in[0] == 1'h1) ? main_loop_outR1 : main_loop_out;
  initial __resumption_tag <= 16'h100;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 16'h100;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module Main_loop (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [23:0] res);
  logic [15:0] zll_main_loop8_in;
  logic [15:0] zll_main_loop4_in;
  assign zll_main_loop8_in = {arg0, arg1};
  assign zll_main_loop4_in = zll_main_loop8_in[15:0];
  assign res = {zll_main_loop4_in[15:8], zll_main_loop4_in[7:0], zll_main_loop4_in[15:8]};
endmodule