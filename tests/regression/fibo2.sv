module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] zll_pure_dispatch_in;
  logic [16:0] zll_main_loop3_in;
  logic [16:0] zll_main_loop9_in;
  logic [17:0] zll_main_loop7_in;
  logic [16:0] zll_main_loop11_in;
  logic [16:0] zll_main_loop6_in;
  logic [15:0] zll_main_loop1_in;
  logic [15:0] binop_in;
  logic [15:0] main_loop1_in;
  logic [23:0] main_loop1_out;
  logic [16:0] zll_main_loop2_in;
  logic [15:0] zll_main_loop_in;
  logic [15:0] main_loop1_inR1;
  logic [23:0] main_loop1_outR1;
  logic [0:0] __continue;
  logic [15:0] __resumption_tag;
  logic [15:0] __resumption_tag_next;
  assign zll_pure_dispatch_in = {__in0, __resumption_tag};
  assign zll_main_loop3_in = {zll_pure_dispatch_in[15:8], zll_pure_dispatch_in[7:0], zll_pure_dispatch_in[16]};
  assign zll_main_loop9_in = {zll_main_loop3_in[8:1], zll_main_loop3_in[16:9], zll_main_loop3_in[0]};
  assign zll_main_loop7_in = {zll_main_loop9_in[16:9], zll_main_loop9_in[0], zll_main_loop9_in[8:1], zll_main_loop9_in[0]};
  assign zll_main_loop11_in = {zll_main_loop7_in[17:10], zll_main_loop7_in[8:1], zll_main_loop7_in[9]};
  assign zll_main_loop6_in = {zll_main_loop11_in[16:9], zll_main_loop11_in[8:1], zll_main_loop11_in[0]};
  assign zll_main_loop1_in = {zll_main_loop6_in[16:9], zll_main_loop6_in[8:1]};
  assign binop_in = {zll_main_loop1_in[7:0], zll_main_loop1_in[15:8]};
  assign main_loop1_in = {zll_main_loop1_in[15:8], binop_in[15:8] + binop_in[7:0]};
  Main_loop1  inst (main_loop1_in[15:8], main_loop1_in[7:0], main_loop1_out);
  assign zll_main_loop2_in = {zll_main_loop7_in[17:10], zll_main_loop7_in[8:1], zll_main_loop7_in[0]};
  assign zll_main_loop_in = {zll_main_loop2_in[16:9], zll_main_loop2_in[8:1]};
  assign main_loop1_inR1 = {zll_main_loop_in[7:0], zll_main_loop_in[15:8]};
  Main_loop1  instR1 (main_loop1_inR1[15:8], main_loop1_inR1[7:0], main_loop1_outR1);
  assign {__continue, __out0, __resumption_tag_next} = (zll_main_loop2_in[0] == 1'h1) ? main_loop1_outR1 : main_loop1_out;
  initial __resumption_tag <= 16'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 16'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module Main_loop1 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [23:0] res);
  logic [15:0] zll_main_loop4_in;
  logic [15:0] zll_main_loop5_in;
  assign zll_main_loop4_in = {arg0, arg1};
  assign zll_main_loop5_in = zll_main_loop4_in[15:0];
  assign res = {zll_main_loop5_in[15:8], zll_main_loop5_in[15:8], zll_main_loop5_in[7:0]};
endmodule