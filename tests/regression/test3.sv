module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] zll_main_go_in;
  logic [8:0] zll_main_go2_in;
  logic [16:0] zll_main_go2_out;
  logic [8:0] zll_main_go2_inR1;
  logic [16:0] zll_main_go2_outR1;
  logic [0:0] __continue;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign zll_main_go_in = {__in0, __st0};
  assign zll_main_go2_in = {zll_main_go_in[7:0], zll_main_go_in[8]};
  ZLL_Main_go2  inst (zll_main_go2_in[8:1], zll_main_go2_out);
  assign zll_main_go2_inR1 = {zll_main_go_in[7:0], zll_main_go_in[8]};
  ZLL_Main_go2  instR1 (zll_main_go2_inR1[8:1], zll_main_go2_outR1);
  assign {__continue, __out0, __st0_next} = (zll_main_go2_inR1[0] == 1'h1) ? zll_main_go2_outR1 : zll_main_go2_out;
  initial __st0 <= 8'h00;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h00;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module ZLL_Main_go2 (input logic [7:0] arg0,
  output logic [16:0] res);
  logic [7:0] main_go_in;
  logic [15:0] zll_main_go8_in;
  logic [15:0] zll_main_go6_in;
  logic [16:0] zll_main_go5_in;
  logic [16:0] zll_main_go4_in;
  logic [15:0] zll_main_go1_in;
  assign main_go_in = arg0;
  assign zll_main_go8_in = {main_go_in[7:0], main_go_in[7:0]};
  assign zll_main_go6_in = zll_main_go8_in[15:0];
  assign zll_main_go5_in = {1'h0, zll_main_go6_in[15:8], zll_main_go6_in[7:0]};
  assign zll_main_go4_in = zll_main_go5_in[16:0];
  assign zll_main_go1_in = {zll_main_go4_in[15:8], zll_main_go4_in[7:0]};
  assign res = {1'h1, zll_main_go1_in[15:8], zll_main_go1_in[7:0]};
endmodule