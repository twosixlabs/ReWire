module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  output logic [7:0] __out0);
  logic [15:0] zll_main_go10_in;
  logic [15:0] binop_in;
  logic [7:0] zll_main_go4_in;
  logic [24:0] zll_main_go1_in;
  logic [24:0] zll_main_go11_in;
  logic [7:0] main_go_in;
  logic [15:0] zll_main_go16_in;
  logic [15:0] zll_main_go14_in;
  logic [24:0] zll_main_go9_in;
  logic [24:0] zll_main_go7_in;
  logic [15:0] zll_main_go5_in;
  logic [0:0] __continue;
  logic [7:0] __resumption_tag;
  logic [7:0] __st0;
  logic [7:0] __resumption_tag_next;
  logic [7:0] __st0_next;
  assign zll_main_go10_in = {__resumption_tag, __st0};
  assign binop_in = {zll_main_go10_in[15:8], 8'h1};
  assign zll_main_go4_in = binop_in[15:8] + binop_in[7:0];
  assign zll_main_go1_in = {17'h100, zll_main_go4_in[7:0]};
  assign zll_main_go11_in = zll_main_go1_in[24:0];
  assign main_go_in = zll_main_go11_in[7:0];
  assign zll_main_go16_in = {main_go_in[7:0], main_go_in[7:0]};
  assign zll_main_go14_in = zll_main_go16_in[15:0];
  assign zll_main_go9_in = {9'h0, zll_main_go14_in[15:8], zll_main_go14_in[7:0]};
  assign zll_main_go7_in = zll_main_go9_in[24:0];
  assign zll_main_go5_in = {zll_main_go7_in[15:8], zll_main_go7_in[7:0]};
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = {1'h1, zll_main_go5_in[15:8], zll_main_go5_in[15:8], zll_main_go5_in[7:0]};
  initial {__resumption_tag, __st0} <= 16'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 16'h0;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule