module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] zll_main_incr12_in;
  logic [7:0] main_incr_in;
  logic [7:0] main_grunt_in;
  logic [15:0] zll_main_grunt1_in;
  logic [15:0] zll_main_grunt2_in;
  logic [7:0] zll_main_incr3_in;
  logic [17:0] zll_main_incr7_in;
  logic [17:0] zll_main_incr6_in;
  logic [7:0] zll_main_incr9_in;
  logic [15:0] zll_main_incr13_in;
  logic [15:0] zll_main_incr10_in;
  logic [17:0] zll_main_incr11_in;
  logic [17:0] zll_main_incr_in;
  logic [15:0] zll_main_incr2_in;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign zll_main_incr12_in = {__in0, __st0};
  assign main_incr_in = zll_main_incr12_in[7:0];
  assign main_grunt_in = main_incr_in[7:0];
  assign zll_main_grunt1_in = {main_grunt_in[7:0], main_grunt_in[7:0]};
  assign zll_main_grunt2_in = zll_main_grunt1_in[15:0];
  assign zll_main_incr3_in = zll_main_grunt2_in[15:8];
  assign zll_main_incr7_in = {10'h100, zll_main_incr3_in[7:0]};
  assign zll_main_incr6_in = zll_main_incr7_in[17:0];
  assign zll_main_incr9_in = zll_main_incr6_in[7:0];
  assign zll_main_incr13_in = {zll_main_incr9_in[7:0], zll_main_incr9_in[7:0]};
  assign zll_main_incr10_in = zll_main_incr13_in[15:0];
  assign zll_main_incr11_in = {2'h0, zll_main_incr10_in[15:8], zll_main_incr10_in[7:0]};
  assign zll_main_incr_in = zll_main_incr11_in[17:0];
  assign zll_main_incr2_in = {zll_main_incr_in[15:8], zll_main_incr_in[7:0]};
  assign {__continue, __padding, __out0, __st0_next} = {2'h2, zll_main_incr2_in[15:8], zll_main_incr2_in[7:0]};
  initial __st0 <= 8'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule