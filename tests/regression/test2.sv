module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] zll_main_incr2_in;
  logic [7:0] main_incr_in;
  logic [15:0] zll_main_incr14_in;
  logic [15:0] zll_main_incr11_in;
  logic [17:0] zll_main_incr13_in;
  logic [17:0] zll_main_incr_in;
  logic [15:0] zll_main_incr5_in;
  logic [7:0] zll_main_incr17_in;
  logic [25:0] zll_main_incr7_in;
  logic [25:0] zll_main_incr12_in;
  logic [15:0] zll_main_incr8_in;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign zll_main_incr2_in = {__in0, __st0};
  assign main_incr_in = zll_main_incr2_in[7:0];
  assign zll_main_incr14_in = {main_incr_in[7:0], main_incr_in[7:0]};
  assign zll_main_incr11_in = zll_main_incr14_in[15:0];
  assign zll_main_incr13_in = {2'h0, zll_main_incr11_in[15:8], zll_main_incr11_in[7:0]};
  assign zll_main_incr_in = zll_main_incr13_in[17:0];
  assign zll_main_incr5_in = {zll_main_incr_in[15:8], zll_main_incr_in[7:0]};
  assign zll_main_incr17_in = zll_main_incr5_in[15:8];
  assign zll_main_incr7_in = {zll_main_incr5_in[15:8], {10'h100, zll_main_incr17_in[7:0]}};
  assign zll_main_incr12_in = {zll_main_incr7_in[25:18], zll_main_incr7_in[17:0]};
  assign zll_main_incr8_in = {zll_main_incr12_in[25:18], zll_main_incr12_in[7:0]};
  assign {__continue, __padding, __out0, __st0_next} = {2'h2, zll_main_incr8_in[15:8], zll_main_incr8_in[7:0]};
  initial __st0 <= 8'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h0;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule