module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [0:0] __out0,
  output logic [7:0] __out1);
  logic [15:0] zll_main_start$_in;
  logic [7:0] main_start$_in;
  logic [0:0] __continue;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign zll_main_start$_in = {__in0, __st0};
  assign main_start$_in = zll_main_start$_in[7:0];
  assign {__continue, __out0, __out1, __st0_next} = {9'h000, main_start$_in[7:0]};
  initial __st0 <= 8'h00;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h00;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule