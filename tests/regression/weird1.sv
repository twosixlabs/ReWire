module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] main_repl_in;
  logic [0:0] __continue;
  logic [0:0] __st0;
  logic [0:0] __st1;
  logic [0:0] __st0_next;
  logic [0:0] __st1_next;
  assign main_repl_in = {__in0, {__st0, __st1}};
  assign {__continue, __out0, __st0_next, __st1_next} = {1'h0, main_repl_in[1:0]};
  initial {__st0, __st1} <= 2'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 2'h0;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule