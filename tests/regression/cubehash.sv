module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [1023:0] __out0);
  logic [1024:0] zll_pure_dispatch_in;
  logic [0:0] __continue;
  logic [1:0] __padding;
  logic [1023:0] __st0;
  logic [1023:0] __st0_next;
  assign zll_pure_dispatch_in = {__in0, __st0};
  assign {__continue, __padding, __out0, __st0_next} = {{11'h402{1'h0}}, zll_pure_dispatch_in[1024], zll_pure_dispatch_in[1023:0]};
  initial __st0 <= {93'h40000000020000001, {10'h3a3{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= {93'h40000000020000001, {10'h3a3{1'h0}}};
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule