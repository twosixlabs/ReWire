module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] gzdLLziMainziincr;
  logic [7:0] gMainziincr;
  logic [7:0] gMainzigrunt;
  logic [15:0] gzdLLziMainzigrunt2;
  logic [15:0] gzdLLziMainzigrunt;
  logic [7:0] gzdLLziMainziincr10;
  logic [17:0] gzdLLziMainziincr9;
  logic [17:0] gzdLLziMainziincr8;
  logic [7:0] gzdLLziMainziincr2;
  logic [15:0] gzdLLziMainziincr7;
  logic [15:0] gzdLLziMainziincr5;
  logic [17:0] gzdLLziMainziincr4;
  logic [17:0] gzdLLziMainziincr3;
  logic [15:0] gzdLLziMainziincr1;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign gzdLLziMainziincr = {__in0, __st0};
  assign gMainziincr = gzdLLziMainziincr[7:0];
  assign gMainzigrunt = gMainziincr[7:0];
  assign gzdLLziMainzigrunt2 = {gMainzigrunt[7:0], gMainzigrunt[7:0]};
  assign gzdLLziMainzigrunt = gzdLLziMainzigrunt2[15:0];
  assign gzdLLziMainziincr10 = gzdLLziMainzigrunt[15:8];
  assign gzdLLziMainziincr9 = {10'h100, gzdLLziMainziincr10[7:0]};
  assign gzdLLziMainziincr8 = gzdLLziMainziincr9[17:0];
  assign gzdLLziMainziincr2 = gzdLLziMainziincr8[7:0];
  assign gzdLLziMainziincr7 = {gzdLLziMainziincr2[7:0], gzdLLziMainziincr2[7:0]};
  assign gzdLLziMainziincr5 = gzdLLziMainziincr7[15:0];
  assign gzdLLziMainziincr4 = {2'h0, gzdLLziMainziincr5[15:8], gzdLLziMainziincr5[7:0]};
  assign gzdLLziMainziincr3 = gzdLLziMainziincr4[17:0];
  assign gzdLLziMainziincr1 = {gzdLLziMainziincr3[15:8], gzdLLziMainziincr3[7:0]};
  assign {__continue, __padding, __out0, __st0_next} = {2'h2, gzdLLziMainziincr1[15:8], gzdLLziMainziincr1[7:0]};
  initial __st0 <= 8'h00;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h00;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule