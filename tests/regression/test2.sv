module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] gzdLLziMainziincr;
  logic [7:0] gMainziincr;
  logic [15:0] gzdLLziMainziincr13;
  logic [15:0] gzdLLziMainziincr11;
  logic [17:0] gzdLLziMainziincr10;
  logic [17:0] gzdLLziMainziincr9;
  logic [15:0] gzdLLziMainziincr2;
  logic [7:0] gzdLLziMainziincr6;
  logic [25:0] gzdLLziMainziincr5;
  logic [25:0] gzdLLziMainziincr3;
  logic [15:0] gzdLLziMainziincr1;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign gzdLLziMainziincr = {__in0, __st0};
  assign gMainziincr = gzdLLziMainziincr[7:0];
  assign gzdLLziMainziincr13 = {gMainziincr[7:0], gMainziincr[7:0]};
  assign gzdLLziMainziincr11 = gzdLLziMainziincr13[15:0];
  assign gzdLLziMainziincr10 = {2'h0, gzdLLziMainziincr11[15:8], gzdLLziMainziincr11[7:0]};
  assign gzdLLziMainziincr9 = gzdLLziMainziincr10[17:0];
  assign gzdLLziMainziincr2 = {gzdLLziMainziincr9[15:8], gzdLLziMainziincr9[7:0]};
  assign gzdLLziMainziincr6 = gzdLLziMainziincr2[15:8];
  assign gzdLLziMainziincr5 = {gzdLLziMainziincr2[15:8], {10'h100, gzdLLziMainziincr6[7:0]}};
  assign gzdLLziMainziincr3 = {gzdLLziMainziincr5[25:18], gzdLLziMainziincr5[17:0]};
  assign gzdLLziMainziincr1 = {gzdLLziMainziincr3[25:18], gzdLLziMainziincr3[7:0]};
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