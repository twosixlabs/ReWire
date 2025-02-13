module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  output logic [7:0] __out0);
  logic [15:0] gzdLLziMainzigo1;
  logic [15:0] binOp;
  logic [7:0] gzdLLziMainzigo5;
  logic [24:0] gzdLLziMainzigo4;
  logic [24:0] gzdLLziMainzigo;
  logic [7:0] gMainzigo;
  logic [15:0] gzdLLziMainzigo12;
  logic [15:0] gzdLLziMainzigo10;
  logic [24:0] gzdLLziMainzigo9;
  logic [24:0] gzdLLziMainzigo8;
  logic [15:0] gzdLLziMainzigo2;
  logic [0:0] __continue;
  logic [7:0] __resumption_tag;
  logic [7:0] __st0;
  logic [7:0] __resumption_tag_next;
  logic [7:0] __st0_next;
  assign gzdLLziMainzigo1 = {__resumption_tag, __st0};
  assign binOp = {gzdLLziMainzigo1[15:8], 8'h01};
  assign gzdLLziMainzigo5 = binOp[15:8] + binOp[7:0];
  assign gzdLLziMainzigo4 = {17'h00100, gzdLLziMainzigo5[7:0]};
  assign gzdLLziMainzigo = gzdLLziMainzigo4[24:0];
  assign gMainzigo = gzdLLziMainzigo[7:0];
  assign gzdLLziMainzigo12 = {gMainzigo[7:0], gMainzigo[7:0]};
  assign gzdLLziMainzigo10 = gzdLLziMainzigo12[15:0];
  assign gzdLLziMainzigo9 = {9'h000, gzdLLziMainzigo10[15:8], gzdLLziMainzigo10[7:0]};
  assign gzdLLziMainzigo8 = gzdLLziMainzigo9[24:0];
  assign gzdLLziMainzigo2 = {gzdLLziMainzigo8[15:8], gzdLLziMainzigo8[7:0]};
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = {1'h1, gzdLLziMainzigo2[15:8], gzdLLziMainzigo2[15:8], gzdLLziMainzigo2[7:0]};
  initial {__resumption_tag, __st0} <= 16'h0000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 16'h0000;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule