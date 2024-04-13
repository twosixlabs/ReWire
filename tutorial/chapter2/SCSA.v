module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  input logic [7:0] __in1,
  input logic [7:0] __in2,
  output logic [7:0] __out0,
  output logic [7:0] __out1);
  logic [40:0] gzdLLzicase5407;
  logic [33:0] callRes;
  logic [40:0] gzdLLzicase5407R1;
  logic [33:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [7:0] __st0;
  logic [7:0] __st1;
  logic [0:0] __resumption_tag_next;
  logic [7:0] __st0_next;
  logic [7:0] __st1_next;
  assign gzdLLzicase5407 = {{__resumption_tag, __st0, __st1}, {__in0, __in1, __in2}};
  zdLLzicase5407  zdLLzicase5407 (gzdLLzicase5407[39:24], gzdLLzicase5407[23:0], callRes);
  assign gzdLLzicase5407R1 = {{__resumption_tag, __st0, __st1}, {__in0, __in1, __in2}};
  zdLLzicase5407  zdLLzicase5407R1 (gzdLLzicase5407R1[39:24], gzdLLzicase5407R1[23:0], callResR1);
  assign {__continue, __out0, __out1, __resumption_tag_next, __st0_next, __st1_next} = (gzdLLzicase5407R1[40] == 1'h0) ? callResR1 : callRes;
  initial {__resumption_tag, __st0, __st1} <= 17'h00000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 17'h00000;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module zdLLzicase5407 (input logic [15:0] arg0,
  input logic [23:0] arg1,
  output logic [33:0] res);
  logic [39:0] gMainziscsa;
  logic [39:0] gzdLLzicase5346;
  logic [23:0] gMainzif;
  logic [15:0] binOp;
  logic [15:0] binOpR1;
  logic [15:0] binOpR2;
  logic [15:0] binOpR3;
  logic [15:0] binOpR4;
  logic [15:0] binOpR5;
  logic [15:0] binOpR6;
  logic [15:0] binOpR7;
  logic [15:0] gzdLLzilambda5342;
  logic [31:0] gzdLLzilambda5415;
  logic [31:0] gzdLLzicase5413;
  logic [33:0] gzdLLzilambda5425;
  logic [33:0] gzdLLzicase5423;
  logic [31:0] gzdLLzilambda5350;
  assign gMainziscsa = {arg1, arg0};
  assign gzdLLzicase5346 = {gMainziscsa[39:16], gMainziscsa[15:0]};
  assign gMainzif = {gzdLLzicase5346[39:32], gzdLLzicase5346[31:24], gzdLLzicase5346[23:16]};
  assign binOp = {gMainzif[23:16], gMainzif[15:8]};
  assign binOpR1 = {gMainzif[23:16], gMainzif[7:0]};
  assign binOpR2 = {binOp[15:8] & binOp[7:0], binOpR1[15:8] & binOpR1[7:0]};
  assign binOpR3 = {gMainzif[15:8], gMainzif[7:0]};
  assign binOpR4 = {binOpR2[15:8] | binOpR2[7:0], binOpR3[15:8] & binOpR3[7:0]};
  assign binOpR5 = {binOpR4[15:8] | binOpR4[7:0], 8'h01};
  assign binOpR6 = {gMainzif[23:16], gMainzif[15:8]};
  assign binOpR7 = {binOpR6[15:8] ^ binOpR6[7:0], gMainzif[7:0]};
  assign gzdLLzilambda5342 = {binOpR5[15:8] << binOpR5[7:0], binOpR7[15:8] ^ binOpR7[7:0]};
  assign gzdLLzilambda5415 = {gzdLLzilambda5342[15:0], gzdLLzilambda5342[15:0]};
  assign gzdLLzicase5413 = gzdLLzilambda5415[31:0];
  assign gzdLLzilambda5425 = {2'h0, gzdLLzicase5413[31:16], gzdLLzicase5413[15:0]};
  assign gzdLLzicase5423 = gzdLLzilambda5425[33:0];
  assign gzdLLzilambda5350 = {gzdLLzicase5423[31:16], gzdLLzicase5423[15:0]};
  assign res = {1'h1, gzdLLzilambda5350[31:16], 1'h0, gzdLLzilambda5350[15:0]};
endmodule