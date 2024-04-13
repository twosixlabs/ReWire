module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  input logic [7:0] __in1,
  input logic [7:0] __in2,
  output logic [7:0] __out0,
  output logic [7:0] __out1);
  logic [24:0] gzdLLzicase4955;
  logic [17:0] callRes;
  logic [24:0] gzdLLzicase4955R1;
  logic [17:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLzicase4955 = {__resumption_tag, {__in0, __in1, __in2}};
  zdLLzicase4955  zdLLzicase4955 (gzdLLzicase4955[23:0], callRes);
  assign gzdLLzicase4955R1 = {__resumption_tag, {__in0, __in1, __in2}};
  zdLLzicase4955  zdLLzicase4955R1 (gzdLLzicase4955R1[23:0], callResR1);
  assign {__continue, __out0, __out1, __resumption_tag_next} = (gzdLLzicase4955R1[24] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase4955 (input logic [23:0] arg0,
  output logic [17:0] res);
  logic [23:0] gMainzicsa;
  logic [23:0] gzdLLzicase4938;
  logic [23:0] gMainzif;
  logic [15:0] binOp;
  logic [15:0] binOpR1;
  logic [15:0] binOpR2;
  logic [15:0] binOpR3;
  logic [15:0] binOpR4;
  logic [15:0] binOpR5;
  logic [15:0] binOpR6;
  logic [15:0] binOpR7;
  assign gMainzicsa = arg0;
  assign gzdLLzicase4938 = gMainzicsa[23:0];
  assign gMainzif = {gzdLLzicase4938[23:16], gzdLLzicase4938[15:8], gzdLLzicase4938[7:0]};
  assign binOp = {gMainzif[23:16], gMainzif[15:8]};
  assign binOpR1 = {gMainzif[23:16], gMainzif[7:0]};
  assign binOpR2 = {binOp[15:8] & binOp[7:0], binOpR1[15:8] & binOpR1[7:0]};
  assign binOpR3 = {gMainzif[15:8], gMainzif[7:0]};
  assign binOpR4 = {binOpR2[15:8] | binOpR2[7:0], binOpR3[15:8] & binOpR3[7:0]};
  assign binOpR5 = {binOpR4[15:8] | binOpR4[7:0], 8'h01};
  assign binOpR6 = {gMainzif[23:16], gMainzif[15:8]};
  assign binOpR7 = {binOpR6[15:8] ^ binOpR6[7:0], gMainzif[7:0]};
  assign res = {1'h1, {binOpR5[15:8] << binOpR5[7:0], binOpR7[15:8] ^ binOpR7[7:0]}, 1'h0};
endmodule