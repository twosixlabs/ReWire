module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [0:0] __out0,
  output logic [15:0] __out1);
  logic [26:0] gzdLLzicase4975;
  logic [36:0] callRes;
  logic [26:0] gzdLLzicase4978;
  logic [36:0] callResR1;
  logic [26:0] gzdLLzicase4980;
  logic [36:0] callResR2;
  logic [26:0] gzdLLzicase4983;
  logic [36:0] callResR3;
  logic [26:0] gzdLLzicase4987;
  logic [36:0] callResR4;
  logic [26:0] gzdLLzicase4980R1;
  logic [36:0] callResR5;
  logic [0:0] __continue;
  logic [18:0] __resumption_tag;
  logic [18:0] __resumption_tag_next;
  assign gzdLLzicase4975 = {__resumption_tag, __in0};
  zdLLzicase4975  zdLLzicase4975 (gzdLLzicase4975[7:0], callRes);
  assign gzdLLzicase4978 = {__resumption_tag, __in0};
  zdLLzicase4978  zdLLzicase4978 (gzdLLzicase4978[15:8], gzdLLzicase4978[7:0], callResR1);
  assign gzdLLzicase4980 = {__resumption_tag, __in0};
  zdLLzicase4980  zdLLzicase4980 (gzdLLzicase4980[7:0], callResR2);
  assign gzdLLzicase4983 = {__resumption_tag, __in0};
  zdLLzicase4983  zdLLzicase4983 (gzdLLzicase4983[15:8], gzdLLzicase4983[7:0], callResR3);
  assign gzdLLzicase4987 = {__resumption_tag, __in0};
  zdLLzicase4987  zdLLzicase4987 (gzdLLzicase4987[23:16], gzdLLzicase4987[15:8], gzdLLzicase4987[7:0], callResR4);
  assign gzdLLzicase4980R1 = {__resumption_tag, __in0};
  zdLLzicase4980  zdLLzicase4980R1 (gzdLLzicase4980R1[7:0], callResR5);
  assign {__continue, __out0, __out1, __resumption_tag_next} = (gzdLLzicase4980R1[26:24] == 3'h0) ? callResR5 : ((gzdLLzicase4987[26:24] == 3'h1) ? callResR4 : ((gzdLLzicase4983[26:24] == 3'h2) ? callResR3 : ((gzdLLzicase4980[26:24] == 3'h3) ? callResR2 : ((gzdLLzicase4978[26:24] == 3'h4) ? callResR1 : callRes))));
  initial __resumption_tag <= 19'h00000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 19'h00000;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase4975 (input logic [7:0] arg0,
  output logic [36:0] res);
  logic [7:0] gzdLLzilambda4950;
  assign gzdLLzilambda4950 = arg0;
  assign res = {29'h10000400, gzdLLzilambda4950[7:0]};
endmodule

module zdLLzicase4978 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [36:0] res);
  logic [15:0] gzdLLzilambda4948;
  logic [15:0] gMainzif4700;
  logic [15:0] binOp;
  logic [15:0] binOpR1;
  logic [15:0] binOpR2;
  logic [15:0] binOpR3;
  logic [15:0] binOpR4;
  logic [15:0] binOpR5;
  logic [15:0] binOpR6;
  logic [15:0] binOpR7;
  assign gzdLLzilambda4948 = {arg0, arg1};
  assign gMainzif4700 = {gzdLLzilambda4948[15:8], gzdLLzilambda4948[7:0]};
  assign binOp = {8'h00, gMainzif4700[15:8]};
  assign binOpR1 = {8'h00, gMainzif4700[7:0]};
  assign binOpR2 = {binOp[15:8] & binOp[7:0], binOpR1[15:8] & binOpR1[7:0]};
  assign binOpR3 = {gMainzif4700[15:8], gMainzif4700[7:0]};
  assign binOpR4 = {binOpR2[15:8] | binOpR2[7:0], binOpR3[15:8] & binOpR3[7:0]};
  assign binOpR5 = {binOpR4[15:8] | binOpR4[7:0], 8'h01};
  assign binOpR6 = {8'h00, gMainzif4700[15:8]};
  assign binOpR7 = {binOpR6[15:8] ^ binOpR6[7:0], gMainzif4700[7:0]};
  assign res = {2'h3, {binOpR5[15:8] << binOpR5[7:0], binOpR7[15:8] ^ binOpR7[7:0]}, 19'h30000};
endmodule

module zdLLzicase4980 (input logic [7:0] arg0,
  output logic [36:0] res);
  logic [7:0] gMainzipcsa;
  assign gMainzipcsa = arg0;
  assign res = {29'h10000200, gMainzipcsa[7:0]};
endmodule

module zdLLzicase4983 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [36:0] res);
  logic [15:0] gzdLLzilambda4943;
  assign gzdLLzilambda4943 = {arg0, arg1};
  assign res = {21'h100001, gzdLLzilambda4943[15:8], gzdLLzilambda4943[7:0]};
endmodule

module zdLLzicase4987 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  output logic [36:0] res);
  logic [23:0] gzdLLzilambda4940;
  logic [23:0] gMainzif;
  logic [15:0] binOp;
  logic [15:0] binOpR1;
  logic [15:0] binOpR2;
  logic [15:0] binOpR3;
  logic [15:0] binOpR4;
  logic [15:0] binOpR5;
  logic [15:0] binOpR6;
  logic [15:0] binOpR7;
  assign gzdLLzilambda4940 = {arg0, arg1, arg2};
  assign gMainzif = {gzdLLzilambda4940[23:16], gzdLLzilambda4940[15:8], gzdLLzilambda4940[7:0]};
  assign binOp = {gMainzif[23:16], gMainzif[15:8]};
  assign binOpR1 = {gMainzif[23:16], gMainzif[7:0]};
  assign binOpR2 = {binOp[15:8] & binOp[7:0], binOpR1[15:8] & binOpR1[7:0]};
  assign binOpR3 = {gMainzif[15:8], gMainzif[7:0]};
  assign binOpR4 = {binOpR2[15:8] | binOpR2[7:0], binOpR3[15:8] & binOpR3[7:0]};
  assign binOpR5 = {binOpR4[15:8] | binOpR4[7:0], 8'h01};
  assign binOpR6 = {gMainzif[23:16], gMainzif[15:8]};
  assign binOpR7 = {binOpR6[15:8] ^ binOpR6[7:0], gMainzif[7:0]};
  assign res = {2'h3, {binOpR5[15:8] << binOpR5[7:0], binOpR7[15:8] ^ binOpR7[7:0]}, 19'h00000};
endmodule