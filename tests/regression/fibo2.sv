module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [20:0] gzdLLzicase6261;
  logic [0:0] gzdLLzilambda6215;
  logic [0:0] lit;
  logic [20:0] gzdLLzicase6263;
  logic [0:0] gzdLLzilambda6211;
  logic [0:0] litR1;
  logic [20:0] gzdLLzicase6265;
  logic [0:0] gzdLLzilambda6207;
  logic [0:0] litR2;
  logic [20:0] gzdLLzicase6267;
  logic [0:0] gzdLLzilambda6203;
  logic [28:0] callRes;
  logic [20:0] gzdLLzicase6269;
  logic [0:0] gzdLLzilambda6203R1;
  logic [28:0] callResR1;
  logic [20:0] gzdLLzicase6271;
  logic [0:0] gzdLLzilambda6197;
  logic [28:0] callResR2;
  logic [20:0] gzdLLzicase6273;
  logic [0:0] gzdLLzilambda6195;
  logic [28:0] callResR3;
  logic [20:0] gzdLLzicase6275;
  logic [0:0] gzdLLzilambda6197R1;
  logic [28:0] callResR4;
  logic [20:0] gzdLLzicase6277;
  logic [0:0] gzdLLzilambda6195R1;
  logic [28:0] callResR5;
  logic [20:0] gzdLLzicase6279;
  logic [0:0] gzdLLzilambda6191;
  logic [28:0] callResR6;
  logic [20:0] gzdLLzicase6281;
  logic [0:0] gzdLLzilambda6191R1;
  logic [28:0] callResR7;
  logic [20:0] gzdLLzicase6283;
  logic [0:0] gzdLLzilambda6180;
  logic [28:0] callResR8;
  logic [20:0] gzdLLzicase6285;
  logic [0:0] gzdLLzilambda6180R1;
  logic [28:0] callResR9;
  logic [20:0] gzdLLzicase6289;
  logic [16:0] gzdLLzilambda6171;
  logic [16:0] gzdLLzicase6292;
  logic [15:0] gzdLLzicase6164;
  logic [15:0] binOp;
  logic [15:0] gMainziloop;
  logic [28:0] callResR10;
  logic [16:0] gzdLLzicase6167;
  logic [15:0] gMainziloopR1;
  logic [28:0] callResR11;
  logic [0:0] __continue;
  logic [19:0] __resumption_tag;
  logic [19:0] __resumption_tag_next;
  assign gzdLLzicase6261 = {__resumption_tag, __in0};
  assign gzdLLzilambda6215 = gzdLLzicase6261[0];
  assign lit = gzdLLzilambda6215[0];
  assign gzdLLzicase6263 = {__resumption_tag, __in0};
  assign gzdLLzilambda6211 = gzdLLzicase6263[0];
  assign litR1 = gzdLLzilambda6211[0];
  assign gzdLLzicase6265 = {__resumption_tag, __in0};
  assign gzdLLzilambda6207 = gzdLLzicase6265[0];
  assign litR2 = gzdLLzilambda6207[0];
  assign gzdLLzicase6267 = {__resumption_tag, __in0};
  assign gzdLLzilambda6203 = gzdLLzicase6267[0];
  zdLLzilambda6203  zdLLzilambda6203 (gzdLLzilambda6203[0], callRes);
  assign gzdLLzicase6269 = {__resumption_tag, __in0};
  assign gzdLLzilambda6203R1 = gzdLLzicase6269[0];
  zdLLzilambda6203  zdLLzilambda6203R1 (gzdLLzilambda6203R1[0], callResR1);
  assign gzdLLzicase6271 = {__resumption_tag, __in0};
  assign gzdLLzilambda6197 = gzdLLzicase6271[0];
  zdLLzilambda6197  zdLLzilambda6197 (gzdLLzilambda6197[0], callResR2);
  assign gzdLLzicase6273 = {__resumption_tag, __in0};
  assign gzdLLzilambda6195 = gzdLLzicase6273[0];
  zdLLzilambda6195  zdLLzilambda6195 (gzdLLzilambda6195[0], callResR3);
  assign gzdLLzicase6275 = {__resumption_tag, __in0};
  assign gzdLLzilambda6197R1 = gzdLLzicase6275[0];
  zdLLzilambda6197  zdLLzilambda6197R1 (gzdLLzilambda6197R1[0], callResR4);
  assign gzdLLzicase6277 = {__resumption_tag, __in0};
  assign gzdLLzilambda6195R1 = gzdLLzicase6277[0];
  zdLLzilambda6195  zdLLzilambda6195R1 (gzdLLzilambda6195R1[0], callResR5);
  assign gzdLLzicase6279 = {__resumption_tag, __in0};
  assign gzdLLzilambda6191 = gzdLLzicase6279[0];
  zdLLzilambda6191  zdLLzilambda6191 (gzdLLzilambda6191[0], callResR6);
  assign gzdLLzicase6281 = {__resumption_tag, __in0};
  assign gzdLLzilambda6191R1 = gzdLLzicase6281[0];
  zdLLzilambda6191  zdLLzilambda6191R1 (gzdLLzilambda6191R1[0], callResR7);
  assign gzdLLzicase6283 = {__resumption_tag, __in0};
  assign gzdLLzilambda6180 = gzdLLzicase6283[0];
  zdLLzilambda6180  zdLLzilambda6180 (gzdLLzilambda6180[0], callResR8);
  assign gzdLLzicase6285 = {__resumption_tag, __in0};
  assign gzdLLzilambda6180R1 = gzdLLzicase6285[0];
  zdLLzilambda6180  zdLLzilambda6180R1 (gzdLLzilambda6180R1[0], callResR9);
  assign gzdLLzicase6289 = {__resumption_tag, __in0};
  assign gzdLLzilambda6171 = {gzdLLzicase6289[16:9], gzdLLzicase6289[8:1], gzdLLzicase6289[0]};
  assign gzdLLzicase6292 = {gzdLLzilambda6171[0], gzdLLzilambda6171[8:1], gzdLLzilambda6171[16:9]};
  assign gzdLLzicase6164 = {gzdLLzicase6292[15:8], gzdLLzicase6292[7:0]};
  assign binOp = {gzdLLzicase6164[7:0], gzdLLzicase6164[15:8]};
  assign gMainziloop = {gzdLLzicase6164[15:8], binOp[15:8] + binOp[7:0]};
  Mainziloop  Mainziloop (gMainziloop[15:8], gMainziloop[7:0], callResR10);
  assign gzdLLzicase6167 = {gzdLLzilambda6171[0], gzdLLzilambda6171[16:9], gzdLLzilambda6171[8:1]};
  assign gMainziloopR1 = {gzdLLzicase6167[15:8], gzdLLzicase6167[7:0]};
  Mainziloop  MainziloopR1 (gMainziloopR1[15:8], gMainziloopR1[7:0], callResR11);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase6289[20:17] == 4'h0) ? ((gzdLLzicase6167[16] == 1'h1) ? callResR11 : callResR10) : ((gzdLLzicase6285[20:17] == 4'h1) ? callResR9 : ((gzdLLzicase6283[20:17] == 4'h2) ? callResR8 : ((gzdLLzicase6281[20:17] == 4'h3) ? callResR7 : ((gzdLLzicase6279[20:17] == 4'h4) ? callResR6 : ((gzdLLzicase6277[20:17] == 4'h5) ? callResR5 : ((gzdLLzicase6275[20:17] == 4'h6) ? callResR4 : ((gzdLLzicase6273[20:17] == 4'h7) ? callResR3 : ((gzdLLzicase6271[20:17] == 4'h8) ? callResR2 : ((gzdLLzicase6269[20:17] == 4'h9) ? callResR1 : ((gzdLLzicase6267[20:17] == 4'ha) ? callRes : ((gzdLLzicase6265[20:17] == 4'hb) ? ((litR2[0] == 1'h1) ? 29'h105d0000 : 29'h108c0000) : ((gzdLLzicase6263[20:17] == 4'hc) ? ((litR1[0] == 1'h1) ? 29'h1080080d : 29'h10d00d15) : ((lit[0] == 1'h1) ? 29'h10500508 : 29'h1080080d)))))))))))));
  initial __resumption_tag <= 20'h10000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 20'h10000;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzilambda6180 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10020000 : 29'h10130000;
endmodule

module zdLLzilambda6191 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10140000 : 29'h10150000;
endmodule

module zdLLzilambda6195 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10170000 : 29'h10260000;
endmodule

module zdLLzilambda6197 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10280000 : 29'h10390000;
endmodule

module zdLLzilambda6203 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h103a0000 : 29'h105b0000;
endmodule

module Mainziloop (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [28:0] res);
  assign res = {1'h1, arg0, 4'h0, arg0, arg1};
endmodule