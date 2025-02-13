module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [20:0] gzdLLziPurezidispatch15;
  logic [16:0] gzdLLziPurezidispatch14;
  logic [16:0] gzdLLziPurezidispatch13;
  logic [16:0] gzdLLziMainziloop;
  logic [16:0] gzdLLziMainziloop21;
  logic [15:0] gzdLLziMainziloop2;
  logic [15:0] binOp;
  logic [15:0] gMainziloop;
  logic [28:0] callRes;
  logic [16:0] gzdLLziMainziloop20;
  logic [15:0] gzdLLziMainziloop1;
  logic [15:0] gMainziloopR1;
  logic [28:0] callResR1;
  logic [20:0] gzdLLziPurezidispatch12;
  logic [0:0] gzdLLziMainziloop17;
  logic [0:0] lit;
  logic [20:0] gzdLLziPurezidispatch11;
  logic [0:0] gzdLLziMainziloop14;
  logic [0:0] litR1;
  logic [20:0] gzdLLziPurezidispatch10;
  logic [0:0] gzdLLziMainziloop13;
  logic [0:0] litR2;
  logic [20:0] gzdLLziPurezidispatch9;
  logic [0:0] gzdLLziMainziloop11;
  logic [28:0] callResR2;
  logic [20:0] gzdLLziPurezidispatch8;
  logic [0:0] gzdLLziMainziloop11R1;
  logic [28:0] callResR3;
  logic [20:0] gzdLLziPurezidispatch7;
  logic [0:0] gzdLLziMainziloop8;
  logic [28:0] callResR4;
  logic [20:0] gzdLLziPurezidispatch6;
  logic [0:0] gzdLLziMainziloop7;
  logic [28:0] callResR5;
  logic [20:0] gzdLLziPurezidispatch5;
  logic [0:0] gzdLLziMainziloop8R1;
  logic [28:0] callResR6;
  logic [20:0] gzdLLziPurezidispatch4;
  logic [0:0] gzdLLziMainziloop7R1;
  logic [28:0] callResR7;
  logic [20:0] gzdLLziPurezidispatch3;
  logic [0:0] gzdLLziMainziloop5;
  logic [28:0] callResR8;
  logic [20:0] gzdLLziPurezidispatch2;
  logic [0:0] gzdLLziMainziloop5R1;
  logic [28:0] callResR9;
  logic [20:0] gzdLLziPurezidispatch1;
  logic [0:0] gzdLLziMainziloop3;
  logic [28:0] callResR10;
  logic [20:0] gzdLLziPurezidispatch;
  logic [0:0] gzdLLziMainziloop3R1;
  logic [28:0] callResR11;
  logic [0:0] __continue;
  logic [19:0] __resumption_tag;
  logic [19:0] __resumption_tag_next;
  assign gzdLLziPurezidispatch15 = {__in0, __resumption_tag};
  assign gzdLLziPurezidispatch14 = {gzdLLziPurezidispatch15[20], gzdLLziPurezidispatch15[15:8], gzdLLziPurezidispatch15[7:0]};
  assign gzdLLziPurezidispatch13 = {gzdLLziPurezidispatch14[15:8], gzdLLziPurezidispatch14[16], gzdLLziPurezidispatch14[7:0]};
  assign gzdLLziMainziloop = {gzdLLziPurezidispatch13[16:9], gzdLLziPurezidispatch13[7:0], gzdLLziPurezidispatch13[8]};
  assign gzdLLziMainziloop21 = {gzdLLziMainziloop[16:9], gzdLLziMainziloop[8:1], gzdLLziMainziloop[0]};
  assign gzdLLziMainziloop2 = {gzdLLziMainziloop21[16:9], gzdLLziMainziloop21[8:1]};
  assign binOp = {gzdLLziMainziloop2[7:0], gzdLLziMainziloop2[15:8]};
  assign gMainziloop = {gzdLLziMainziloop2[15:8], binOp[15:8] + binOp[7:0]};
  Mainziloop  Mainziloop (gMainziloop[15:8], gMainziloop[7:0], callRes);
  assign gzdLLziMainziloop20 = {gzdLLziMainziloop[16:9], gzdLLziMainziloop[8:1], gzdLLziMainziloop[0]};
  assign gzdLLziMainziloop1 = {gzdLLziMainziloop20[16:9], gzdLLziMainziloop20[8:1]};
  assign gMainziloopR1 = {gzdLLziMainziloop1[7:0], gzdLLziMainziloop1[15:8]};
  Mainziloop  MainziloopR1 (gMainziloopR1[15:8], gMainziloopR1[7:0], callResR1);
  assign gzdLLziPurezidispatch12 = {__in0, __resumption_tag};
  assign gzdLLziMainziloop17 = gzdLLziPurezidispatch12[20];
  assign lit = gzdLLziMainziloop17[0];
  assign gzdLLziPurezidispatch11 = {__in0, __resumption_tag};
  assign gzdLLziMainziloop14 = gzdLLziPurezidispatch11[20];
  assign litR1 = gzdLLziMainziloop14[0];
  assign gzdLLziPurezidispatch10 = {__in0, __resumption_tag};
  assign gzdLLziMainziloop13 = gzdLLziPurezidispatch10[20];
  assign litR2 = gzdLLziMainziloop13[0];
  assign gzdLLziPurezidispatch9 = {__in0, __resumption_tag};
  assign gzdLLziMainziloop11 = gzdLLziPurezidispatch9[20];
  zdLLziMainziloop11  zdLLziMainziloop11 (gzdLLziMainziloop11[0], callResR2);
  assign gzdLLziPurezidispatch8 = {__in0, __resumption_tag};
  assign gzdLLziMainziloop11R1 = gzdLLziPurezidispatch8[20];
  zdLLziMainziloop11  zdLLziMainziloop11R1 (gzdLLziMainziloop11R1[0], callResR3);
  assign gzdLLziPurezidispatch7 = {__in0, __resumption_tag};
  assign gzdLLziMainziloop8 = gzdLLziPurezidispatch7[20];
  zdLLziMainziloop8  zdLLziMainziloop8 (gzdLLziMainziloop8[0], callResR4);
  assign gzdLLziPurezidispatch6 = {__in0, __resumption_tag};
  assign gzdLLziMainziloop7 = gzdLLziPurezidispatch6[20];
  zdLLziMainziloop7  zdLLziMainziloop7 (gzdLLziMainziloop7[0], callResR5);
  assign gzdLLziPurezidispatch5 = {__in0, __resumption_tag};
  assign gzdLLziMainziloop8R1 = gzdLLziPurezidispatch5[20];
  zdLLziMainziloop8  zdLLziMainziloop8R1 (gzdLLziMainziloop8R1[0], callResR6);
  assign gzdLLziPurezidispatch4 = {__in0, __resumption_tag};
  assign gzdLLziMainziloop7R1 = gzdLLziPurezidispatch4[20];
  zdLLziMainziloop7  zdLLziMainziloop7R1 (gzdLLziMainziloop7R1[0], callResR7);
  assign gzdLLziPurezidispatch3 = {__in0, __resumption_tag};
  assign gzdLLziMainziloop5 = gzdLLziPurezidispatch3[20];
  zdLLziMainziloop5  zdLLziMainziloop5 (gzdLLziMainziloop5[0], callResR8);
  assign gzdLLziPurezidispatch2 = {__in0, __resumption_tag};
  assign gzdLLziMainziloop5R1 = gzdLLziPurezidispatch2[20];
  zdLLziMainziloop5  zdLLziMainziloop5R1 (gzdLLziMainziloop5R1[0], callResR9);
  assign gzdLLziPurezidispatch1 = {__in0, __resumption_tag};
  assign gzdLLziMainziloop3 = gzdLLziPurezidispatch1[20];
  zdLLziMainziloop3  zdLLziMainziloop3 (gzdLLziMainziloop3[0], callResR10);
  assign gzdLLziPurezidispatch = {__in0, __resumption_tag};
  assign gzdLLziMainziloop3R1 = gzdLLziPurezidispatch[20];
  zdLLziMainziloop3  zdLLziMainziloop3R1 (gzdLLziMainziloop3R1[0], callResR11);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLziPurezidispatch[19:16] == 4'h1) ? callResR11 : ((gzdLLziPurezidispatch1[19:16] == 4'h2) ? callResR10 : ((gzdLLziPurezidispatch2[19:16] == 4'h3) ? callResR9 : ((gzdLLziPurezidispatch3[19:16] == 4'h4) ? callResR8 : ((gzdLLziPurezidispatch4[19:16] == 4'h5) ? callResR7 : ((gzdLLziPurezidispatch5[19:16] == 4'h6) ? callResR6 : ((gzdLLziPurezidispatch6[19:16] == 4'h7) ? callResR5 : ((gzdLLziPurezidispatch7[19:16] == 4'h8) ? callResR4 : ((gzdLLziPurezidispatch8[19:16] == 4'h9) ? callResR3 : ((gzdLLziPurezidispatch9[19:16] == 4'ha) ? callResR2 : ((gzdLLziPurezidispatch10[19:16] == 4'hb) ? ((litR2[0] == 1'h1) ? 29'h105d0000 : 29'h108c0000) : ((gzdLLziPurezidispatch11[19:16] == 4'hc) ? ((litR1[0] == 1'h1) ? 29'h10800d08 : 29'h10d0150d) : ((gzdLLziPurezidispatch12[19:16] == 4'hd) ? ((lit[0] == 1'h1) ? 29'h10500805 : 29'h10800d08) : ((gzdLLziMainziloop20[0] == 1'h1) ? callResR1 : callRes)))))))))))));
  initial __resumption_tag <= 20'h10000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 20'h10000;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLziMainziloop3 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10020000 : 29'h10130000;
endmodule

module zdLLziMainziloop5 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10140000 : 29'h10150000;
endmodule

module zdLLziMainziloop7 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10170000 : 29'h10260000;
endmodule

module zdLLziMainziloop8 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10280000 : 29'h10390000;
endmodule

module zdLLziMainziloop11 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h103a0000 : 29'h105b0000;
endmodule

module Mainziloop (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [28:0] res);
  assign res = {1'h1, arg0, 4'h0, arg1, arg0};
endmodule