module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [17:0] gzdLLziPurezidispatch2;
  logic [15:0] gzdLLziMainziloop1;
  logic [18:0] gzdLLziMainziloop8;
  logic [18:0] gzdLLziMainziloop;
  logic [18:0] callRes;
  logic [17:0] gzdLLziPurezidispatch1;
  logic [15:0] gzdLLziMainzireset1;
  logic [7:0] gzdLLziMainzireset6;
  logic [18:0] gzdLLziMainzireset5;
  logic [18:0] gzdLLziMainziloopR1;
  logic [18:0] callResR1;
  logic [17:0] gzdLLziPurezidispatch;
  logic [15:0] gzdLLziMainziloop3;
  logic [18:0] gzdLLziMainziloop10;
  logic [18:0] gzdLLziMainziloopR2;
  logic [18:0] callResR2;
  logic [0:0] __continue;
  logic [1:0] __resumption_tag;
  logic [7:0] __st0;
  logic [1:0] __resumption_tag_next;
  logic [7:0] __st0_next;
  assign gzdLLziPurezidispatch2 = {__in0, {__resumption_tag, __st0}};
  assign gzdLLziMainziloop1 = {gzdLLziPurezidispatch2[17:10], gzdLLziPurezidispatch2[7:0]};
  assign gzdLLziMainziloop8 = {11'h200, gzdLLziMainziloop1[7:0]};
  assign gzdLLziMainziloop = gzdLLziMainziloop8[18:0];
  zdLLziMainziloop  zdLLziMainziloop (gzdLLziMainziloop[7:0], callRes);
  assign gzdLLziPurezidispatch1 = {__in0, {__resumption_tag, __st0}};
  assign gzdLLziMainzireset1 = {gzdLLziPurezidispatch1[17:10], gzdLLziPurezidispatch1[7:0]};
  assign gzdLLziMainzireset6 = gzdLLziMainzireset1[15:8];
  assign gzdLLziMainzireset5 = {11'h200, gzdLLziMainzireset6[7:0]};
  assign gzdLLziMainziloopR1 = gzdLLziMainzireset5[18:0];
  zdLLziMainziloop  zdLLziMainziloopR1 (gzdLLziMainziloopR1[7:0], callResR1);
  assign gzdLLziPurezidispatch = {__in0, {__resumption_tag, __st0}};
  assign gzdLLziMainziloop3 = {gzdLLziPurezidispatch[17:10], gzdLLziPurezidispatch[7:0]};
  assign gzdLLziMainziloop10 = {11'h200, gzdLLziMainziloop3[7:0]};
  assign gzdLLziMainziloopR2 = gzdLLziMainziloop10[18:0];
  zdLLziMainziloop  zdLLziMainziloopR2 (gzdLLziMainziloopR2[7:0], callResR2);
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = (gzdLLziPurezidispatch[9:8] == 2'h1) ? callResR2 : ((gzdLLziPurezidispatch1[9:8] == 2'h2) ? callResR1 : callRes);
  initial {__resumption_tag, __st0} <= 10'h200;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 10'h200;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module zdLLziMainziloop (input logic [7:0] arg0,
  output logic [18:0] res);
  logic [7:0] gMainziloop;
  logic [15:0] gzdLLziMainziloop17;
  logic [15:0] gzdLLziMainziloop15;
  logic [18:0] gzdLLziMainziloop14;
  logic [18:0] gzdLLziMainziloop13;
  logic [15:0] gzdLLziMainziloop4;
  logic [7:0] resizze;
  logic [0:0] msbit;
  logic [0:0] gReWireziPreludezinot;
  logic [0:0] callRes;
  logic [7:0] resizzeR1;
  logic [0:0] msbitR1;
  logic [0:0] gReWireziPreludezinotR1;
  logic [0:0] callResR1;
  logic [8:0] gzdLLziMainziloop12;
  logic [7:0] gzdLLziMainziloop6;
  logic [8:0] gzdLLziMainziloop11;
  logic [7:0] gzdLLziMainziloop5;
  assign gMainziloop = arg0;
  assign gzdLLziMainziloop17 = {gMainziloop[7:0], gMainziloop[7:0]};
  assign gzdLLziMainziloop15 = gzdLLziMainziloop17[15:0];
  assign gzdLLziMainziloop14 = {3'h0, gzdLLziMainziloop15[15:8], gzdLLziMainziloop15[7:0]};
  assign gzdLLziMainziloop13 = gzdLLziMainziloop14[18:0];
  assign gzdLLziMainziloop4 = {gzdLLziMainziloop13[15:8], gzdLLziMainziloop13[7:0]};
  assign resizze = gzdLLziMainziloop4[15:8];
  assign msbit = resizze[0];
  assign gReWireziPreludezinot = msbit[0];
  ReWireziPreludezinot  ReWireziPreludezinot (gReWireziPreludezinot[0], callRes);
  assign resizzeR1 = gzdLLziMainziloop4[15:8];
  assign msbitR1 = resizzeR1[0];
  assign gReWireziPreludezinotR1 = msbitR1[0];
  ReWireziPreludezinot  ReWireziPreludezinotR1 (gReWireziPreludezinotR1[0], callResR1);
  assign gzdLLziMainziloop12 = {gzdLLziMainziloop4[7:0], callResR1};
  assign gzdLLziMainziloop6 = gzdLLziMainziloop12[8:1];
  assign gzdLLziMainziloop11 = {gzdLLziMainziloop4[7:0], callRes};
  assign gzdLLziMainziloop5 = gzdLLziMainziloop11[8:1];
  assign res = (gzdLLziMainziloop11[0] == 1'h1) ? {11'h408, gzdLLziMainziloop5[7:0]} : {11'h409, gzdLLziMainziloop6[7:0]};
endmodule

module ReWireziPreludezinot (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 1'h0 : 1'h1;
endmodule