module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] gzdLLziMainzigo4;
  logic [8:0] gzdLLziMainzigo19;
  logic [7:0] gzdLLziMainzigo6;
  logic [15:0] gzdLLziMainzigo29;
  logic [15:0] gzdLLziMainzigo22;
  logic [17:0] callRes;
  logic [17:0] gzdLLziMainzigo26;
  logic [17:0] gzdLLziMainzigo25;
  logic [15:0] gzdLLziMainzigo3;
  logic [7:0] gMainziincW8;
  logic [15:0] binOp;
  logic [7:0] gzdLLziMainzigo10;
  logic [17:0] callResR1;
  logic [17:0] gzdLLziMainzigo14;
  logic [17:0] gzdLLziMainzigo;
  logic [17:0] callResR2;
  logic [8:0] gzdLLziMainzigo18;
  logic [7:0] gzdLLziMainzigo5;
  logic [15:0] gzdLLziMainzigo24;
  logic [15:0] gzdLLziMainzigo22R1;
  logic [17:0] callResR3;
  logic [17:0] gzdLLziMainzigo21;
  logic [17:0] gzdLLziMainzigo20;
  logic [15:0] gzdLLziMainzigo1;
  logic [7:0] gMainzirolW8;
  logic [15:0] binOpR1;
  logic [7:0] gMainzimsbitW8;
  logic [7:0] msbit;
  logic [0:0] resizze;
  logic [15:0] binOpR2;
  logic [7:0] gzdLLziMainzigo10R1;
  logic [17:0] callResR4;
  logic [17:0] gzdLLziMainzigo9;
  logic [17:0] gzdLLziMainzigoR1;
  logic [17:0] callResR5;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign gzdLLziMainzigo4 = {__in0, __st0};
  assign gzdLLziMainzigo19 = {gzdLLziMainzigo4[7:0], gzdLLziMainzigo4[8]};
  assign gzdLLziMainzigo6 = gzdLLziMainzigo19[8:1];
  assign gzdLLziMainzigo29 = {gzdLLziMainzigo6[7:0], gzdLLziMainzigo6[7:0]};
  assign gzdLLziMainzigo22 = gzdLLziMainzigo29[15:0];
  zdLLziMainzigo22  zdLLziMainzigo22 (gzdLLziMainzigo22[15:8], gzdLLziMainzigo22[7:0], callRes);
  assign gzdLLziMainzigo26 = callRes;
  assign gzdLLziMainzigo25 = gzdLLziMainzigo26[17:0];
  assign gzdLLziMainzigo3 = {gzdLLziMainzigo25[15:8], gzdLLziMainzigo25[7:0]};
  assign gMainziincW8 = gzdLLziMainzigo3[15:8];
  assign binOp = {gMainziincW8[7:0], 8'h01};
  assign gzdLLziMainzigo10 = binOp[15:8] + binOp[7:0];
  zdLLziMainzigo10  zdLLziMainzigo10 (gzdLLziMainzigo10[7:0], callResR1);
  assign gzdLLziMainzigo14 = callResR1;
  assign gzdLLziMainzigo = gzdLLziMainzigo14[17:0];
  zdLLziMainzigo  zdLLziMainzigo (gzdLLziMainzigo[7:0], callResR2);
  assign gzdLLziMainzigo18 = {gzdLLziMainzigo4[7:0], gzdLLziMainzigo4[8]};
  assign gzdLLziMainzigo5 = gzdLLziMainzigo18[8:1];
  assign gzdLLziMainzigo24 = {gzdLLziMainzigo5[7:0], gzdLLziMainzigo5[7:0]};
  assign gzdLLziMainzigo22R1 = gzdLLziMainzigo24[15:0];
  zdLLziMainzigo22  zdLLziMainzigo22R1 (gzdLLziMainzigo22R1[15:8], gzdLLziMainzigo22R1[7:0], callResR3);
  assign gzdLLziMainzigo21 = callResR3;
  assign gzdLLziMainzigo20 = gzdLLziMainzigo21[17:0];
  assign gzdLLziMainzigo1 = {gzdLLziMainzigo20[15:8], gzdLLziMainzigo20[7:0]};
  assign gMainzirolW8 = gzdLLziMainzigo1[15:8];
  assign binOpR1 = {gMainzirolW8[7:0], 8'h01};
  assign gMainzimsbitW8 = gMainzirolW8[7:0];
  assign msbit = gMainzimsbitW8[7:0];
  assign resizze = msbit[7];
  assign binOpR2 = {binOpR1[15:8] << binOpR1[7:0], 8'(resizze[0])};
  assign gzdLLziMainzigo10R1 = binOpR2[15:8] | binOpR2[7:0];
  zdLLziMainzigo10  zdLLziMainzigo10R1 (gzdLLziMainzigo10R1[7:0], callResR4);
  assign gzdLLziMainzigo9 = callResR4;
  assign gzdLLziMainzigoR1 = gzdLLziMainzigo9[17:0];
  zdLLziMainzigo  zdLLziMainzigoR1 (gzdLLziMainzigoR1[7:0], callResR5);
  assign {__continue, __padding, __out0, __st0_next} = (gzdLLziMainzigo18[0] == 1'h1) ? callResR5 : callResR2;
  initial __st0 <= 8'h00;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h00;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module zdLLziMainzigo (input logic [7:0] arg0,
  output logic [17:0] res);
  logic [7:0] gMainzigo;
  logic [15:0] gzdLLziMainzigo34;
  logic [15:0] gzdLLziMainzigo22;
  logic [17:0] callRes;
  logic [17:0] gzdLLziMainzigo31;
  logic [17:0] gzdLLziMainzigo30;
  logic [15:0] gzdLLziMainzigo7;
  assign gMainzigo = arg0;
  assign gzdLLziMainzigo34 = {gMainzigo[7:0], gMainzigo[7:0]};
  assign gzdLLziMainzigo22 = gzdLLziMainzigo34[15:0];
  zdLLziMainzigo22  zdLLziMainzigo22 (gzdLLziMainzigo22[15:8], gzdLLziMainzigo22[7:0], callRes);
  assign gzdLLziMainzigo31 = callRes;
  assign gzdLLziMainzigo30 = gzdLLziMainzigo31[17:0];
  assign gzdLLziMainzigo7 = {gzdLLziMainzigo30[15:8], gzdLLziMainzigo30[7:0]};
  assign res = {2'h2, gzdLLziMainzigo7[15:8], gzdLLziMainzigo7[7:0]};
endmodule

module zdLLziMainzigo10 (input logic [7:0] arg0,
  output logic [17:0] res);
  assign res = {10'h100, arg0};
endmodule

module zdLLziMainzigo22 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [17:0] res);
  assign res = {2'h0, arg0, arg1};
endmodule