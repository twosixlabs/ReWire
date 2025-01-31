module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] gzdLLzicase8221;
  logic [8:0] gzdLLzilambda8169;
  logic [8:0] gzdLLzicase8258;
  logic [7:0] gzdLLzicase8164;
  logic [15:0] gzdLLzilambda8231;
  logic [15:0] gzdLLzicase8229;
  logic [17:0] callRes;
  logic [17:0] gzdLLzilambda8226;
  logic [17:0] gzdLLzicase8224;
  logic [15:0] gzdLLzilambda8163;
  logic [7:0] gMainziincW8;
  logic [15:0] binOp;
  logic [7:0] gzdLLzicase8244;
  logic [17:0] callResR1;
  logic [8:0] gzdLLzicase8260;
  logic [7:0] gzdLLzicase8165;
  logic [15:0] gzdLLzilambda8241;
  logic [15:0] gzdLLzicase8229R1;
  logic [17:0] callResR2;
  logic [17:0] gzdLLzilambda8236;
  logic [17:0] gzdLLzicase8234;
  logic [15:0] gzdLLzilambda8161;
  logic [7:0] gMainzirolW8;
  logic [15:0] binOpR1;
  logic [7:0] gMainzimsbitW8;
  logic [7:0] msbit;
  logic [0:0] resizze;
  logic [15:0] binOpR2;
  logic [7:0] gzdLLzicase8244R1;
  logic [17:0] callResR3;
  logic [17:0] gzdLLzilambda8256;
  logic [17:0] gzdLLzicase8254;
  logic [7:0] gMainzigo;
  logic [15:0] gzdLLzilambda8270;
  logic [15:0] gzdLLzicase8229R2;
  logic [17:0] callResR4;
  logic [17:0] gzdLLzilambda8265;
  logic [17:0] gzdLLzicase8263;
  logic [15:0] gzdLLzilambda8171;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign gzdLLzicase8221 = {__st0, __in0};
  assign gzdLLzilambda8169 = {gzdLLzicase8221[0], gzdLLzicase8221[8:1]};
  assign gzdLLzicase8258 = {gzdLLzilambda8169[8], gzdLLzilambda8169[7:0]};
  assign gzdLLzicase8164 = gzdLLzicase8258[7:0];
  assign gzdLLzilambda8231 = {gzdLLzicase8164[7:0], gzdLLzicase8164[7:0]};
  assign gzdLLzicase8229 = gzdLLzilambda8231[15:0];
  zdLLzicase8229  zdLLzicase8229 (gzdLLzicase8229[15:8], gzdLLzicase8229[7:0], callRes);
  assign gzdLLzilambda8226 = callRes;
  assign gzdLLzicase8224 = gzdLLzilambda8226[17:0];
  assign gzdLLzilambda8163 = {gzdLLzicase8224[15:8], gzdLLzicase8224[7:0]};
  assign gMainziincW8 = gzdLLzilambda8163[15:8];
  assign binOp = {gMainziincW8[7:0], 8'h01};
  assign gzdLLzicase8244 = binOp[15:8] + binOp[7:0];
  zdLLzicase8244  zdLLzicase8244 (gzdLLzicase8244[7:0], callResR1);
  assign gzdLLzicase8260 = {gzdLLzilambda8169[8], gzdLLzilambda8169[7:0]};
  assign gzdLLzicase8165 = gzdLLzicase8260[7:0];
  assign gzdLLzilambda8241 = {gzdLLzicase8165[7:0], gzdLLzicase8165[7:0]};
  assign gzdLLzicase8229R1 = gzdLLzilambda8241[15:0];
  zdLLzicase8229  zdLLzicase8229R1 (gzdLLzicase8229R1[15:8], gzdLLzicase8229R1[7:0], callResR2);
  assign gzdLLzilambda8236 = callResR2;
  assign gzdLLzicase8234 = gzdLLzilambda8236[17:0];
  assign gzdLLzilambda8161 = {gzdLLzicase8234[15:8], gzdLLzicase8234[7:0]};
  assign gMainzirolW8 = gzdLLzilambda8161[15:8];
  assign binOpR1 = {gMainzirolW8[7:0], 8'h01};
  assign gMainzimsbitW8 = gMainzirolW8[7:0];
  assign msbit = gMainzimsbitW8[7:0];
  assign resizze = msbit[7];
  assign binOpR2 = {binOpR1[15:8] << binOpR1[7:0], 8'(resizze[0])};
  assign gzdLLzicase8244R1 = binOpR2[15:8] | binOpR2[7:0];
  zdLLzicase8244  zdLLzicase8244R1 (gzdLLzicase8244R1[7:0], callResR3);
  assign gzdLLzilambda8256 = (gzdLLzicase8260[8] == 1'h1) ? callResR3 : callResR1;
  assign gzdLLzicase8254 = gzdLLzilambda8256[17:0];
  assign gMainzigo = gzdLLzicase8254[7:0];
  assign gzdLLzilambda8270 = {gMainzigo[7:0], gMainzigo[7:0]};
  assign gzdLLzicase8229R2 = gzdLLzilambda8270[15:0];
  zdLLzicase8229  zdLLzicase8229R2 (gzdLLzicase8229R2[15:8], gzdLLzicase8229R2[7:0], callResR4);
  assign gzdLLzilambda8265 = callResR4;
  assign gzdLLzicase8263 = gzdLLzilambda8265[17:0];
  assign gzdLLzilambda8171 = {gzdLLzicase8263[15:8], gzdLLzicase8263[7:0]};
  assign {__continue, __padding, __out0, __st0_next} = {2'h2, gzdLLzilambda8171[15:8], gzdLLzilambda8171[7:0]};
  initial __st0 <= 8'h00;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h00;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module zdLLzicase8229 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [17:0] res);
  assign res = {2'h0, arg0, arg1};
endmodule

module zdLLzicase8244 (input logic [7:0] arg0,
  output logic [17:0] res);
  assign res = {10'h100, arg0};
endmodule