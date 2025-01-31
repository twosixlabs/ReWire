module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] gzdLLzicase4866;
  logic [16:0] gzdLLzilambda4762;
  logic [16:0] gzdLLzicase4923;
  logic [15:0] gMainziincr;
  logic [23:0] gzdLLzilambda4950;
  logic [23:0] gzdLLzicase4918;
  logic [25:0] callRes;
  logic [25:0] gzdLLzilambda4944;
  logic [25:0] gzdLLzicase4942;
  logic [23:0] gzdLLzilambda4760;
  logic [23:0] gzdLLzilambda4920;
  logic [23:0] gzdLLzicase4918R1;
  logic [25:0] callResR1;
  logic [33:0] gzdLLzilambda4914;
  logic [33:0] gzdLLzicase4911;
  logic [31:0] gzdLLzilambda4758;
  logic [15:0] gzdLLzilambda4906;
  logic [15:0] gzdLLzicase4876;
  logic [25:0] callResR2;
  logic [41:0] gzdLLzilambda4900;
  logic [41:0] gzdLLzicase4896;
  logic [31:0] gzdLLzilambda4755;
  logic [15:0] binOp;
  logic [15:0] gzdLLzilambda4890;
  logic [15:0] gzdLLzicase4876R1;
  logic [25:0] callResR3;
  logic [25:0] gzdLLzilambda4884;
  logic [25:0] gzdLLzicase4870;
  logic [25:0] callResR4;
  logic [16:0] gzdLLzicase4870R1;
  logic [25:0] callResR5;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st1;
  logic [7:0] __st0_next;
  logic [7:0] __st1_next;
  assign gzdLLzicase4866 = {{__st0, __st1}, __in0};
  assign gzdLLzilambda4762 = {gzdLLzicase4866[0], gzdLLzicase4866[16:9], gzdLLzicase4866[8:1]};
  assign gzdLLzicase4923 = {gzdLLzilambda4762[16], gzdLLzilambda4762[15:8], gzdLLzilambda4762[7:0]};
  assign gMainziincr = {gzdLLzicase4923[15:8], gzdLLzicase4923[7:0]};
  assign gzdLLzilambda4950 = {gMainziincr[15:8], gMainziincr[15:8], gMainziincr[7:0]};
  assign gzdLLzicase4918 = gzdLLzilambda4950[23:0];
  zdLLzicase4918  zdLLzicase4918 (gzdLLzicase4918[23:16], gzdLLzicase4918[15:8], gzdLLzicase4918[7:0], callRes);
  assign gzdLLzilambda4944 = callRes;
  assign gzdLLzicase4942 = gzdLLzilambda4944[25:0];
  assign gzdLLzilambda4760 = {gzdLLzicase4942[23:16], gzdLLzicase4942[15:8], gzdLLzicase4942[7:0]};
  assign gzdLLzilambda4920 = {gzdLLzilambda4760[7:0], gzdLLzilambda4760[15:8], gzdLLzilambda4760[7:0]};
  assign gzdLLzicase4918R1 = gzdLLzilambda4920[23:0];
  zdLLzicase4918  zdLLzicase4918R1 (gzdLLzicase4918R1[23:16], gzdLLzicase4918R1[15:8], gzdLLzicase4918R1[7:0], callResR1);
  assign gzdLLzilambda4914 = {gzdLLzilambda4760[23:16], callResR1};
  assign gzdLLzicase4911 = {gzdLLzilambda4914[25:0], gzdLLzilambda4914[33:26]};
  assign gzdLLzilambda4758 = {gzdLLzicase4911[7:0], gzdLLzicase4911[31:24], gzdLLzicase4911[23:16], gzdLLzicase4911[15:8]};
  assign gzdLLzilambda4906 = {gzdLLzilambda4758[23:16], gzdLLzilambda4758[7:0]};
  assign gzdLLzicase4876 = gzdLLzilambda4906[15:0];
  zdLLzicase4876  zdLLzicase4876 (gzdLLzicase4876[15:8], gzdLLzicase4876[7:0], callResR2);
  assign gzdLLzilambda4900 = {gzdLLzilambda4758[31:24], gzdLLzilambda4758[23:16], callResR2};
  assign gzdLLzicase4896 = {gzdLLzilambda4900[25:0], gzdLLzilambda4900[41:34], gzdLLzilambda4900[33:26]};
  assign gzdLLzilambda4755 = {gzdLLzicase4896[15:8], gzdLLzicase4896[7:0], gzdLLzicase4896[31:24], gzdLLzicase4896[23:16]};
  assign binOp = {gzdLLzilambda4755[31:24], gzdLLzilambda4755[23:16]};
  assign gzdLLzilambda4890 = {gzdLLzilambda4755[15:8], binOp[15:8] + binOp[7:0]};
  assign gzdLLzicase4876R1 = gzdLLzilambda4890[15:0];
  zdLLzicase4876  zdLLzicase4876R1 (gzdLLzicase4876R1[15:8], gzdLLzicase4876R1[7:0], callResR3);
  assign gzdLLzilambda4884 = callResR3;
  assign gzdLLzicase4870 = gzdLLzilambda4884[25:0];
  zdLLzicase4870  zdLLzicase4870 (gzdLLzicase4870[15:8], gzdLLzicase4870[7:0], callResR4);
  assign gzdLLzicase4870R1 = {gzdLLzilambda4762[16], gzdLLzilambda4762[15:8], gzdLLzilambda4762[7:0]};
  zdLLzicase4870  zdLLzicase4870R1 (gzdLLzicase4870R1[15:8], gzdLLzicase4870R1[7:0], callResR5);
  assign {__continue, __padding, __out0, __st0_next, __st1_next} = (gzdLLzicase4870R1[16] == 1'h1) ? callResR5 : callResR4;
  initial {__st0, __st1} <= 16'h0001;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 16'h0001;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module zdLLzicase4870 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  logic [15:0] gMainzisig;
  logic [23:0] gzdLLzilambda4962;
  logic [23:0] gzdLLzicase4918;
  logic [25:0] callRes;
  logic [25:0] gzdLLzilambda4956;
  logic [25:0] gzdLLzicase4954;
  logic [23:0] gzdLLzilambda4764;
  assign gMainzisig = {arg0, arg1};
  assign gzdLLzilambda4962 = {gMainzisig[15:8], gMainzisig[15:8], gMainzisig[7:0]};
  assign gzdLLzicase4918 = gzdLLzilambda4962[23:0];
  zdLLzicase4918  zdLLzicase4918 (gzdLLzicase4918[23:16], gzdLLzicase4918[15:8], gzdLLzicase4918[7:0], callRes);
  assign gzdLLzilambda4956 = callRes;
  assign gzdLLzicase4954 = gzdLLzilambda4956[25:0];
  assign gzdLLzilambda4764 = {gzdLLzicase4954[23:16], gzdLLzicase4954[15:8], gzdLLzicase4954[7:0]};
  assign res = {2'h2, gzdLLzilambda4764[23:16], gzdLLzilambda4764[15:8], gzdLLzilambda4764[7:0]};
endmodule

module zdLLzicase4876 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  assign res = {10'h100, arg0, arg1};
endmodule

module zdLLzicase4918 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  output logic [25:0] res);
  assign res = {2'h0, arg0, arg1, arg2};
endmodule