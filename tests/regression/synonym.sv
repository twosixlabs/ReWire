module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] gzdLLzicase4818;
  logic [16:0] gzdLLzilambda4714;
  logic [16:0] gzdLLzicase4822;
  logic [25:0] callRes;
  logic [16:0] gzdLLzicase4878;
  logic [15:0] gMainziincr;
  logic [23:0] gzdLLzilambda4902;
  logic [23:0] gzdLLzicase4870;
  logic [25:0] callResR1;
  logic [25:0] gzdLLzilambda4896;
  logic [25:0] gzdLLzicase4894;
  logic [23:0] gzdLLzilambda4712;
  logic [23:0] gzdLLzilambda4872;
  logic [23:0] gzdLLzicase4870R1;
  logic [25:0] callResR2;
  logic [33:0] gzdLLzilambda4866;
  logic [33:0] gzdLLzicase4863;
  logic [31:0] gzdLLzilambda4710;
  logic [15:0] gzdLLzilambda4858;
  logic [15:0] gzdLLzicase4828;
  logic [25:0] callResR3;
  logic [41:0] gzdLLzilambda4852;
  logic [41:0] gzdLLzicase4848;
  logic [31:0] gzdLLzilambda4707;
  logic [15:0] binOp;
  logic [15:0] gzdLLzilambda4842;
  logic [15:0] gzdLLzicase4828R1;
  logic [25:0] callResR4;
  logic [25:0] gzdLLzilambda4836;
  logic [25:0] gzdLLzicase4822R1;
  logic [25:0] callResR5;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st1;
  logic [7:0] __st0_next;
  logic [7:0] __st1_next;
  assign gzdLLzicase4818 = {{__st0, __st1}, __in0};
  assign gzdLLzilambda4714 = {gzdLLzicase4818[0], gzdLLzicase4818[16:9], gzdLLzicase4818[8:1]};
  assign gzdLLzicase4822 = {gzdLLzilambda4714[16], gzdLLzilambda4714[15:8], gzdLLzilambda4714[7:0]};
  zdLLzicase4822  zdLLzicase4822 (gzdLLzicase4822[15:8], gzdLLzicase4822[7:0], callRes);
  assign gzdLLzicase4878 = {gzdLLzilambda4714[16], gzdLLzilambda4714[15:8], gzdLLzilambda4714[7:0]};
  assign gMainziincr = {gzdLLzicase4878[15:8], gzdLLzicase4878[7:0]};
  assign gzdLLzilambda4902 = {gMainziincr[15:8], gMainziincr[15:8], gMainziincr[7:0]};
  assign gzdLLzicase4870 = gzdLLzilambda4902[23:0];
  zdLLzicase4870  zdLLzicase4870 (gzdLLzicase4870[23:16], gzdLLzicase4870[15:8], gzdLLzicase4870[7:0], callResR1);
  assign gzdLLzilambda4896 = callResR1;
  assign gzdLLzicase4894 = gzdLLzilambda4896[25:0];
  assign gzdLLzilambda4712 = {gzdLLzicase4894[23:16], gzdLLzicase4894[15:8], gzdLLzicase4894[7:0]};
  assign gzdLLzilambda4872 = {gzdLLzilambda4712[7:0], gzdLLzilambda4712[15:8], gzdLLzilambda4712[7:0]};
  assign gzdLLzicase4870R1 = gzdLLzilambda4872[23:0];
  zdLLzicase4870  zdLLzicase4870R1 (gzdLLzicase4870R1[23:16], gzdLLzicase4870R1[15:8], gzdLLzicase4870R1[7:0], callResR2);
  assign gzdLLzilambda4866 = {gzdLLzilambda4712[23:16], callResR2};
  assign gzdLLzicase4863 = {gzdLLzilambda4866[25:0], gzdLLzilambda4866[33:26]};
  assign gzdLLzilambda4710 = {gzdLLzicase4863[7:0], gzdLLzicase4863[31:24], gzdLLzicase4863[23:16], gzdLLzicase4863[15:8]};
  assign gzdLLzilambda4858 = {gzdLLzilambda4710[23:16], gzdLLzilambda4710[7:0]};
  assign gzdLLzicase4828 = gzdLLzilambda4858[15:0];
  zdLLzicase4828  zdLLzicase4828 (gzdLLzicase4828[15:8], gzdLLzicase4828[7:0], callResR3);
  assign gzdLLzilambda4852 = {gzdLLzilambda4710[31:24], gzdLLzilambda4710[23:16], callResR3};
  assign gzdLLzicase4848 = {gzdLLzilambda4852[25:0], gzdLLzilambda4852[41:34], gzdLLzilambda4852[33:26]};
  assign gzdLLzilambda4707 = {gzdLLzicase4848[15:8], gzdLLzicase4848[7:0], gzdLLzicase4848[31:24], gzdLLzicase4848[23:16]};
  assign binOp = {gzdLLzilambda4707[31:24], gzdLLzilambda4707[23:16]};
  assign gzdLLzilambda4842 = {gzdLLzilambda4707[15:8], binOp[15:8] + binOp[7:0]};
  assign gzdLLzicase4828R1 = gzdLLzilambda4842[15:0];
  zdLLzicase4828  zdLLzicase4828R1 (gzdLLzicase4828R1[15:8], gzdLLzicase4828R1[7:0], callResR4);
  assign gzdLLzilambda4836 = callResR4;
  assign gzdLLzicase4822R1 = gzdLLzilambda4836[25:0];
  zdLLzicase4822  zdLLzicase4822R1 (gzdLLzicase4822R1[15:8], gzdLLzicase4822R1[7:0], callResR5);
  assign {__continue, __padding, __out0, __st0_next, __st1_next} = (gzdLLzicase4878[16] == 1'h1) ? callResR5 : callRes;
  initial {__st0, __st1} <= 16'h0001;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 16'h0001;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module zdLLzicase4822 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  logic [15:0] gMainzisig;
  logic [23:0] gzdLLzilambda4914;
  logic [23:0] gzdLLzicase4870;
  logic [25:0] callRes;
  logic [25:0] gzdLLzilambda4908;
  logic [25:0] gzdLLzicase4906;
  logic [23:0] gzdLLzilambda4716;
  assign gMainzisig = {arg0, arg1};
  assign gzdLLzilambda4914 = {gMainzisig[15:8], gMainzisig[15:8], gMainzisig[7:0]};
  assign gzdLLzicase4870 = gzdLLzilambda4914[23:0];
  zdLLzicase4870  zdLLzicase4870 (gzdLLzicase4870[23:16], gzdLLzicase4870[15:8], gzdLLzicase4870[7:0], callRes);
  assign gzdLLzilambda4908 = callRes;
  assign gzdLLzicase4906 = gzdLLzilambda4908[25:0];
  assign gzdLLzilambda4716 = {gzdLLzicase4906[23:16], gzdLLzicase4906[15:8], gzdLLzicase4906[7:0]};
  assign res = {2'h2, gzdLLzilambda4716[23:16], gzdLLzilambda4716[15:8], gzdLLzilambda4716[7:0]};
endmodule

module zdLLzicase4828 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  assign res = {10'h100, arg0, arg1};
endmodule

module zdLLzicase4870 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  output logic [25:0] res);
  assign res = {2'h0, arg0, arg1, arg2};
endmodule