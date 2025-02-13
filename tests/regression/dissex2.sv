module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] gzdLLziMainzisig;
  logic [16:0] gzdLLziMainzibegin;
  logic [25:0] callRes;
  logic [16:0] gzdLLziMainzisig2;
  logic [15:0] gMainziincr;
  logic [23:0] gzdLLziMainziincr33;
  logic [23:0] gzdLLziMainziincr23;
  logic [25:0] callResR1;
  logic [25:0] gzdLLziMainziincr29;
  logic [25:0] gzdLLziMainziincr27;
  logic [23:0] gzdLLziMainziincr3;
  logic [23:0] gzdLLziMainziincr26;
  logic [23:0] gzdLLziMainziincr23R1;
  logic [25:0] callResR2;
  logic [33:0] gzdLLziMainziincr22;
  logic [33:0] gzdLLziMainziincr19;
  logic [31:0] gzdLLziMainziincr2;
  logic [15:0] gzdLLziMainziincr18;
  logic [15:0] gzdLLziMainzibegin5;
  logic [25:0] callResR3;
  logic [41:0] gzdLLziMainziincr14;
  logic [41:0] gzdLLziMainziincr11;
  logic [31:0] gzdLLziMainziincr1;
  logic [15:0] binOp;
  logic [15:0] gzdLLziMainziincr10;
  logic [15:0] gzdLLziMainzibegin5R1;
  logic [25:0] callResR4;
  logic [25:0] gzdLLziMainziincr6;
  logic [25:0] gzdLLziMainzibeginR1;
  logic [25:0] callResR5;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st1;
  logic [7:0] __st0_next;
  logic [7:0] __st1_next;
  assign gzdLLziMainzisig = {__in0, {__st0, __st1}};
  assign gzdLLziMainzibegin = {gzdLLziMainzisig[15:8], gzdLLziMainzisig[7:0], gzdLLziMainzisig[16]};
  zdLLziMainzibegin  zdLLziMainzibegin (gzdLLziMainzibegin[16:9], gzdLLziMainzibegin[8:1], callRes);
  assign gzdLLziMainzisig2 = {gzdLLziMainzisig[15:8], gzdLLziMainzisig[7:0], gzdLLziMainzisig[16]};
  assign gMainziincr = {gzdLLziMainzisig2[16:9], gzdLLziMainzisig2[8:1]};
  assign gzdLLziMainziincr33 = {gMainziincr[15:8], gMainziincr[15:8], gMainziincr[7:0]};
  assign gzdLLziMainziincr23 = gzdLLziMainziincr33[23:0];
  zdLLziMainziincr23  zdLLziMainziincr23 (gzdLLziMainziincr23[23:16], gzdLLziMainziincr23[15:8], gzdLLziMainziincr23[7:0], callResR1);
  assign gzdLLziMainziincr29 = callResR1;
  assign gzdLLziMainziincr27 = gzdLLziMainziincr29[25:0];
  assign gzdLLziMainziincr3 = {gzdLLziMainziincr27[23:16], gzdLLziMainziincr27[15:8], gzdLLziMainziincr27[7:0]};
  assign gzdLLziMainziincr26 = {gzdLLziMainziincr3[7:0], gzdLLziMainziincr3[15:8], gzdLLziMainziincr3[7:0]};
  assign gzdLLziMainziincr23R1 = gzdLLziMainziincr26[23:0];
  zdLLziMainziincr23  zdLLziMainziincr23R1 (gzdLLziMainziincr23R1[23:16], gzdLLziMainziincr23R1[15:8], gzdLLziMainziincr23R1[7:0], callResR2);
  assign gzdLLziMainziincr22 = {gzdLLziMainziincr3[23:16], callResR2};
  assign gzdLLziMainziincr19 = {gzdLLziMainziincr22[33:26], gzdLLziMainziincr22[25:0]};
  assign gzdLLziMainziincr2 = {gzdLLziMainziincr19[33:26], gzdLLziMainziincr19[23:16], gzdLLziMainziincr19[15:8], gzdLLziMainziincr19[7:0]};
  assign gzdLLziMainziincr18 = {gzdLLziMainziincr2[23:16], gzdLLziMainziincr2[7:0]};
  assign gzdLLziMainzibegin5 = gzdLLziMainziincr18[15:0];
  zdLLziMainzibegin5  zdLLziMainzibegin5 (gzdLLziMainzibegin5[15:8], gzdLLziMainzibegin5[7:0], callResR3);
  assign gzdLLziMainziincr14 = {gzdLLziMainziincr2[23:16], gzdLLziMainziincr2[31:24], callResR3};
  assign gzdLLziMainziincr11 = {gzdLLziMainziincr14[41:34], gzdLLziMainziincr14[33:26], gzdLLziMainziincr14[25:0]};
  assign gzdLLziMainziincr1 = {gzdLLziMainziincr11[41:34], gzdLLziMainziincr11[33:26], gzdLLziMainziincr11[15:8], gzdLLziMainziincr11[7:0]};
  assign binOp = {gzdLLziMainziincr1[23:16], gzdLLziMainziincr1[31:24]};
  assign gzdLLziMainziincr10 = {gzdLLziMainziincr1[15:8], binOp[15:8] + binOp[7:0]};
  assign gzdLLziMainzibegin5R1 = gzdLLziMainziincr10[15:0];
  zdLLziMainzibegin5  zdLLziMainzibegin5R1 (gzdLLziMainzibegin5R1[15:8], gzdLLziMainzibegin5R1[7:0], callResR4);
  assign gzdLLziMainziincr6 = callResR4;
  assign gzdLLziMainzibeginR1 = gzdLLziMainziincr6[25:0];
  zdLLziMainzibegin  zdLLziMainzibeginR1 (gzdLLziMainzibeginR1[15:8], gzdLLziMainzibeginR1[7:0], callResR5);
  assign {__continue, __padding, __out0, __st0_next, __st1_next} = (gzdLLziMainzisig2[0] == 1'h1) ? callResR5 : callRes;
  initial {__st0, __st1} <= 16'h0001;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 16'h0001;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module zdLLziMainzibegin (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  logic [15:0] gMainzisig;
  logic [23:0] gzdLLziMainzisig10;
  logic [23:0] gzdLLziMainziincr23;
  logic [25:0] callRes;
  logic [25:0] gzdLLziMainzisig6;
  logic [25:0] gzdLLziMainzisig4;
  logic [23:0] gzdLLziMainzisig1;
  assign gMainzisig = {arg0, arg1};
  assign gzdLLziMainzisig10 = {gMainzisig[15:8], gMainzisig[15:8], gMainzisig[7:0]};
  assign gzdLLziMainziincr23 = gzdLLziMainzisig10[23:0];
  zdLLziMainziincr23  zdLLziMainziincr23 (gzdLLziMainziincr23[23:16], gzdLLziMainziincr23[15:8], gzdLLziMainziincr23[7:0], callRes);
  assign gzdLLziMainzisig6 = callRes;
  assign gzdLLziMainzisig4 = gzdLLziMainzisig6[25:0];
  assign gzdLLziMainzisig1 = {gzdLLziMainzisig4[23:16], gzdLLziMainzisig4[15:8], gzdLLziMainzisig4[7:0]};
  assign res = {2'h2, gzdLLziMainzisig1[23:16], gzdLLziMainzisig1[15:8], gzdLLziMainzisig1[7:0]};
endmodule

module zdLLziMainzibegin5 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  assign res = {10'h100, arg0, arg1};
endmodule

module zdLLziMainziincr23 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  output logic [25:0] res);
  assign res = {2'h0, arg0, arg1, arg2};
endmodule