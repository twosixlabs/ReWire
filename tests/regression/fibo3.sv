module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] gzdLLziMainzisig;
  logic [16:0] gzdLLziMainzisig3;
  logic [15:0] gMainziincr;
  logic [15:0] gMainzifirst;
  logic [23:0] callRes;
  logic [23:0] gzdLLziMainziincr18;
  logic [23:0] gzdLLziMainziincr11;
  logic [25:0] callResR1;
  logic [25:0] gzdLLziMainziincr15;
  logic [25:0] gzdLLziMainziincr14;
  logic [23:0] gzdLLziMainziincr2;
  logic [15:0] gMainzisecond;
  logic [31:0] gzdLLziMainzisecond2;
  logic [31:0] gzdLLziMainzisecond;
  logic [15:0] resizze;
  logic [255:0] binOp;
  logic [127:0] resizzeR1;
  logic [23:0] gzdLLziMainziincr13;
  logic [23:0] gzdLLziMainziincr11R1;
  logic [25:0] callResR2;
  logic [33:0] gzdLLziMainziincr10;
  logic [33:0] gzdLLziMainziincr8;
  logic [31:0] gzdLLziMainziincr1;
  logic [15:0] binOpR1;
  logic [15:0] gzdLLziMainzibegin3;
  logic [25:0] gzdLLziMainziincr4;
  logic [25:0] gzdLLziMainzibegin;
  logic [25:0] callResR3;
  logic [16:0] gzdLLziMainzibeginR1;
  logic [25:0] callResR4;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [15:0] __st0;
  logic [15:0] __st0_next;
  assign gzdLLziMainzisig = {__in0, __st0};
  assign gzdLLziMainzisig3 = {gzdLLziMainzisig[15:0], gzdLLziMainzisig[16]};
  assign gMainziincr = gzdLLziMainzisig3[16:1];
  assign gMainzifirst = gMainziincr[15:0];
  Mainzifirst  Mainzifirst (gMainzifirst[15:0], callRes);
  assign gzdLLziMainziincr18 = callRes;
  assign gzdLLziMainziincr11 = gzdLLziMainziincr18[23:0];
  zdLLziMainziincr11  zdLLziMainziincr11 (gzdLLziMainziincr11[23:16], gzdLLziMainziincr11[15:0], callResR1);
  assign gzdLLziMainziincr15 = callResR1;
  assign gzdLLziMainziincr14 = gzdLLziMainziincr15[25:0];
  assign gzdLLziMainziincr2 = {gzdLLziMainziincr14[23:16], gzdLLziMainziincr14[15:0]};
  assign gMainzisecond = gzdLLziMainziincr2[15:0];
  assign gzdLLziMainzisecond2 = {gMainzisecond[15:0], gMainzisecond[15:0]};
  assign gzdLLziMainzisecond = gzdLLziMainzisecond2[31:0];
  assign resizze = gzdLLziMainzisecond[31:16];
  assign binOp = {128'(resizze[15:0]), {8'h80{1'h0}}};
  assign resizzeR1 = binOp[255:128] >> binOp[127:0];
  assign gzdLLziMainziincr13 = {resizzeR1[7:0], gzdLLziMainzisecond[15:0]};
  assign gzdLLziMainziincr11R1 = gzdLLziMainziincr13[23:0];
  zdLLziMainziincr11  zdLLziMainziincr11R1 (gzdLLziMainziincr11R1[23:16], gzdLLziMainziincr11R1[15:0], callResR2);
  assign gzdLLziMainziincr10 = {gzdLLziMainziincr2[23:16], callResR2};
  assign gzdLLziMainziincr8 = {gzdLLziMainziincr10[33:26], gzdLLziMainziincr10[25:0]};
  assign gzdLLziMainziincr1 = {gzdLLziMainziincr8[33:26], gzdLLziMainziincr8[23:16], gzdLLziMainziincr8[15:0]};
  assign binOpR1 = {gzdLLziMainziincr1[31:24], gzdLLziMainziincr1[23:16]};
  assign gzdLLziMainzibegin3 = {gzdLLziMainziincr1[23:16], binOpR1[15:8] + binOpR1[7:0]};
  assign gzdLLziMainziincr4 = {10'h100, gzdLLziMainzibegin3[15:0]};
  assign gzdLLziMainzibegin = gzdLLziMainziincr4[25:0];
  zdLLziMainzibegin  zdLLziMainzibegin (gzdLLziMainzibegin[15:0], callResR3);
  assign gzdLLziMainzibeginR1 = {gzdLLziMainzisig[15:0], gzdLLziMainzisig[16]};
  zdLLziMainzibegin  zdLLziMainzibeginR1 (gzdLLziMainzibeginR1[16:1], callResR4);
  assign {__continue, __padding, __out0, __st0_next} = (gzdLLziMainzibeginR1[0] == 1'h1) ? callResR4 : callResR3;
  initial __st0 <= 16'h0001;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 16'h0001;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module zdLLziMainzibegin (input logic [15:0] arg0,
  output logic [25:0] res);
  logic [15:0] gMainzisig;
  logic [15:0] gMainzifirst;
  logic [23:0] callRes;
  logic [23:0] gzdLLziMainzisig8;
  logic [23:0] gzdLLziMainziincr11;
  logic [25:0] callResR1;
  logic [25:0] gzdLLziMainzisig5;
  logic [25:0] gzdLLziMainzisig4;
  logic [23:0] gzdLLziMainzisig1;
  assign gMainzisig = arg0;
  assign gMainzifirst = gMainzisig[15:0];
  Mainzifirst  Mainzifirst (gMainzifirst[15:0], callRes);
  assign gzdLLziMainzisig8 = callRes;
  assign gzdLLziMainziincr11 = gzdLLziMainzisig8[23:0];
  zdLLziMainziincr11  zdLLziMainziincr11 (gzdLLziMainziincr11[23:16], gzdLLziMainziincr11[15:0], callResR1);
  assign gzdLLziMainzisig5 = callResR1;
  assign gzdLLziMainzisig4 = gzdLLziMainzisig5[25:0];
  assign gzdLLziMainzisig1 = {gzdLLziMainzisig4[23:16], gzdLLziMainzisig4[15:0]};
  assign res = {2'h2, gzdLLziMainzisig1[23:16], gzdLLziMainzisig1[15:0]};
endmodule

module zdLLziMainziincr11 (input logic [7:0] arg0,
  input logic [15:0] arg1,
  output logic [25:0] res);
  assign res = {2'h0, arg0, arg1};
endmodule

module Mainzifirst (input logic [15:0] arg0,
  output logic [23:0] res);
  logic [31:0] gzdLLziMainzifirst2;
  logic [31:0] gzdLLziMainzifirst;
  logic [15:0] id;
  assign gzdLLziMainzifirst2 = {arg0, arg0};
  assign gzdLLziMainzifirst = gzdLLziMainzifirst2[31:0];
  assign id = gzdLLziMainzifirst[31:16];
  assign res = {id[15:8], gzdLLziMainzifirst[15:0]};
endmodule