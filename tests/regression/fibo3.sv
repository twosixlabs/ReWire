module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] gzdLLzicase8523;
  logic [16:0] gzdLLzilambda8453;
  logic [16:0] gzdLLzicase8557;
  logic [15:0] gMainziincr;
  logic [15:0] gMainzifirst;
  logic [23:0] callRes;
  logic [23:0] gzdLLzilambda8579;
  logic [23:0] gzdLLzicase8553;
  logic [25:0] callResR1;
  logic [25:0] gzdLLzilambda8574;
  logic [25:0] gzdLLzicase8572;
  logic [23:0] gzdLLzilambda8449;
  logic [15:0] gMainzisecond;
  logic [31:0] gzdLLzilambda8533;
  logic [31:0] gzdLLzilambda8451;
  logic [15:0] resizze;
  logic [255:0] binOp;
  logic [127:0] resizzeR1;
  logic [23:0] gzdLLzilambda8555;
  logic [23:0] gzdLLzicase8553R1;
  logic [25:0] callResR2;
  logic [33:0] gzdLLzilambda8550;
  logic [33:0] gzdLLzicase8547;
  logic [31:0] gzdLLzilambda8447;
  logic [15:0] binOpR1;
  logic [15:0] gzdLLzicase8541;
  logic [25:0] gzdLLzilambda8538;
  logic [25:0] gzdLLzicase8536;
  logic [25:0] callResR3;
  logic [16:0] gzdLLzicase8536R1;
  logic [25:0] callResR4;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [15:0] __st0;
  logic [15:0] __st0_next;
  assign gzdLLzicase8523 = {__st0, __in0};
  assign gzdLLzilambda8453 = {gzdLLzicase8523[0], gzdLLzicase8523[16:1]};
  assign gzdLLzicase8557 = {gzdLLzilambda8453[16], gzdLLzilambda8453[15:0]};
  assign gMainziincr = gzdLLzicase8557[15:0];
  assign gMainzifirst = gMainziincr[15:0];
  Mainzifirst  Mainzifirst (gMainzifirst[15:0], callRes);
  assign gzdLLzilambda8579 = callRes;
  assign gzdLLzicase8553 = gzdLLzilambda8579[23:0];
  zdLLzicase8553  zdLLzicase8553 (gzdLLzicase8553[23:16], gzdLLzicase8553[15:0], callResR1);
  assign gzdLLzilambda8574 = callResR1;
  assign gzdLLzicase8572 = gzdLLzilambda8574[25:0];
  assign gzdLLzilambda8449 = {gzdLLzicase8572[23:16], gzdLLzicase8572[15:0]};
  assign gMainzisecond = gzdLLzilambda8449[15:0];
  assign gzdLLzilambda8533 = {gMainzisecond[15:0], gMainzisecond[15:0]};
  assign gzdLLzilambda8451 = gzdLLzilambda8533[31:0];
  assign resizze = gzdLLzilambda8451[31:16];
  assign binOp = {128'(resizze[15:0]), {8'h80{1'h0}}};
  assign resizzeR1 = binOp[255:128] >> binOp[127:0];
  assign gzdLLzilambda8555 = {resizzeR1[7:0], gzdLLzilambda8451[15:0]};
  assign gzdLLzicase8553R1 = gzdLLzilambda8555[23:0];
  zdLLzicase8553  zdLLzicase8553R1 (gzdLLzicase8553R1[23:16], gzdLLzicase8553R1[15:0], callResR2);
  assign gzdLLzilambda8550 = {gzdLLzilambda8449[23:16], callResR2};
  assign gzdLLzicase8547 = {gzdLLzilambda8550[25:0], gzdLLzilambda8550[33:26]};
  assign gzdLLzilambda8447 = {gzdLLzicase8547[7:0], gzdLLzicase8547[31:24], gzdLLzicase8547[23:8]};
  assign binOpR1 = {gzdLLzilambda8447[31:24], gzdLLzilambda8447[23:16]};
  assign gzdLLzicase8541 = {gzdLLzilambda8447[23:16], binOpR1[15:8] + binOpR1[7:0]};
  assign gzdLLzilambda8538 = {10'h100, gzdLLzicase8541[15:0]};
  assign gzdLLzicase8536 = gzdLLzilambda8538[25:0];
  zdLLzicase8536  zdLLzicase8536 (gzdLLzicase8536[15:0], callResR3);
  assign gzdLLzicase8536R1 = {gzdLLzilambda8453[16], gzdLLzilambda8453[15:0]};
  zdLLzicase8536  zdLLzicase8536R1 (gzdLLzicase8536R1[15:0], callResR4);
  assign {__continue, __padding, __out0, __st0_next} = (gzdLLzicase8536R1[16] == 1'h1) ? callResR4 : callResR3;
  initial __st0 <= 16'h0001;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 16'h0001;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module zdLLzicase8536 (input logic [15:0] arg0,
  output logic [25:0] res);
  logic [15:0] gMainzisig;
  logic [15:0] gMainzifirst;
  logic [23:0] callRes;
  logic [23:0] gzdLLzilambda8589;
  logic [23:0] gzdLLzicase8553;
  logic [25:0] callResR1;
  logic [25:0] gzdLLzilambda8584;
  logic [25:0] gzdLLzicase8582;
  logic [23:0] gzdLLzilambda8455;
  assign gMainzisig = arg0;
  assign gMainzifirst = gMainzisig[15:0];
  Mainzifirst  Mainzifirst (gMainzifirst[15:0], callRes);
  assign gzdLLzilambda8589 = callRes;
  assign gzdLLzicase8553 = gzdLLzilambda8589[23:0];
  zdLLzicase8553  zdLLzicase8553 (gzdLLzicase8553[23:16], gzdLLzicase8553[15:0], callResR1);
  assign gzdLLzilambda8584 = callResR1;
  assign gzdLLzicase8582 = gzdLLzilambda8584[25:0];
  assign gzdLLzilambda8455 = {gzdLLzicase8582[23:16], gzdLLzicase8582[15:0]};
  assign res = {2'h2, gzdLLzilambda8455[23:16], gzdLLzilambda8455[15:0]};
endmodule

module zdLLzicase8553 (input logic [7:0] arg0,
  input logic [15:0] arg1,
  output logic [25:0] res);
  assign res = {2'h0, arg0, arg1};
endmodule

module Mainzifirst (input logic [15:0] arg0,
  output logic [23:0] res);
  logic [31:0] gzdLLzilambda8528;
  logic [31:0] gzdLLzilambda8442;
  logic [15:0] id;
  assign gzdLLzilambda8528 = {arg0, arg0};
  assign gzdLLzilambda8442 = gzdLLzilambda8528[31:0];
  assign id = gzdLLzilambda8442[31:16];
  assign res = {id[15:8], gzdLLzilambda8442[15:0]};
endmodule