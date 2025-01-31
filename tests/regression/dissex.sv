module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] gzdLLzicase4553;
  logic [16:0] gzdLLzilambda4473;
  logic [16:0] gzdLLzicase4557;
  logic [25:0] callRes;
  logic [16:0] gzdLLzicase4601;
  logic [15:0] gMainziincr;
  logic [23:0] gzdLLzilambda4613;
  logic [23:0] gzdLLzicase4593;
  logic [25:0] callResR1;
  logic [25:0] gzdLLzilambda4607;
  logic [25:0] gzdLLzicase4605;
  logic [23:0] gzdLLzilambda4471;
  logic [23:0] gzdLLzilambda4595;
  logic [23:0] gzdLLzicase4593R1;
  logic [25:0] callResR2;
  logic [33:0] gzdLLzilambda4589;
  logic [33:0] gzdLLzicase4586;
  logic [31:0] gzdLLzilambda4469;
  logic [15:0] gzdLLzilambda4581;
  logic [15:0] gzdLLzicase4563;
  logic [25:0] callResR3;
  logic [41:0] gzdLLzilambda4575;
  logic [41:0] gzdLLzicase4571;
  logic [31:0] gzdLLzilambda4466;
  logic [15:0] binOp;
  logic [15:0] gzdLLzilambda4565;
  logic [15:0] gzdLLzicase4563R1;
  logic [25:0] callResR4;
  logic [25:0] gzdLLzilambda4559;
  logic [25:0] gzdLLzicase4557R1;
  logic [25:0] callResR5;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st1;
  logic [7:0] __st0_next;
  logic [7:0] __st1_next;
  assign gzdLLzicase4553 = {{__st0, __st1}, __in0};
  assign gzdLLzilambda4473 = {gzdLLzicase4553[0], gzdLLzicase4553[16:9], gzdLLzicase4553[8:1]};
  assign gzdLLzicase4557 = {gzdLLzilambda4473[16], gzdLLzilambda4473[15:8], gzdLLzilambda4473[7:0]};
  zdLLzicase4557  zdLLzicase4557 (gzdLLzicase4557[15:8], gzdLLzicase4557[7:0], callRes);
  assign gzdLLzicase4601 = {gzdLLzilambda4473[16], gzdLLzilambda4473[15:8], gzdLLzilambda4473[7:0]};
  assign gMainziincr = {gzdLLzicase4601[15:8], gzdLLzicase4601[7:0]};
  assign gzdLLzilambda4613 = {gMainziincr[15:8], gMainziincr[15:8], gMainziincr[7:0]};
  assign gzdLLzicase4593 = gzdLLzilambda4613[23:0];
  zdLLzicase4593  zdLLzicase4593 (gzdLLzicase4593[23:16], gzdLLzicase4593[15:8], gzdLLzicase4593[7:0], callResR1);
  assign gzdLLzilambda4607 = callResR1;
  assign gzdLLzicase4605 = gzdLLzilambda4607[25:0];
  assign gzdLLzilambda4471 = {gzdLLzicase4605[23:16], gzdLLzicase4605[15:8], gzdLLzicase4605[7:0]};
  assign gzdLLzilambda4595 = {gzdLLzilambda4471[7:0], gzdLLzilambda4471[15:8], gzdLLzilambda4471[7:0]};
  assign gzdLLzicase4593R1 = gzdLLzilambda4595[23:0];
  zdLLzicase4593  zdLLzicase4593R1 (gzdLLzicase4593R1[23:16], gzdLLzicase4593R1[15:8], gzdLLzicase4593R1[7:0], callResR2);
  assign gzdLLzilambda4589 = {gzdLLzilambda4471[23:16], callResR2};
  assign gzdLLzicase4586 = {gzdLLzilambda4589[25:0], gzdLLzilambda4589[33:26]};
  assign gzdLLzilambda4469 = {gzdLLzicase4586[7:0], gzdLLzicase4586[31:24], gzdLLzicase4586[23:16], gzdLLzicase4586[15:8]};
  assign gzdLLzilambda4581 = {gzdLLzilambda4469[23:16], gzdLLzilambda4469[7:0]};
  assign gzdLLzicase4563 = gzdLLzilambda4581[15:0];
  zdLLzicase4563  zdLLzicase4563 (gzdLLzicase4563[15:8], gzdLLzicase4563[7:0], callResR3);
  assign gzdLLzilambda4575 = {gzdLLzilambda4469[31:24], gzdLLzilambda4469[23:16], callResR3};
  assign gzdLLzicase4571 = {gzdLLzilambda4575[25:0], gzdLLzilambda4575[41:34], gzdLLzilambda4575[33:26]};
  assign gzdLLzilambda4466 = {gzdLLzicase4571[15:8], gzdLLzicase4571[7:0], gzdLLzicase4571[31:24], gzdLLzicase4571[23:16]};
  assign binOp = {gzdLLzilambda4466[31:24], gzdLLzilambda4466[23:16]};
  assign gzdLLzilambda4565 = {gzdLLzilambda4466[15:8], binOp[15:8] + binOp[7:0]};
  assign gzdLLzicase4563R1 = gzdLLzilambda4565[15:0];
  zdLLzicase4563  zdLLzicase4563R1 (gzdLLzicase4563R1[15:8], gzdLLzicase4563R1[7:0], callResR4);
  assign gzdLLzilambda4559 = callResR4;
  assign gzdLLzicase4557R1 = gzdLLzilambda4559[25:0];
  zdLLzicase4557  zdLLzicase4557R1 (gzdLLzicase4557R1[15:8], gzdLLzicase4557R1[7:0], callResR5);
  assign {__continue, __padding, __out0, __st0_next, __st1_next} = (gzdLLzicase4601[16] == 1'h1) ? callResR5 : callRes;
  initial {__st0, __st1} <= 16'h0001;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 16'h0001;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module zdLLzicase4557 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  logic [15:0] gMainzisig;
  logic [23:0] gzdLLzilambda4625;
  logic [23:0] gzdLLzicase4593;
  logic [25:0] callRes;
  logic [25:0] gzdLLzilambda4619;
  logic [25:0] gzdLLzicase4617;
  logic [23:0] gzdLLzilambda4475;
  assign gMainzisig = {arg0, arg1};
  assign gzdLLzilambda4625 = {gMainzisig[15:8], gMainzisig[15:8], gMainzisig[7:0]};
  assign gzdLLzicase4593 = gzdLLzilambda4625[23:0];
  zdLLzicase4593  zdLLzicase4593 (gzdLLzicase4593[23:16], gzdLLzicase4593[15:8], gzdLLzicase4593[7:0], callRes);
  assign gzdLLzilambda4619 = callRes;
  assign gzdLLzicase4617 = gzdLLzilambda4619[25:0];
  assign gzdLLzilambda4475 = {gzdLLzicase4617[23:16], gzdLLzicase4617[15:8], gzdLLzicase4617[7:0]};
  assign res = {2'h2, gzdLLzilambda4475[23:16], gzdLLzilambda4475[15:8], gzdLLzilambda4475[7:0]};
endmodule

module zdLLzicase4563 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  assign res = {10'h100, arg0, arg1};
endmodule

module zdLLzicase4593 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  output logic [25:0] res);
  assign res = {2'h0, arg0, arg1, arg2};
endmodule