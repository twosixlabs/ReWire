module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] gzdLLzicase4690;
  logic [16:0] gzdLLzilambda4586;
  logic [16:0] gzdLLzicase4694;
  logic [25:0] callRes;
  logic [16:0] gzdLLzicase4750;
  logic [15:0] gMainziincr;
  logic [23:0] gzdLLzilambda4774;
  logic [23:0] gzdLLzicase4742;
  logic [25:0] callResR1;
  logic [25:0] gzdLLzilambda4768;
  logic [25:0] gzdLLzicase4766;
  logic [23:0] gzdLLzilambda4584;
  logic [23:0] gzdLLzilambda4744;
  logic [23:0] gzdLLzicase4742R1;
  logic [25:0] callResR2;
  logic [33:0] gzdLLzilambda4738;
  logic [33:0] gzdLLzicase4735;
  logic [31:0] gzdLLzilambda4582;
  logic [15:0] gzdLLzilambda4730;
  logic [15:0] gzdLLzicase4700;
  logic [25:0] callResR3;
  logic [41:0] gzdLLzilambda4724;
  logic [41:0] gzdLLzicase4720;
  logic [31:0] gzdLLzilambda4579;
  logic [15:0] binOp;
  logic [15:0] gzdLLzilambda4714;
  logic [15:0] gzdLLzicase4700R1;
  logic [25:0] callResR4;
  logic [25:0] gzdLLzilambda4708;
  logic [25:0] gzdLLzicase4694R1;
  logic [25:0] callResR5;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [7:0] __st0;
  logic [7:0] __st1;
  logic [7:0] __st0_next;
  logic [7:0] __st1_next;
  assign gzdLLzicase4690 = {{__st0, __st1}, __in0};
  assign gzdLLzilambda4586 = {gzdLLzicase4690[0], gzdLLzicase4690[16:9], gzdLLzicase4690[8:1]};
  assign gzdLLzicase4694 = {gzdLLzilambda4586[16], gzdLLzilambda4586[15:8], gzdLLzilambda4586[7:0]};
  zdLLzicase4694  zdLLzicase4694 (gzdLLzicase4694[15:8], gzdLLzicase4694[7:0], callRes);
  assign gzdLLzicase4750 = {gzdLLzilambda4586[16], gzdLLzilambda4586[15:8], gzdLLzilambda4586[7:0]};
  assign gMainziincr = {gzdLLzicase4750[15:8], gzdLLzicase4750[7:0]};
  assign gzdLLzilambda4774 = {gMainziincr[15:8], gMainziincr[15:8], gMainziincr[7:0]};
  assign gzdLLzicase4742 = gzdLLzilambda4774[23:0];
  zdLLzicase4742  zdLLzicase4742 (gzdLLzicase4742[23:16], gzdLLzicase4742[15:8], gzdLLzicase4742[7:0], callResR1);
  assign gzdLLzilambda4768 = callResR1;
  assign gzdLLzicase4766 = gzdLLzilambda4768[25:0];
  assign gzdLLzilambda4584 = {gzdLLzicase4766[23:16], gzdLLzicase4766[15:8], gzdLLzicase4766[7:0]};
  assign gzdLLzilambda4744 = {gzdLLzilambda4584[7:0], gzdLLzilambda4584[15:8], gzdLLzilambda4584[7:0]};
  assign gzdLLzicase4742R1 = gzdLLzilambda4744[23:0];
  zdLLzicase4742  zdLLzicase4742R1 (gzdLLzicase4742R1[23:16], gzdLLzicase4742R1[15:8], gzdLLzicase4742R1[7:0], callResR2);
  assign gzdLLzilambda4738 = {gzdLLzilambda4584[23:16], callResR2};
  assign gzdLLzicase4735 = {gzdLLzilambda4738[25:0], gzdLLzilambda4738[33:26]};
  assign gzdLLzilambda4582 = {gzdLLzicase4735[7:0], gzdLLzicase4735[31:24], gzdLLzicase4735[23:16], gzdLLzicase4735[15:8]};
  assign gzdLLzilambda4730 = {gzdLLzilambda4582[23:16], gzdLLzilambda4582[7:0]};
  assign gzdLLzicase4700 = gzdLLzilambda4730[15:0];
  zdLLzicase4700  zdLLzicase4700 (gzdLLzicase4700[15:8], gzdLLzicase4700[7:0], callResR3);
  assign gzdLLzilambda4724 = {gzdLLzilambda4582[31:24], gzdLLzilambda4582[23:16], callResR3};
  assign gzdLLzicase4720 = {gzdLLzilambda4724[25:0], gzdLLzilambda4724[41:34], gzdLLzilambda4724[33:26]};
  assign gzdLLzilambda4579 = {gzdLLzicase4720[15:8], gzdLLzicase4720[7:0], gzdLLzicase4720[31:24], gzdLLzicase4720[23:16]};
  assign binOp = {gzdLLzilambda4579[31:24], gzdLLzilambda4579[23:16]};
  assign gzdLLzilambda4714 = {gzdLLzilambda4579[15:8], binOp[15:8] + binOp[7:0]};
  assign gzdLLzicase4700R1 = gzdLLzilambda4714[15:0];
  zdLLzicase4700  zdLLzicase4700R1 (gzdLLzicase4700R1[15:8], gzdLLzicase4700R1[7:0], callResR4);
  assign gzdLLzilambda4708 = callResR4;
  assign gzdLLzicase4694R1 = gzdLLzilambda4708[25:0];
  zdLLzicase4694  zdLLzicase4694R1 (gzdLLzicase4694R1[15:8], gzdLLzicase4694R1[7:0], callResR5);
  assign {__continue, __padding, __out0, __st0_next, __st1_next} = (gzdLLzicase4750[16] == 1'h1) ? callResR5 : callRes;
  initial {__st0, __st1} <= 16'h0001;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 16'h0001;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module zdLLzicase4694 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  logic [15:0] gMainzisig;
  logic [23:0] gzdLLzilambda4786;
  logic [23:0] gzdLLzicase4742;
  logic [25:0] callRes;
  logic [25:0] gzdLLzilambda4780;
  logic [25:0] gzdLLzicase4778;
  logic [23:0] gzdLLzilambda4588;
  assign gMainzisig = {arg0, arg1};
  assign gzdLLzilambda4786 = {gMainzisig[15:8], gMainzisig[15:8], gMainzisig[7:0]};
  assign gzdLLzicase4742 = gzdLLzilambda4786[23:0];
  zdLLzicase4742  zdLLzicase4742 (gzdLLzicase4742[23:16], gzdLLzicase4742[15:8], gzdLLzicase4742[7:0], callRes);
  assign gzdLLzilambda4780 = callRes;
  assign gzdLLzicase4778 = gzdLLzilambda4780[25:0];
  assign gzdLLzilambda4588 = {gzdLLzicase4778[23:16], gzdLLzicase4778[15:8], gzdLLzicase4778[7:0]};
  assign res = {2'h2, gzdLLzilambda4588[23:16], gzdLLzilambda4588[15:8], gzdLLzilambda4588[7:0]};
endmodule

module zdLLzicase4700 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [25:0] res);
  assign res = {10'h100, arg0, arg1};
endmodule

module zdLLzicase4742 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [7:0] arg2,
  output logic [25:0] res);
  assign res = {2'h0, arg0, arg1, arg2};
endmodule