module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] gzdLLzicase5713;
  logic [4:0] callRes;
  logic [2:0] gzdLLzicase5713R1;
  logic [4:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [0:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  assign gzdLLzicase5713 = {{__resumption_tag, __st0}, __in0};
  zdLLzicase5713  zdLLzicase5713 (gzdLLzicase5713[1], gzdLLzicase5713[0], callRes);
  assign gzdLLzicase5713R1 = {{__resumption_tag, __st0}, __in0};
  zdLLzicase5713  zdLLzicase5713R1 (gzdLLzicase5713R1[1], gzdLLzicase5713R1[0], callResR1);
  assign {__continue, __padding, __out0, __resumption_tag_next, __st0_next} = (gzdLLzicase5713R1[2] == 1'h0) ? callResR1 : callRes;
  initial {__resumption_tag, __st0} <= 2'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 2'h0;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module zdLLzicase5713 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [4:0] res);
  logic [1:0] gReWireziMonadziiterSt5184;
  logic [2:0] gzdLLzilambda5780;
  logic [2:0] gzdLLzicase5777;
  logic [2:0] gzdLLzilambda5628;
  logic [1:0] gMainzif;
  logic [1:0] binOp;
  logic [0:0] msbit;
  logic [2:0] gzdLLzilambda5773;
  logic [2:0] gzdLLzicase5756;
  logic [4:0] gzdLLzilambda5768;
  logic [4:0] gzdLLzicase5766;
  logic [2:0] gzdLLzilambda5638;
  logic [2:0] gzdLLzicase5636;
  logic [0:0] gzdLLzicase5726;
  logic [5:0] gzdLLzilambda5735;
  logic [5:0] gzdLLzicase5732;
  logic [1:0] gzdLLzilambda5633;
  assign gReWireziMonadziiterSt5184 = {arg1, arg0};
  assign gzdLLzilambda5780 = {gReWireziMonadziiterSt5184[1], gReWireziMonadziiterSt5184[0], gReWireziMonadziiterSt5184[0]};
  assign gzdLLzicase5777 = {gzdLLzilambda5780[1:0], gzdLLzilambda5780[2]};
  assign gzdLLzilambda5628 = {gzdLLzicase5777[0], gzdLLzicase5777[2], gzdLLzicase5777[1]};
  assign gMainzif = {gzdLLzilambda5628[2], gzdLLzilambda5628[1]};
  assign binOp = {gMainzif[0], gMainzif[1]};
  assign msbit = binOp[1] ^ binOp[0];
  assign gzdLLzilambda5773 = {{msbit[0], gMainzif[0]}, gzdLLzilambda5628[0]};
  assign gzdLLzicase5756 = gzdLLzilambda5773[2:0];
  assign gzdLLzilambda5768 = {2'h0, gzdLLzicase5756[2:1], gzdLLzicase5756[0]};
  assign gzdLLzicase5766 = gzdLLzilambda5768[4:0];
  assign gzdLLzilambda5638 = {gzdLLzicase5766[2:1], gzdLLzicase5766[0]};
  assign gzdLLzicase5636 = {gzdLLzilambda5638[2:1], gzdLLzilambda5638[0]};
  assign gzdLLzicase5726 = gzdLLzicase5636[1];
  assign gzdLLzilambda5735 = {gzdLLzicase5636[2], {4'h4, gzdLLzicase5726[0]}};
  assign gzdLLzicase5732 = {gzdLLzilambda5735[4:0], gzdLLzilambda5735[5]};
  assign gzdLLzilambda5633 = {gzdLLzicase5732[0], gzdLLzicase5732[1]};
  assign res = {2'h2, gzdLLzilambda5633[1], 1'h1, gzdLLzilambda5633[0]};
endmodule