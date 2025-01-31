module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [3:0] gzdLLzicase5371;
  logic [4:0] callRes;
  logic [3:0] gzdLLzicase5371R1;
  logic [4:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __st1;
  logic [0:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  logic [0:0] __st1_next;
  assign gzdLLzicase5371 = {{__resumption_tag, __st0, __st1}, __in0};
  zdLLzicase5371  zdLLzicase5371 (gzdLLzicase5371[2], gzdLLzicase5371[1], gzdLLzicase5371[0], callRes);
  assign gzdLLzicase5371R1 = {{__resumption_tag, __st0, __st1}, __in0};
  zdLLzicase5371  zdLLzicase5371R1 (gzdLLzicase5371R1[2], gzdLLzicase5371R1[1], gzdLLzicase5371R1[0], callResR1);
  assign {__continue, __out0, __resumption_tag_next, __st0_next, __st1_next} = (gzdLLzicase5371R1[3] == 1'h0) ? callResR1 : callRes;
  initial {__resumption_tag, __st0, __st1} <= 3'h7;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 3'h7;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module zdLLzicase5194 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [4:0] res);
  assign res = {3'h0, arg0, arg1};
endmodule

module zdLLzicase5371 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  output logic [4:0] res);
  logic [2:0] gMainzisig;
  logic [0:0] gReWireziPreludezinot;
  logic [0:0] callRes;
  logic [0:0] gReWireziPreludezinotR1;
  logic [0:0] callResR1;
  logic [2:0] gzdLLzicase5504;
  logic [1:0] gzdLLzicase5194;
  logic [4:0] callResR2;
  logic [2:0] gzdLLzicase5507;
  logic [1:0] gzdLLzicase5195;
  logic [2:0] gzdLLzilambda5387;
  logic [2:0] gzdLLzicase5385;
  logic [4:0] callResR3;
  logic [4:0] gzdLLzilambda5381;
  logic [4:0] gzdLLzicase5379;
  logic [2:0] gzdLLzilambda5193;
  logic [2:0] gzdLLzilambda5435;
  logic [2:0] gzdLLzicase5385R1;
  logic [4:0] callResR4;
  logic [5:0] gzdLLzilambda5429;
  logic [5:0] gzdLLzicase5426;
  logic [3:0] gzdLLzilambda5191;
  logic [1:0] gzdLLzilambda5421;
  logic [1:0] gzdLLzicase5194R1;
  logic [4:0] callResR5;
  logic [6:0] gzdLLzilambda5415;
  logic [6:0] gzdLLzicase5411;
  logic [3:0] gzdLLzilambda5188;
  logic [1:0] binOp;
  logic [0:0] msbit;
  logic [1:0] gzdLLzilambda5405;
  logic [1:0] gzdLLzicase5194R2;
  logic [4:0] callResR6;
  logic [4:0] gzdLLzilambda5501;
  logic [4:0] gzdLLzicase5499;
  logic [1:0] gzdLLzilambda5201;
  logic [2:0] gzdLLzilambda5447;
  logic [2:0] gzdLLzicase5385R2;
  logic [4:0] callResR7;
  logic [4:0] gzdLLzilambda5441;
  logic [4:0] gzdLLzicase5439;
  logic [2:0] gzdLLzilambda5199;
  assign gMainzisig = {arg2, arg0, arg1};
  assign gReWireziPreludezinot = gMainzisig[2];
  ReWireziPreludezinot  ReWireziPreludezinot (gReWireziPreludezinot[0], callRes);
  assign gReWireziPreludezinotR1 = gMainzisig[2];
  ReWireziPreludezinot  ReWireziPreludezinotR1 (gReWireziPreludezinotR1[0], callResR1);
  assign gzdLLzicase5504 = {callResR1, gMainzisig[1], gMainzisig[0]};
  assign gzdLLzicase5194 = {gzdLLzicase5504[1], gzdLLzicase5504[0]};
  zdLLzicase5194  zdLLzicase5194 (gzdLLzicase5194[1], gzdLLzicase5194[0], callResR2);
  assign gzdLLzicase5507 = {callRes, gMainzisig[1], gMainzisig[0]};
  assign gzdLLzicase5195 = {gzdLLzicase5507[1], gzdLLzicase5507[0]};
  assign gzdLLzilambda5387 = {gzdLLzicase5195[1], gzdLLzicase5195[1], gzdLLzicase5195[0]};
  assign gzdLLzicase5385 = gzdLLzilambda5387[2:0];
  zdLLzicase5385  zdLLzicase5385 (gzdLLzicase5385[2], gzdLLzicase5385[1], gzdLLzicase5385[0], callResR3);
  assign gzdLLzilambda5381 = callResR3;
  assign gzdLLzicase5379 = gzdLLzilambda5381[4:0];
  assign gzdLLzilambda5193 = {gzdLLzicase5379[2], gzdLLzicase5379[1], gzdLLzicase5379[0]};
  assign gzdLLzilambda5435 = {gzdLLzilambda5193[0], gzdLLzilambda5193[1], gzdLLzilambda5193[0]};
  assign gzdLLzicase5385R1 = gzdLLzilambda5435[2:0];
  zdLLzicase5385  zdLLzicase5385R1 (gzdLLzicase5385R1[2], gzdLLzicase5385R1[1], gzdLLzicase5385R1[0], callResR4);
  assign gzdLLzilambda5429 = {gzdLLzilambda5193[2], callResR4};
  assign gzdLLzicase5426 = {gzdLLzilambda5429[4:0], gzdLLzilambda5429[5]};
  assign gzdLLzilambda5191 = {gzdLLzicase5426[0], gzdLLzicase5426[3], gzdLLzicase5426[2], gzdLLzicase5426[1]};
  assign gzdLLzilambda5421 = {gzdLLzilambda5191[2], gzdLLzilambda5191[0]};
  assign gzdLLzicase5194R1 = gzdLLzilambda5421[1:0];
  zdLLzicase5194  zdLLzicase5194R1 (gzdLLzicase5194R1[1], gzdLLzicase5194R1[0], callResR5);
  assign gzdLLzilambda5415 = {gzdLLzilambda5191[3], gzdLLzilambda5191[2], callResR5};
  assign gzdLLzicase5411 = {gzdLLzilambda5415[4:0], gzdLLzilambda5415[6], gzdLLzilambda5415[5]};
  assign gzdLLzilambda5188 = {gzdLLzicase5411[1], gzdLLzicase5411[0], gzdLLzicase5411[3], gzdLLzicase5411[2]};
  assign binOp = {gzdLLzilambda5188[3], gzdLLzilambda5188[2]};
  assign msbit = binOp[1] ^ binOp[0];
  assign gzdLLzilambda5405 = {gzdLLzilambda5188[1], msbit[0]};
  assign gzdLLzicase5194R2 = gzdLLzilambda5405[1:0];
  zdLLzicase5194  zdLLzicase5194R2 (gzdLLzicase5194R2[1], gzdLLzicase5194R2[0], callResR6);
  assign gzdLLzilambda5501 = (gzdLLzicase5507[2] == 1'h1) ? callResR6 : callResR2;
  assign gzdLLzicase5499 = gzdLLzilambda5501[4:0];
  assign gzdLLzilambda5201 = {gzdLLzicase5499[1], gzdLLzicase5499[0]};
  assign gzdLLzilambda5447 = {gzdLLzilambda5201[1], gzdLLzilambda5201[1], gzdLLzilambda5201[0]};
  assign gzdLLzicase5385R2 = gzdLLzilambda5447[2:0];
  zdLLzicase5385  zdLLzicase5385R2 (gzdLLzicase5385R2[2], gzdLLzicase5385R2[1], gzdLLzicase5385R2[0], callResR7);
  assign gzdLLzilambda5441 = callResR7;
  assign gzdLLzicase5439 = gzdLLzilambda5441[4:0];
  assign gzdLLzilambda5199 = {gzdLLzicase5439[2], gzdLLzicase5439[1], gzdLLzicase5439[0]};
  assign res = {1'h1, gzdLLzilambda5199[2], 1'h0, gzdLLzilambda5199[1], gzdLLzilambda5199[0]};
endmodule

module zdLLzicase5385 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  output logic [4:0] res);
  assign res = {2'h1, arg0, arg1, arg2};
endmodule

module ReWireziPreludezinot (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 1'h0 : 1'h1;
endmodule