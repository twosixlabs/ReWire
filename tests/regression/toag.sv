module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] gzdLLzicase2617;
  logic [2:0] gzdLLzicase2619;
  logic [0:0] gzdLLzilambda2598;
  logic [1:0] gzdLLzicase2629;
  logic [0:0] gzdLLzicase2586;
  logic [0:0] lit;
  logic [4:0] gzdLLzilambda2627;
  logic [4:0] gzdLLzicase2624;
  logic [1:0] gzdLLzilambda2596;
  logic [1:0] gzdLLzicase2589;
  logic [1:0] gReWireziPreludezizaza;
  logic [0:0] callRes;
  logic [1:0] id;
  logic [1:0] gReWireziPreludezizazaR1;
  logic [0:0] callResR1;
  logic [2:0] litR1;
  logic [0:0] __continue;
  logic [1:0] __resumption_tag;
  logic [1:0] __resumption_tag_next;
  assign gzdLLzicase2617 = {__resumption_tag, __in0};
  assign gzdLLzicase2619 = {__resumption_tag, __in0};
  assign gzdLLzilambda2598 = gzdLLzicase2619[0];
  assign gzdLLzicase2629 = {gzdLLzilambda2598[0], gzdLLzilambda2598[0]};
  assign gzdLLzicase2586 = gzdLLzicase2629[0];
  assign lit = gzdLLzilambda2598[0];
  assign gzdLLzilambda2627 = {gzdLLzilambda2598[0], (lit[0] == 1'h1) ? 4'ha : {1'h1, gzdLLzicase2586[0], 2'h2}};
  assign gzdLLzicase2624 = {gzdLLzilambda2627[3:0], gzdLLzilambda2627[4]};
  assign gzdLLzilambda2596 = {gzdLLzicase2624[0], gzdLLzicase2624[1]};
  assign gzdLLzicase2589 = {gzdLLzilambda2596[0], gzdLLzilambda2596[1]};
  assign gReWireziPreludezizaza = {gzdLLzicase2589[0], gzdLLzicase2589[0]};
  ReWireziPreludezizaza  ReWireziPreludezizaza (gReWireziPreludezizaza[1], gReWireziPreludezizaza[0], callRes);
  assign id = {gzdLLzilambda2596[0], gzdLLzilambda2596[1]};
  assign gReWireziPreludezizazaR1 = {(id[1] == 1'h1) ? id[0] : callRes, gzdLLzilambda2596[0]};
  ReWireziPreludezizaza  ReWireziPreludezizazaR1 (gReWireziPreludezizazaR1[1], gReWireziPreludezizazaR1[0], callResR1);
  assign litR1 = {__resumption_tag, __in0};
  assign {__continue, __out0, __resumption_tag_next} = (litR1[2:1] == 2'h0) ? 4'h9 : ((gzdLLzicase2619[2:1] == 2'h1) ? {1'h1, callResR1, 2'h0} : {3'h0, gzdLLzicase2617[0]});
  initial __resumption_tag <= 2'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 2'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module ReWireziPreludezizaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [1:0] lit;
  logic [1:0] id;
  assign lit = {arg0, arg1};
  assign id = {arg0, arg1};
  assign res = (id[1] == 1'h1) ? id[0] : 1'h0;
endmodule