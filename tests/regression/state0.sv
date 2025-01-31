module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [2:0] __in0,
  output logic [0:0] __out0,
  output logic [2:0] __out1);
  logic [8:0] lit;
  logic [8:0] gzdLLzicase4581;
  logic [2:0] litR1;
  logic [8:0] gzdLLzicase4583;
  logic [10:0] callRes;
  logic [8:0] gzdLLzicase4586;
  logic [5:0] gzdLLzilambda4552;
  logic [8:0] gzdLLzicase4589;
  logic [5:0] gzdLLzilambda4549;
  logic [8:0] gzdLLzicase4583R1;
  logic [10:0] callResR1;
  logic [0:0] __continue;
  logic [5:0] __resumption_tag;
  logic [5:0] __resumption_tag_next;
  assign lit = {__resumption_tag, __in0};
  assign gzdLLzicase4581 = {__resumption_tag, __in0};
  assign litR1 = gzdLLzicase4581[2:0];
  assign gzdLLzicase4583 = {__resumption_tag, __in0};
  zdLLzicase4583  zdLLzicase4583 (gzdLLzicase4583[2:0], callRes);
  assign gzdLLzicase4586 = {__resumption_tag, __in0};
  assign gzdLLzilambda4552 = {gzdLLzicase4586[5:3], gzdLLzicase4586[2:0]};
  assign gzdLLzicase4589 = {__resumption_tag, __in0};
  assign gzdLLzilambda4549 = {gzdLLzicase4589[5:3], gzdLLzicase4589[2:0]};
  assign gzdLLzicase4583R1 = {__resumption_tag, __in0};
  zdLLzicase4583  zdLLzicase4583R1 (gzdLLzicase4583R1[2:0], callResR1);
  assign {__continue, __out0, __out1, __resumption_tag_next} = (gzdLLzicase4583R1[8:6] == 3'h0) ? callResR1 : ((gzdLLzicase4589[8:6] == 3'h1) ? {2'h3, gzdLLzilambda4549[5:3], 6'h00} : ((gzdLLzicase4586[8:6] == 3'h2) ? {8'h81, gzdLLzilambda4552[5:3]} : ((gzdLLzicase4583[8:6] == 3'h3) ? callRes : ((gzdLLzicase4581[8:6] == 3'h4) ? 11'h618 : 11'h420))));
  initial __resumption_tag <= 6'h28;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 6'h28;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase4583 (input logic [2:0] arg0,
  output logic [10:0] res);
  logic [2:0] gMainzidev;
  assign gMainzidev = arg0;
  assign res = {8'h82, gMainzidev[2:0]};
endmodule