module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [4:0] lit;
  logic [4:0] gzdLLziPurezidispatch5;
  logic [0:0] gzdLLziMainzigo8;
  logic [1:0] gzdLLziMainzigo10;
  logic [1:0] gzdLLziMainzigo9;
  logic [4:0] gzdLLziPurezidispatch4;
  logic [1:0] gzdLLziPurezidispatch3;
  logic [1:0] gzdLLziMainzigo5;
  logic [1:0] gzdLLziMainzigo3;
  logic [0:0] callRes;
  logic [1:0] id;
  logic [1:0] gReWireziPreludezizaza;
  logic [0:0] callResR1;
  logic [4:0] litR1;
  logic [4:0] gzdLLziPurezidispatch1;
  logic [1:0] gzdLLziPurezidispatch;
  logic [1:0] gzdLLziMainzigo1;
  logic [1:0] gzdLLziMainzigo3R1;
  logic [0:0] callResR2;
  logic [1:0] idR1;
  logic [1:0] gReWireziPreludezizazaR1;
  logic [0:0] callResR3;
  logic [0:0] __continue;
  logic [3:0] __resumption_tag;
  logic [3:0] __resumption_tag_next;
  assign lit = {__in0, __resumption_tag};
  assign gzdLLziPurezidispatch5 = {__in0, __resumption_tag};
  assign gzdLLziMainzigo8 = gzdLLziPurezidispatch5[4];
  assign gzdLLziMainzigo10 = {gzdLLziMainzigo8[0], gzdLLziMainzigo8[0]};
  assign gzdLLziMainzigo9 = {gzdLLziMainzigo8[0], gzdLLziMainzigo8[0]};
  assign gzdLLziPurezidispatch4 = {__in0, __resumption_tag};
  assign gzdLLziPurezidispatch3 = {gzdLLziPurezidispatch4[4], gzdLLziPurezidispatch4[0]};
  assign gzdLLziMainzigo5 = {gzdLLziPurezidispatch3[0], gzdLLziPurezidispatch3[1]};
  assign gzdLLziMainzigo3 = {gzdLLziMainzigo5[1], gzdLLziMainzigo5[0]};
  zdLLziMainzigo3  zdLLziMainzigo3 (gzdLLziMainzigo3[1], callRes);
  assign id = {gzdLLziMainzigo5[1], gzdLLziMainzigo5[0]};
  assign gReWireziPreludezizaza = {(id[0] == 1'h1) ? id[1] : callRes, gzdLLziMainzigo5[0]};
  ReWireziPreludezizaza  ReWireziPreludezizaza (gReWireziPreludezizaza[1], gReWireziPreludezizaza[0], callResR1);
  assign litR1 = {__in0, __resumption_tag};
  assign gzdLLziPurezidispatch1 = {__in0, __resumption_tag};
  assign gzdLLziPurezidispatch = {gzdLLziPurezidispatch1[4], gzdLLziPurezidispatch1[0]};
  assign gzdLLziMainzigo1 = {gzdLLziPurezidispatch[0], gzdLLziPurezidispatch[1]};
  assign gzdLLziMainzigo3R1 = {gzdLLziMainzigo1[1], gzdLLziMainzigo1[0]};
  zdLLziMainzigo3  zdLLziMainzigo3R1 (gzdLLziMainzigo3R1[1], callResR2);
  assign idR1 = {gzdLLziMainzigo1[1], gzdLLziMainzigo1[0]};
  assign gReWireziPreludezizazaR1 = {(idR1[0] == 1'h1) ? idR1[1] : callResR2, gzdLLziMainzigo1[0]};
  ReWireziPreludezizaza  ReWireziPreludezizazaR1 (gReWireziPreludezizazaR1[1], gReWireziPreludezizazaR1[0], callResR3);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLziPurezidispatch1[3:1] == 3'h1) ? {1'h1, callResR3, 4'h0} : ((litR1[3:1] == 3'h2) ? 6'h28 : ((gzdLLziPurezidispatch4[3:1] == 3'h3) ? {1'h1, callResR1, 4'h4} : ((gzdLLziPurezidispatch5[3:1] == 3'h4) ? ((gzdLLziMainzigo9[0] == 1'h1) ? {5'h11, gzdLLziMainzigo9[1]} : {1'h1, gzdLLziMainzigo10[1], 3'h3, gzdLLziMainzigo10[1]}) : 6'h28)));
  initial __resumption_tag <= 4'h8;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 4'h8;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLziMainzigo3 (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [1:0] gReWireziPreludezizaza;
  logic [0:0] callRes;
  assign gReWireziPreludezizaza = {arg0, arg0};
  ReWireziPreludezizaza  ReWireziPreludezizaza (gReWireziPreludezizaza[1], gReWireziPreludezizaza[0], callRes);
  assign res = callRes;
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