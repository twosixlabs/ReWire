module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] gzdLLzicase5410;
  logic [9:0] callRes;
  logic [16:0] gzdLLzicase5410R1;
  logic [9:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLzicase5410 = {__resumption_tag, __in0};
  zdLLzicase5410  zdLLzicase5410 (gzdLLzicase5410[15:0], callRes);
  assign gzdLLzicase5410R1 = {__resumption_tag, __in0};
  zdLLzicase5410  zdLLzicase5410R1 (gzdLLzicase5410R1[15:0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase5410R1[16] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase5410 (input logic [15:0] arg0,
  output logic [9:0] res);
  logic [15:0] gMainziloop;
  logic [15:0] gMainzicompute;
  logic [15:0] msbit;
  logic [15:0] id;
  logic [1:0] gReWireziPreludezizaza;
  logic [0:0] callRes;
  logic [15:0] msbitR1;
  logic [15:0] idR1;
  logic [1:0] gReWireziPreludezizazaR1;
  logic [0:0] callResR1;
  logic [16:0] gzdLLzicase5374;
  logic [15:0] idR2;
  logic [16:0] gzdLLzicase5376;
  logic [15:0] idR3;
  logic [9:0] gzdLLzilambda5416;
  logic [9:0] gzdLLzicase5414;
  logic [7:0] gzdLLzilambda5380;
  assign gMainziloop = arg0;
  assign gMainzicompute = gMainziloop[15:0];
  assign msbit = gMainzicompute[15:0];
  assign id = gMainzicompute[15:0];
  assign gReWireziPreludezizaza = {msbit[15], id[8]};
  ReWireziPreludezizaza  ReWireziPreludezizaza (gReWireziPreludezizaza[1], gReWireziPreludezizaza[0], callRes);
  assign msbitR1 = gMainzicompute[15:0];
  assign idR1 = gMainzicompute[15:0];
  assign gReWireziPreludezizazaR1 = {msbitR1[15], idR1[8]};
  ReWireziPreludezizaza  ReWireziPreludezizazaR1 (gReWireziPreludezizazaR1[1], gReWireziPreludezizazaR1[0], callResR1);
  assign gzdLLzicase5374 = {callResR1, gMainzicompute[15:0]};
  assign idR2 = gzdLLzicase5374[15:0];
  assign gzdLLzicase5376 = {callRes, gMainzicompute[15:0]};
  assign idR3 = gzdLLzicase5376[15:0];
  assign gzdLLzilambda5416 = {2'h0, (gzdLLzicase5376[16] == 1'h1) ? idR3[7:0] : idR2[15:8]};
  assign gzdLLzicase5414 = gzdLLzilambda5416[9:0];
  assign gzdLLzilambda5380 = gzdLLzicase5414[7:0];
  assign res = {1'h1, gzdLLzilambda5380[7:0], 1'h0};
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