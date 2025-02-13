module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [15:0] __in0,
  output logic [7:0] __out0);
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
  logic [16:0] gzdLLziMainzicompute1;
  logic [15:0] idR2;
  logic [16:0] gzdLLziMainzicompute;
  logic [15:0] idR3;
  logic [8:0] gzdLLziMainziloop2;
  logic [8:0] gzdLLziMainziloop;
  logic [0:0] __continue;
  logic [15:0] __resumption_tag;
  logic [15:0] __resumption_tag_next;
  assign gMainziloop = __resumption_tag;
  assign gMainzicompute = gMainziloop[15:0];
  assign msbit = gMainzicompute[15:0];
  assign id = gMainzicompute[15:0];
  assign gReWireziPreludezizaza = {msbit[15], id[8]};
  ReWireziPreludezizaza  ReWireziPreludezizaza (gReWireziPreludezizaza[1], gReWireziPreludezizaza[0], callRes);
  assign msbitR1 = gMainzicompute[15:0];
  assign idR1 = gMainzicompute[15:0];
  assign gReWireziPreludezizazaR1 = {msbitR1[15], idR1[8]};
  ReWireziPreludezizaza  ReWireziPreludezizazaR1 (gReWireziPreludezizazaR1[1], gReWireziPreludezizazaR1[0], callResR1);
  assign gzdLLziMainzicompute1 = {gMainzicompute[15:0], callResR1};
  assign idR2 = gzdLLziMainzicompute1[16:1];
  assign gzdLLziMainzicompute = {gMainzicompute[15:0], callRes};
  assign idR3 = gzdLLziMainzicompute[16:1];
  assign gzdLLziMainziloop2 = {1'h0, (gzdLLziMainzicompute[0] == 1'h1) ? idR3[7:0] : idR2[15:8]};
  assign gzdLLziMainziloop = gzdLLziMainziloop2[8:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, gzdLLziMainziloop[7:0]};
  initial __resumption_tag <= 16'h0100;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 16'h0100;
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