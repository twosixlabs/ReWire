module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [8:0] gzdLLziMainzigo;
  logic [8:0] gzdLLziMainzigo2;
  logic [16:0] callRes;
  logic [8:0] gzdLLziMainzigo2R1;
  logic [16:0] callResR1;
  logic [0:0] __continue;
  logic [7:0] __st0;
  logic [7:0] __st0_next;
  assign gzdLLziMainzigo = {__in0, __st0};
  assign gzdLLziMainzigo2 = {gzdLLziMainzigo[7:0], gzdLLziMainzigo[8]};
  zdLLziMainzigo2  zdLLziMainzigo2 (gzdLLziMainzigo2[8:1], callRes);
  assign gzdLLziMainzigo2R1 = {gzdLLziMainzigo[7:0], gzdLLziMainzigo[8]};
  zdLLziMainzigo2  zdLLziMainzigo2R1 (gzdLLziMainzigo2R1[8:1], callResR1);
  assign {__continue, __out0, __st0_next} = (gzdLLziMainzigo2R1[0] == 1'h1) ? callResR1 : callRes;
  initial __st0 <= 8'h00;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 8'h00;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule

module zdLLziMainzigo2 (input logic [7:0] arg0,
  output logic [16:0] res);
  logic [7:0] gMainzigo;
  logic [15:0] gzdLLziMainzigo8;
  logic [15:0] gzdLLziMainzigo6;
  logic [16:0] gzdLLziMainzigo5;
  logic [16:0] gzdLLziMainzigo4;
  logic [15:0] gzdLLziMainzigo1;
  assign gMainzigo = arg0;
  assign gzdLLziMainzigo8 = {gMainzigo[7:0], gMainzigo[7:0]};
  assign gzdLLziMainzigo6 = gzdLLziMainzigo8[15:0];
  assign gzdLLziMainzigo5 = {1'h0, gzdLLziMainzigo6[15:8], gzdLLziMainzigo6[7:0]};
  assign gzdLLziMainzigo4 = gzdLLziMainzigo5[16:0];
  assign gzdLLziMainzigo1 = {gzdLLziMainzigo4[15:8], gzdLLziMainzigo4[7:0]};
  assign res = {1'h1, gzdLLziMainzigo1[15:8], gzdLLziMainzigo1[7:0]};
endmodule