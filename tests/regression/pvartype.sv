module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] gzdLLziMainzigo;
  logic [0:0] gMainzigo;
  logic [1:0] gzdLLziMainzigo13;
  logic [1:0] gzdLLziMainzigo2;
  logic [1:0] id;
  logic [1:0] idR1;
  logic [0:0] gzdLLziMainzigo9;
  logic [2:0] gzdLLziMainzigo8;
  logic [2:0] gzdLLziMainzigo7;
  logic [0:0] gzdLLziMainzigo1;
  logic [0:0] __continue;
  logic [0:0] __st0;
  logic [0:0] __st0_next;
  assign gzdLLziMainzigo = {__in0, __st0};
  assign gMainzigo = gzdLLziMainzigo[0];
  assign gzdLLziMainzigo13 = {gMainzigo[0], gMainzigo[0]};
  assign gzdLLziMainzigo2 = gzdLLziMainzigo13[1:0];
  assign id = {gzdLLziMainzigo2[0], gzdLLziMainzigo2[1]};
  assign idR1 = {gzdLLziMainzigo2[0], gzdLLziMainzigo2[1]};
  assign gzdLLziMainzigo9 = (idR1[0] == 1'h1) ? idR1[1] : id[1];
  assign gzdLLziMainzigo8 = {2'h0, gzdLLziMainzigo9[0]};
  assign gzdLLziMainzigo7 = gzdLLziMainzigo8[2:0];
  assign gzdLLziMainzigo1 = gzdLLziMainzigo7[0];
  assign {__continue, __out0, __st0_next} = {2'h2, gzdLLziMainzigo1[0]};
  initial __st0 <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __st0 <= 1'h1;
    end else begin
      __st0 <= __st0_next;
    end
  end
endmodule