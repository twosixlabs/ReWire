module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  input logic [7:0] __in1,
  input logic [7:0] __in2,
  output logic [7:0] __out0);
  logic [23:0] gMainziloop;
  logic [23:0] gMainzicompute;
  logic [23:0] gzdLLziMainzicompute1;
  logic [23:0] gzdLLziMainzicompute;
  logic [15:0] binOp;
  logic [15:0] binOpR1;
  logic [7:0] unOp;
  logic [7:0] unOpR1;
  logic [15:0] binOpR2;
  logic [15:0] binOpR3;
  logic [15:0] binOpR4;
  logic [8:0] gzdLLziMainziloop2;
  logic [8:0] gzdLLziMainziloop;
  logic [8:0] callRes;
  logic [127:0] resizzeS0;
  logic [7:0] unOpS0;
  logic [7:0] unOpS0R1;
  logic [15:0] binOpS0;
  logic [15:0] binOpS0R1;
  logic [15:0] binOpS0R2;
  logic [8:0] gzdLLziMainziloop3S0;
  logic [8:0] gzdLLziMainziloopS0;
  logic [0:0] __continue;
  logic [23:0] __resumption_tag;
  logic [23:0] __resumption_tag_next;
  assign resizzeS0 = {8'h80{1'h0}};
  assign unOpS0 = resizzeS0[7:0];
  assign unOpS0R1 = 8'(128'h00000000000000000000000000000001);
  assign binOpS0 = {~unOpS0[7:0], ~unOpS0R1[7:0]};
  assign binOpS0R1 = {binOpS0[15:8] & binOpS0[7:0], 8'(128'h00000000000000000000000000000002)};
  assign binOpS0R2 = {8'h02, binOpS0R1[15:8] ~^ binOpS0R1[7:0]};
  assign gzdLLziMainziloop3S0 = {1'h0, binOpS0R2[15:8] | binOpS0R2[7:0]};
  assign gzdLLziMainziloopS0 = gzdLLziMainziloop3S0[8:0];
  assign gMainziloop = __resumption_tag;
  assign gMainzicompute = gMainziloop[23:0];
  assign gzdLLziMainzicompute1 = gMainzicompute[23:0];
  assign gzdLLziMainzicompute = {gzdLLziMainzicompute1[15:8], gzdLLziMainzicompute1[23:16], gzdLLziMainzicompute1[7:0]};
  assign binOp = {gzdLLziMainzicompute[15:8], gzdLLziMainzicompute[23:16]};
  assign binOpR1 = {binOp[15:8] & binOp[7:0], gzdLLziMainzicompute[7:0]};
  assign unOp = gzdLLziMainzicompute[15:8];
  assign unOpR1 = gzdLLziMainzicompute[23:16];
  assign binOpR2 = {~unOp[7:0], ~unOpR1[7:0]};
  assign binOpR3 = {binOpR2[15:8] & binOpR2[7:0], gzdLLziMainzicompute[7:0]};
  assign binOpR4 = {binOpR1[15:8] ^ binOpR1[7:0], binOpR3[15:8] ~^ binOpR3[7:0]};
  assign gzdLLziMainziloop2 = {1'h0, binOpR4[15:8] | binOpR4[7:0]};
  assign gzdLLziMainziloop = gzdLLziMainziloop2[8:0];
  zdLLziMainziloop  zdLLziMainziloop (gzdLLziMainziloop[7:0], callRes);
  assign {__continue, __out0, __resumption_tag_next} = callRes;
  initial __resumption_tag <= 24'h000000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 24'h000000;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLziMainziloop (input logic [7:0] arg0,
  output logic [8:0] res);
  assign res = {1'h1, arg0};
endmodule