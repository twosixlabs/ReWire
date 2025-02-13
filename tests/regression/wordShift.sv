module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [7:0] __out0);
  logic [7:0] gMainziloop;
  logic [7:0] gMainzimyRotr;
  logic [15:0] binOp;
  logic [15:0] binOpR1;
  logic [15:0] binOpR2;
  logic [7:0] gMainzimyArithRotr;
  logic [15:0] binOpR3;
  logic [15:0] binOpR4;
  logic [15:0] binOpR5;
  logic [8:0] gzdLLziMainziloop2;
  logic [8:0] gzdLLziMainziloop;
  logic [0:0] __continue;
  logic [7:0] __resumption_tag;
  logic [7:0] __resumption_tag_next;
  assign gMainziloop = __resumption_tag;
  assign gMainzimyRotr = gMainziloop[7:0];
  assign binOp = {gMainzimyRotr[7:0], 8'h05};
  assign binOpR1 = {gMainzimyRotr[7:0], 8'h03};
  assign binOpR2 = {binOp[15:8] << binOp[7:0], binOpR1[15:8] >> binOpR1[7:0]};
  assign gMainzimyArithRotr = binOpR2[15:8] | binOpR2[7:0];
  assign binOpR3 = {gMainzimyArithRotr[7:0], 8'h03};
  assign binOpR4 = {gMainzimyArithRotr[7:0], 8'h05};
  assign binOpR5 = {binOpR3[15:8] << binOpR3[7:0], binOpR4[15:8] >>> binOpR4[7:0]};
  assign gzdLLziMainziloop2 = {1'h0, binOpR5[15:8] | binOpR5[7:0]};
  assign gzdLLziMainziloop = gzdLLziMainziloop2[8:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, gzdLLziMainziloop[7:0]};
  initial __resumption_tag <= 8'hfc;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 8'hfc;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule