module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [1023:0] __in0,
  input logic [31:0] __in1,
  output logic [63:0] __out0);
  logic [1055:0] gMainziloop;
  logic [1055:0] gzdLLziMainziloop2;
  logic [1055:0] gMainzicompute;
  logic [1023:0] id;
  logic [511:0] idR1;
  logic [95:0] gzdLLziMainzicompute;
  logic [7:0] callRes;
  logic [1023:0] idR2;
  logic [511:0] idR3;
  logic [95:0] gzdLLziMainzicomputeR1;
  logic [7:0] callResR1;
  logic [1023:0] idR4;
  logic [511:0] idR5;
  logic [95:0] gzdLLziMainzicomputeR2;
  logic [7:0] callResR2;
  logic [1023:0] idR6;
  logic [511:0] idR7;
  logic [95:0] gzdLLziMainzicomputeR3;
  logic [7:0] callResR3;
  logic [1023:0] idR8;
  logic [511:0] idR9;
  logic [95:0] gzdLLziMainzicomputeR4;
  logic [7:0] callResR4;
  logic [1023:0] idR10;
  logic [511:0] idR11;
  logic [95:0] gzdLLziMainzicomputeR5;
  logic [7:0] callResR5;
  logic [1023:0] idR12;
  logic [511:0] idR13;
  logic [95:0] gzdLLziMainzicomputeR6;
  logic [7:0] callResR6;
  logic [1023:0] idR14;
  logic [511:0] idR15;
  logic [95:0] gzdLLziMainzicomputeR7;
  logic [7:0] callResR7;
  logic [64:0] gzdLLziMainziloop4;
  logic [64:0] gzdLLziMainziloop;
  logic [0:0] __continue;
  logic [1055:0] __resumption_tag;
  logic [1055:0] __resumption_tag_next;
  assign gMainziloop = __resumption_tag;
  assign gzdLLziMainziloop2 = gMainziloop[1055:0];
  assign gMainzicompute = {gzdLLziMainziloop2[1055:32], gzdLLziMainziloop2[31:0]};
  assign id = gMainzicompute[1055:32];
  assign idR1 = id[1023:512];
  assign gzdLLziMainzicompute = {gMainzicompute[31:0], idR1[511:448]};
  zdLLziMainzicompute  zdLLziMainzicompute (gzdLLziMainzicompute[95:64], gzdLLziMainzicompute[63:0], callRes);
  assign idR2 = gMainzicompute[1055:32];
  assign idR3 = idR2[1023:512];
  assign gzdLLziMainzicomputeR1 = {gMainzicompute[31:0], idR3[447:384]};
  zdLLziMainzicompute  zdLLziMainzicomputeR1 (gzdLLziMainzicomputeR1[95:64], gzdLLziMainzicomputeR1[63:0], callResR1);
  assign idR4 = gMainzicompute[1055:32];
  assign idR5 = idR4[1023:512];
  assign gzdLLziMainzicomputeR2 = {gMainzicompute[31:0], idR5[383:320]};
  zdLLziMainzicompute  zdLLziMainzicomputeR2 (gzdLLziMainzicomputeR2[95:64], gzdLLziMainzicomputeR2[63:0], callResR2);
  assign idR6 = gMainzicompute[1055:32];
  assign idR7 = idR6[1023:512];
  assign gzdLLziMainzicomputeR3 = {gMainzicompute[31:0], idR7[319:256]};
  zdLLziMainzicompute  zdLLziMainzicomputeR3 (gzdLLziMainzicomputeR3[95:64], gzdLLziMainzicomputeR3[63:0], callResR3);
  assign idR8 = gMainzicompute[1055:32];
  assign idR9 = idR8[1023:512];
  assign gzdLLziMainzicomputeR4 = {gMainzicompute[31:0], idR9[255:192]};
  zdLLziMainzicompute  zdLLziMainzicomputeR4 (gzdLLziMainzicomputeR4[95:64], gzdLLziMainzicomputeR4[63:0], callResR4);
  assign idR10 = gMainzicompute[1055:32];
  assign idR11 = idR10[1023:512];
  assign gzdLLziMainzicomputeR5 = {gMainzicompute[31:0], idR11[191:128]};
  zdLLziMainzicompute  zdLLziMainzicomputeR5 (gzdLLziMainzicomputeR5[95:64], gzdLLziMainzicomputeR5[63:0], callResR5);
  assign idR12 = gMainzicompute[1055:32];
  assign idR13 = idR12[1023:512];
  assign gzdLLziMainzicomputeR6 = {gMainzicompute[31:0], idR13[127:64]};
  zdLLziMainzicompute  zdLLziMainzicomputeR6 (gzdLLziMainzicomputeR6[95:64], gzdLLziMainzicomputeR6[63:0], callResR6);
  assign idR14 = gMainzicompute[1055:32];
  assign idR15 = idR14[1023:512];
  assign gzdLLziMainzicomputeR7 = {gMainzicompute[31:0], idR15[63:0]};
  zdLLziMainzicompute  zdLLziMainzicomputeR7 (gzdLLziMainzicomputeR7[95:64], gzdLLziMainzicomputeR7[63:0], callResR7);
  assign gzdLLziMainziloop4 = {1'h0, {callRes, callResR1, callResR2, callResR3, callResR4, callResR5, callResR6, callResR7}};
  assign gzdLLziMainziloop = gzdLLziMainziloop4[64:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, gzdLLziMainziloop[63:0]};
  initial __resumption_tag <= {992'h00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001, {7'h40{1'h0}}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= {992'h00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001, {7'h40{1'h0}}};
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLziMainzicompute (input logic [31:0] arg0,
  input logic [63:0] arg1,
  output logic [7:0] res);
  logic [31:0] id;
  logic [63:0] idR1;
  logic [15:0] binOp;
  assign id = arg0;
  assign idR1 = arg1;
  assign binOp = {id[7:0], idR1[39:32]};
  assign res = binOp[15:8] + binOp[7:0];
endmodule