module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [1023:0] __in0,
  input logic [31:0] __in1,
  output logic [63:0] __out0);
  logic [1056:0] gzdLLzicase8530;
  logic [65:0] callRes;
  logic [1056:0] gzdLLzicase8530R1;
  logic [65:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLzicase8530 = {__resumption_tag, {__in0, __in1}};
  zdLLzicase8530  zdLLzicase8530 (gzdLLzicase8530[1055:0], callRes);
  assign gzdLLzicase8530R1 = {__resumption_tag, {__in0, __in1}};
  zdLLzicase8530  zdLLzicase8530R1 (gzdLLzicase8530R1[1055:0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase8530R1[1056] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase8530 (input logic [1055:0] arg0,
  output logic [65:0] res);
  logic [1055:0] gMainziloop;
  logic [1055:0] gzdLLzicase8504;
  logic [1055:0] gMainzicompute;
  logic [1023:0] id;
  logic [511:0] idR1;
  logic [95:0] gzdLLzilambda8497;
  logic [7:0] callRes;
  logic [1023:0] idR2;
  logic [511:0] idR3;
  logic [95:0] gzdLLzilambda8497R1;
  logic [7:0] callResR1;
  logic [1023:0] idR4;
  logic [511:0] idR5;
  logic [95:0] gzdLLzilambda8497R2;
  logic [7:0] callResR2;
  logic [1023:0] idR6;
  logic [511:0] idR7;
  logic [95:0] gzdLLzilambda8497R3;
  logic [7:0] callResR3;
  logic [1023:0] idR8;
  logic [511:0] idR9;
  logic [95:0] gzdLLzilambda8497R4;
  logic [7:0] callResR4;
  logic [1023:0] idR10;
  logic [511:0] idR11;
  logic [95:0] gzdLLzilambda8497R5;
  logic [7:0] callResR5;
  logic [1023:0] idR12;
  logic [511:0] idR13;
  logic [95:0] gzdLLzilambda8497R6;
  logic [7:0] callResR6;
  logic [1023:0] idR14;
  logic [511:0] idR15;
  logic [95:0] gzdLLzilambda8497R7;
  logic [7:0] callResR7;
  logic [65:0] gzdLLzilambda8536;
  logic [65:0] gzdLLzicase8534;
  logic [63:0] gzdLLzilambda8501;
  assign gMainziloop = arg0;
  assign gzdLLzicase8504 = gMainziloop[1055:0];
  assign gMainzicompute = {gzdLLzicase8504[1055:32], gzdLLzicase8504[31:0]};
  assign id = gMainzicompute[1055:32];
  assign idR1 = id[1023:512];
  assign gzdLLzilambda8497 = {gMainzicompute[31:0], idR1[511:448]};
  zdLLzilambda8497  zdLLzilambda8497 (gzdLLzilambda8497[95:64], gzdLLzilambda8497[63:0], callRes);
  assign idR2 = gMainzicompute[1055:32];
  assign idR3 = idR2[1023:512];
  assign gzdLLzilambda8497R1 = {gMainzicompute[31:0], idR3[447:384]};
  zdLLzilambda8497  zdLLzilambda8497R1 (gzdLLzilambda8497R1[95:64], gzdLLzilambda8497R1[63:0], callResR1);
  assign idR4 = gMainzicompute[1055:32];
  assign idR5 = idR4[1023:512];
  assign gzdLLzilambda8497R2 = {gMainzicompute[31:0], idR5[383:320]};
  zdLLzilambda8497  zdLLzilambda8497R2 (gzdLLzilambda8497R2[95:64], gzdLLzilambda8497R2[63:0], callResR2);
  assign idR6 = gMainzicompute[1055:32];
  assign idR7 = idR6[1023:512];
  assign gzdLLzilambda8497R3 = {gMainzicompute[31:0], idR7[319:256]};
  zdLLzilambda8497  zdLLzilambda8497R3 (gzdLLzilambda8497R3[95:64], gzdLLzilambda8497R3[63:0], callResR3);
  assign idR8 = gMainzicompute[1055:32];
  assign idR9 = idR8[1023:512];
  assign gzdLLzilambda8497R4 = {gMainzicompute[31:0], idR9[255:192]};
  zdLLzilambda8497  zdLLzilambda8497R4 (gzdLLzilambda8497R4[95:64], gzdLLzilambda8497R4[63:0], callResR4);
  assign idR10 = gMainzicompute[1055:32];
  assign idR11 = idR10[1023:512];
  assign gzdLLzilambda8497R5 = {gMainzicompute[31:0], idR11[191:128]};
  zdLLzilambda8497  zdLLzilambda8497R5 (gzdLLzilambda8497R5[95:64], gzdLLzilambda8497R5[63:0], callResR5);
  assign idR12 = gMainzicompute[1055:32];
  assign idR13 = idR12[1023:512];
  assign gzdLLzilambda8497R6 = {gMainzicompute[31:0], idR13[127:64]};
  zdLLzilambda8497  zdLLzilambda8497R6 (gzdLLzilambda8497R6[95:64], gzdLLzilambda8497R6[63:0], callResR6);
  assign idR14 = gMainzicompute[1055:32];
  assign idR15 = idR14[1023:512];
  assign gzdLLzilambda8497R7 = {gMainzicompute[31:0], idR15[63:0]};
  zdLLzilambda8497  zdLLzilambda8497R7 (gzdLLzilambda8497R7[95:64], gzdLLzilambda8497R7[63:0], callResR7);
  assign gzdLLzilambda8536 = {2'h0, {callRes, callResR1, callResR2, callResR3, callResR4, callResR5, callResR6, callResR7}};
  assign gzdLLzicase8534 = gzdLLzilambda8536[65:0];
  assign gzdLLzilambda8501 = gzdLLzicase8534[63:0];
  assign res = {1'h1, gzdLLzilambda8501[63:0], 1'h0};
endmodule

module zdLLzilambda8497 (input logic [31:0] arg0,
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