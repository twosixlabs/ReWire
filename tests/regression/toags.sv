module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [5:0] gzdLLziPurezidispatch10;
  logic [3:0] gzdLLziPurezidispatch9;
  logic [3:0] gzdLLziMainzigo1;
  logic [1:0] gzdLLziMainzigo19;
  logic [1:0] gzdLLziMainzigo16;
  logic [6:0] callRes;
  logic [6:0] gzdLLziMainzigo15;
  logic [6:0] gzdLLziMainzigo;
  logic [6:0] callResR1;
  logic [5:0] gzdLLziPurezidispatch6;
  logic [2:0] gzdLLziMainzigo10;
  logic [3:0] gzdLLziMainzigo30;
  logic [2:0] gzdLLziMainzigo29;
  logic [2:0] gzdLLziMainzigo12;
  logic [2:0] gzdLLziMainzigo38;
  logic [2:0] gzdLLziMainzigo35;
  logic [7:0] gzdLLziMainzigo34;
  logic [7:0] gzdLLziMainzigo31;
  logic [3:0] gzdLLziMainzigo7;
  logic [1:0] gzdLLziMainzigo4;
  logic [0:0] callResR2;
  logic [1:0] id;
  logic [1:0] gReWireziPreludezizaza;
  logic [0:0] callResR3;
  logic [3:0] gzdLLziMainzigo28;
  logic [2:0] gzdLLziMainzigo27;
  logic [2:0] gzdLLziMainzigo11;
  logic [5:0] gzdLLziPurezidispatch5;
  logic [3:0] gzdLLziPurezidispatch4;
  logic [3:0] gzdLLziMainzigo6;
  logic [1:0] gzdLLziMainzigo26;
  logic [1:0] gzdLLziMainzigo16R1;
  logic [6:0] callResR4;
  logic [6:0] gzdLLziMainzigo22;
  logic [6:0] gzdLLziMainzigoR1;
  logic [6:0] callResR5;
  logic [5:0] gzdLLziPurezidispatch2;
  logic [3:0] gzdLLziPurezidispatch1;
  logic [3:0] gzdLLziMainzigo2;
  logic [1:0] gzdLLziMainzigo4R1;
  logic [0:0] callResR6;
  logic [1:0] idR1;
  logic [1:0] gReWireziPreludezizazaR1;
  logic [0:0] callResR7;
  logic [0:0] __continue;
  logic [2:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __st1;
  logic [2:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  logic [0:0] __st1_next;
  assign gzdLLziPurezidispatch10 = {__in0, {__resumption_tag, __st0, __st1}};
  assign gzdLLziPurezidispatch9 = {gzdLLziPurezidispatch10[5], gzdLLziPurezidispatch10[2], gzdLLziPurezidispatch10[1], gzdLLziPurezidispatch10[0]};
  assign gzdLLziMainzigo1 = {gzdLLziPurezidispatch9[2], gzdLLziPurezidispatch9[3], gzdLLziPurezidispatch9[1], gzdLLziPurezidispatch9[0]};
  assign gzdLLziMainzigo19 = {gzdLLziMainzigo1[1], gzdLLziMainzigo1[3]};
  assign gzdLLziMainzigo16 = gzdLLziMainzigo19[1:0];
  zdLLziMainzigo16  zdLLziMainzigo16 (gzdLLziMainzigo16[1], gzdLLziMainzigo16[0], callRes);
  assign gzdLLziMainzigo15 = callRes;
  assign gzdLLziMainzigo = gzdLLziMainzigo15[6:0];
  zdLLziMainzigo  zdLLziMainzigo (gzdLLziMainzigo[1], gzdLLziMainzigo[0], callResR1);
  assign gzdLLziPurezidispatch6 = {__in0, {__resumption_tag, __st0, __st1}};
  assign gzdLLziMainzigo10 = {gzdLLziPurezidispatch6[5], gzdLLziPurezidispatch6[1], gzdLLziPurezidispatch6[0]};
  assign gzdLLziMainzigo30 = {gzdLLziMainzigo10[0], gzdLLziMainzigo10[1], gzdLLziMainzigo10[2], gzdLLziMainzigo10[2]};
  assign gzdLLziMainzigo29 = {gzdLLziMainzigo30[3], gzdLLziMainzigo30[2], gzdLLziMainzigo30[1]};
  assign gzdLLziMainzigo12 = {gzdLLziMainzigo29[0], gzdLLziMainzigo29[1], gzdLLziMainzigo29[2]};
  assign gzdLLziMainzigo38 = {gzdLLziMainzigo12[1], gzdLLziMainzigo12[1], gzdLLziMainzigo12[0]};
  assign gzdLLziMainzigo35 = gzdLLziMainzigo38[2:0];
  assign gzdLLziMainzigo34 = {gzdLLziMainzigo12[2], {4'h1, gzdLLziMainzigo35[2], gzdLLziMainzigo35[1], gzdLLziMainzigo35[0]}};
  assign gzdLLziMainzigo31 = {gzdLLziMainzigo34[7], gzdLLziMainzigo34[6:0]};
  assign gzdLLziMainzigo7 = {gzdLLziMainzigo31[7], gzdLLziMainzigo31[2], gzdLLziMainzigo31[1], gzdLLziMainzigo31[0]};
  assign gzdLLziMainzigo4 = {gzdLLziMainzigo7[3], gzdLLziMainzigo7[2]};
  zdLLziMainzigo4  zdLLziMainzigo4 (gzdLLziMainzigo4[1], callResR2);
  assign id = {gzdLLziMainzigo7[3], gzdLLziMainzigo7[2]};
  assign gReWireziPreludezizaza = {(id[0] == 1'h1) ? id[1] : callResR2, gzdLLziMainzigo7[2]};
  ReWireziPreludezizaza  ReWireziPreludezizaza (gReWireziPreludezizaza[1], gReWireziPreludezizaza[0], callResR3);
  assign gzdLLziMainzigo28 = {gzdLLziMainzigo10[0], gzdLLziMainzigo10[1], gzdLLziMainzigo10[2], gzdLLziMainzigo10[2]};
  assign gzdLLziMainzigo27 = {gzdLLziMainzigo28[3], gzdLLziMainzigo28[2], gzdLLziMainzigo28[1]};
  assign gzdLLziMainzigo11 = {gzdLLziMainzigo27[0], gzdLLziMainzigo27[1], gzdLLziMainzigo27[2]};
  assign gzdLLziPurezidispatch5 = {__in0, {__resumption_tag, __st0, __st1}};
  assign gzdLLziPurezidispatch4 = {gzdLLziPurezidispatch5[5], gzdLLziPurezidispatch5[2], gzdLLziPurezidispatch5[1], gzdLLziPurezidispatch5[0]};
  assign gzdLLziMainzigo6 = {gzdLLziPurezidispatch4[2], gzdLLziPurezidispatch4[3], gzdLLziPurezidispatch4[1], gzdLLziPurezidispatch4[0]};
  assign gzdLLziMainzigo26 = {gzdLLziMainzigo6[1], gzdLLziMainzigo6[3]};
  assign gzdLLziMainzigo16R1 = gzdLLziMainzigo26[1:0];
  zdLLziMainzigo16  zdLLziMainzigo16R1 (gzdLLziMainzigo16R1[1], gzdLLziMainzigo16R1[0], callResR4);
  assign gzdLLziMainzigo22 = callResR4;
  assign gzdLLziMainzigoR1 = gzdLLziMainzigo22[6:0];
  zdLLziMainzigo  zdLLziMainzigoR1 (gzdLLziMainzigoR1[1], gzdLLziMainzigoR1[0], callResR5);
  assign gzdLLziPurezidispatch2 = {__in0, {__resumption_tag, __st0, __st1}};
  assign gzdLLziPurezidispatch1 = {gzdLLziPurezidispatch2[5], gzdLLziPurezidispatch2[2], gzdLLziPurezidispatch2[1], gzdLLziPurezidispatch2[0]};
  assign gzdLLziMainzigo2 = {gzdLLziPurezidispatch1[2], gzdLLziPurezidispatch1[3], gzdLLziPurezidispatch1[1], gzdLLziPurezidispatch1[0]};
  assign gzdLLziMainzigo4R1 = {gzdLLziMainzigo2[3], gzdLLziMainzigo2[2]};
  zdLLziMainzigo4  zdLLziMainzigo4R1 (gzdLLziMainzigo4R1[1], callResR6);
  assign idR1 = {gzdLLziMainzigo2[3], gzdLLziMainzigo2[2]};
  assign gReWireziPreludezizazaR1 = {(idR1[0] == 1'h1) ? idR1[1] : callResR6, gzdLLziMainzigo2[2]};
  ReWireziPreludezizaza  ReWireziPreludezizazaR1 (gReWireziPreludezizazaR1[1], gReWireziPreludezizazaR1[0], callResR7);
  assign {__continue, __out0, __resumption_tag_next, __st0_next, __st1_next} = (gzdLLziPurezidispatch2[4:3] == 2'h1) ? {1'h1, callResR7, 2'h0, gzdLLziMainzigo2[2], gzdLLziMainzigo2[1], gzdLLziMainzigo2[0]} : ((gzdLLziPurezidispatch5[4:3] == 2'h2) ? callResR5 : ((gzdLLziPurezidispatch6[4:3] == 2'h3) ? ((gzdLLziMainzigo28[0] == 1'h1) ? {4'h9, gzdLLziMainzigo11[2], gzdLLziMainzigo11[1], gzdLLziMainzigo11[0]} : {1'h1, callResR3, 2'h2, gzdLLziMainzigo7[2], gzdLLziMainzigo7[1], gzdLLziMainzigo7[0]}) : callResR1));
  initial {__resumption_tag, __st0, __st1} <= 5'h19;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 5'h19;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module zdLLziMainzigo (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [6:0] res);
  logic [1:0] gMainzigo;
  assign gMainzigo = {arg0, arg1};
  assign res = {5'h16, gMainzigo[1], gMainzigo[0]};
endmodule

module zdLLziMainzigo4 (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [1:0] gReWireziPreludezizaza;
  logic [0:0] callRes;
  assign gReWireziPreludezizaza = {arg0, arg0};
  ReWireziPreludezizaza  ReWireziPreludezizaza (gReWireziPreludezizaza[1], gReWireziPreludezizaza[0], callRes);
  assign res = callRes;
endmodule

module zdLLziMainzigo16 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [6:0] res);
  assign res = {5'h00, arg0, arg1};
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