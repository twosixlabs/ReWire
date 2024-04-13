module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [20:0] gzdLLzicase7138;
  logic [28:0] callRes;
  logic [20:0] gzdLLzicase7140;
  logic [28:0] callResR1;
  logic [20:0] gzdLLzicase7142;
  logic [28:0] callResR2;
  logic [20:0] gzdLLzicase7144;
  logic [28:0] callResR3;
  logic [20:0] gzdLLzicase7146;
  logic [28:0] callResR4;
  logic [20:0] gzdLLzicase7148;
  logic [28:0] callResR5;
  logic [20:0] gzdLLzicase7150;
  logic [28:0] callResR6;
  logic [20:0] gzdLLzicase7152;
  logic [28:0] callResR7;
  logic [20:0] gzdLLzicase7154;
  logic [28:0] callResR8;
  logic [20:0] gzdLLzicase7156;
  logic [28:0] callResR9;
  logic [20:0] gzdLLzicase7158;
  logic [28:0] callResR10;
  logic [20:0] gzdLLzicase7160;
  logic [28:0] callResR11;
  logic [20:0] gzdLLzicase7162;
  logic [28:0] callResR12;
  logic [20:0] gzdLLzicase7164;
  logic [28:0] callResR13;
  logic [20:0] gzdLLzicase7166;
  logic [28:0] callResR14;
  logic [20:0] gzdLLzicase7170;
  logic [28:0] callResR15;
  logic [0:0] __continue;
  logic [19:0] __resumption_tag;
  logic [19:0] __resumption_tag_next;
  assign gzdLLzicase7138 = {__resumption_tag, __in0};
  zdLLzicase7138  zdLLzicase7138 (gzdLLzicase7138[0], callRes);
  assign gzdLLzicase7140 = {__resumption_tag, __in0};
  zdLLzicase7140  zdLLzicase7140 (gzdLLzicase7140[0], callResR1);
  assign gzdLLzicase7142 = {__resumption_tag, __in0};
  zdLLzicase7142  zdLLzicase7142 (gzdLLzicase7142[0], callResR2);
  assign gzdLLzicase7144 = {__resumption_tag, __in0};
  zdLLzicase7144  zdLLzicase7144 (gzdLLzicase7144[0], callResR3);
  assign gzdLLzicase7146 = {__resumption_tag, __in0};
  zdLLzicase7146  zdLLzicase7146 (gzdLLzicase7146[0], callResR4);
  assign gzdLLzicase7148 = {__resumption_tag, __in0};
  zdLLzicase7148  zdLLzicase7148 (gzdLLzicase7148[0], callResR5);
  assign gzdLLzicase7150 = {__resumption_tag, __in0};
  zdLLzicase7150  zdLLzicase7150 (gzdLLzicase7150[0], callResR6);
  assign gzdLLzicase7152 = {__resumption_tag, __in0};
  zdLLzicase7152  zdLLzicase7152 (gzdLLzicase7152[0], callResR7);
  assign gzdLLzicase7154 = {__resumption_tag, __in0};
  zdLLzicase7154  zdLLzicase7154 (gzdLLzicase7154[0], callResR8);
  assign gzdLLzicase7156 = {__resumption_tag, __in0};
  zdLLzicase7156  zdLLzicase7156 (gzdLLzicase7156[0], callResR9);
  assign gzdLLzicase7158 = {__resumption_tag, __in0};
  zdLLzicase7158  zdLLzicase7158 (gzdLLzicase7158[0], callResR10);
  assign gzdLLzicase7160 = {__resumption_tag, __in0};
  zdLLzicase7160  zdLLzicase7160 (gzdLLzicase7160[0], callResR11);
  assign gzdLLzicase7162 = {__resumption_tag, __in0};
  zdLLzicase7162  zdLLzicase7162 (gzdLLzicase7162[0], callResR12);
  assign gzdLLzicase7164 = {__resumption_tag, __in0};
  zdLLzicase7164  zdLLzicase7164 (gzdLLzicase7164[0], callResR13);
  assign gzdLLzicase7166 = {__resumption_tag, __in0};
  zdLLzicase7166  zdLLzicase7166 (gzdLLzicase7166[0], callResR14);
  assign gzdLLzicase7170 = {__resumption_tag, __in0};
  zdLLzicase7170  zdLLzicase7170 (gzdLLzicase7170[16:9], gzdLLzicase7170[8:1], gzdLLzicase7170[0], callResR15);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase7170[20:17] == 4'h0) ? callResR15 : ((gzdLLzicase7166[20:17] == 4'h1) ? callResR14 : ((gzdLLzicase7164[20:17] == 4'h2) ? callResR13 : ((gzdLLzicase7162[20:17] == 4'h3) ? callResR12 : ((gzdLLzicase7160[20:17] == 4'h4) ? callResR11 : ((gzdLLzicase7158[20:17] == 4'h5) ? callResR10 : ((gzdLLzicase7156[20:17] == 4'h6) ? callResR9 : ((gzdLLzicase7154[20:17] == 4'h7) ? callResR8 : ((gzdLLzicase7152[20:17] == 4'h8) ? callResR7 : ((gzdLLzicase7150[20:17] == 4'h9) ? callResR6 : ((gzdLLzicase7148[20:17] == 4'ha) ? callResR5 : ((gzdLLzicase7146[20:17] == 4'hb) ? callResR4 : ((gzdLLzicase7144[20:17] == 4'hc) ? callResR3 : ((gzdLLzicase7142[20:17] == 4'hd) ? callResR2 : ((gzdLLzicase7140[20:17] == 4'he) ? callResR1 : callRes))))))))))))));
  initial __resumption_tag <= 20'h10000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 20'h10000;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase7138 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7088;
  logic [0:0] lit;
  assign gzdLLzilambda7088 = arg0;
  assign lit = gzdLLzilambda7088[0];
  assign res = (lit[0] == 1'h1) ? 29'h10d00d15 : 29'h11501522;
endmodule

module zdLLzicase7140 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7084;
  logic [0:0] lit;
  assign gzdLLzilambda7084 = arg0;
  assign lit = gzdLLzilambda7084[0];
  assign res = (lit[0] == 1'h1) ? 29'h1080080d : 29'h10d00d15;
endmodule

module zdLLzicase7142 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7080;
  logic [0:0] lit;
  assign gzdLLzilambda7080 = arg0;
  assign lit = gzdLLzilambda7080[0];
  assign res = (lit[0] == 1'h1) ? 29'h108e0000 : 29'h10df0000;
endmodule

module zdLLzicase7144 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7076;
  logic [28:0] callRes;
  assign gzdLLzilambda7076 = arg0;
  zdLLzilambda7076  zdLLzilambda7076 (gzdLLzilambda7076[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase7146 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7076;
  logic [28:0] callRes;
  assign gzdLLzilambda7076 = arg0;
  zdLLzilambda7076  zdLLzilambda7076 (gzdLLzilambda7076[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase7148 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7072;
  logic [28:0] callRes;
  assign gzdLLzilambda7072 = arg0;
  zdLLzilambda7072  zdLLzilambda7072 (gzdLLzilambda7072[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase7150 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7072;
  logic [28:0] callRes;
  assign gzdLLzilambda7072 = arg0;
  zdLLzilambda7072  zdLLzilambda7072 (gzdLLzilambda7072[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase7152 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7068;
  logic [28:0] callRes;
  assign gzdLLzilambda7068 = arg0;
  zdLLzilambda7068  zdLLzilambda7068 (gzdLLzilambda7068[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase7154 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7068;
  logic [28:0] callRes;
  assign gzdLLzilambda7068 = arg0;
  zdLLzilambda7068  zdLLzilambda7068 (gzdLLzilambda7068[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase7156 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7064;
  logic [28:0] callRes;
  assign gzdLLzilambda7064 = arg0;
  zdLLzilambda7064  zdLLzilambda7064 (gzdLLzilambda7064[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase7158 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7064;
  logic [28:0] callRes;
  assign gzdLLzilambda7064 = arg0;
  zdLLzilambda7064  zdLLzilambda7064 (gzdLLzilambda7064[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase7160 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7060;
  logic [28:0] callRes;
  assign gzdLLzilambda7060 = arg0;
  zdLLzilambda7060  zdLLzilambda7060 (gzdLLzilambda7060[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase7162 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7060;
  logic [28:0] callRes;
  assign gzdLLzilambda7060 = arg0;
  zdLLzilambda7060  zdLLzilambda7060 (gzdLLzilambda7060[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase7164 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7056;
  logic [28:0] callRes;
  assign gzdLLzilambda7056 = arg0;
  zdLLzilambda7056  zdLLzilambda7056 (gzdLLzilambda7056[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase7166 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda7056;
  logic [28:0] callRes;
  assign gzdLLzilambda7056 = arg0;
  zdLLzilambda7056  zdLLzilambda7056 (gzdLLzilambda7056[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase7170 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [0:0] arg2,
  output logic [28:0] res);
  logic [16:0] gzdLLzilambda7054;
  logic [16:0] gzdLLzicase7173;
  logic [15:0] gzdLLzicase7047;
  logic [15:0] binOp;
  logic [15:0] gMainzifib;
  logic [28:0] callRes;
  logic [16:0] gzdLLzicase7050;
  logic [15:0] gMainzifibR1;
  logic [28:0] callResR1;
  assign gzdLLzilambda7054 = {arg0, arg1, arg2};
  assign gzdLLzicase7173 = {gzdLLzilambda7054[0], gzdLLzilambda7054[8:1], gzdLLzilambda7054[16:9]};
  assign gzdLLzicase7047 = {gzdLLzicase7173[15:8], gzdLLzicase7173[7:0]};
  assign binOp = {gzdLLzicase7047[7:0], gzdLLzicase7047[15:8]};
  assign gMainzifib = {gzdLLzicase7047[15:8], binOp[15:8] + binOp[7:0]};
  Mainzifib  Mainzifib (gMainzifib[15:8], gMainzifib[7:0], callRes);
  assign gzdLLzicase7050 = {gzdLLzilambda7054[0], gzdLLzilambda7054[16:9], gzdLLzilambda7054[8:1]};
  assign gMainzifibR1 = {gzdLLzicase7050[15:8], gzdLLzicase7050[7:0]};
  Mainzifib  MainzifibR1 (gMainzifibR1[15:8], gMainzifibR1[7:0], callResR1);
  assign res = (gzdLLzicase7050[16] == 1'h1) ? callResR1 : callRes;
endmodule

module zdLLzilambda7056 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10020000 : 29'h10130000;
endmodule

module zdLLzilambda7060 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10140000 : 29'h10150000;
endmodule

module zdLLzilambda7064 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10160000 : 29'h10270000;
endmodule

module zdLLzilambda7068 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10280000 : 29'h10390000;
endmodule

module zdLLzilambda7072 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h103a0000 : 29'h105b0000;
endmodule

module zdLLzilambda7076 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h105c0000 : 29'h108d0000;
endmodule

module Mainzifib (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [28:0] res);
  assign res = {1'h1, arg0, 4'h0, arg0, arg1};
endmodule