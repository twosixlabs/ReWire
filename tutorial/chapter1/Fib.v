module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [7:0] __out0);
  logic [20:0] gzdLLzicase6941;
  logic [28:0] callRes;
  logic [20:0] gzdLLzicase6943;
  logic [28:0] callResR1;
  logic [20:0] gzdLLzicase6945;
  logic [28:0] callResR2;
  logic [20:0] gzdLLzicase6947;
  logic [28:0] callResR3;
  logic [20:0] gzdLLzicase6949;
  logic [28:0] callResR4;
  logic [20:0] gzdLLzicase6951;
  logic [28:0] callResR5;
  logic [20:0] gzdLLzicase6953;
  logic [28:0] callResR6;
  logic [20:0] gzdLLzicase6955;
  logic [28:0] callResR7;
  logic [20:0] gzdLLzicase6957;
  logic [28:0] callResR8;
  logic [20:0] gzdLLzicase6959;
  logic [28:0] callResR9;
  logic [20:0] gzdLLzicase6961;
  logic [28:0] callResR10;
  logic [20:0] gzdLLzicase6963;
  logic [28:0] callResR11;
  logic [20:0] gzdLLzicase6965;
  logic [28:0] callResR12;
  logic [20:0] gzdLLzicase6967;
  logic [28:0] callResR13;
  logic [20:0] gzdLLzicase6969;
  logic [28:0] callResR14;
  logic [20:0] gzdLLzicase6973;
  logic [28:0] callResR15;
  logic [0:0] __continue;
  logic [19:0] __resumption_tag;
  logic [19:0] __resumption_tag_next;
  assign gzdLLzicase6941 = {__resumption_tag, __in0};
  zdLLzicase6941  zdLLzicase6941 (gzdLLzicase6941[0], callRes);
  assign gzdLLzicase6943 = {__resumption_tag, __in0};
  zdLLzicase6943  zdLLzicase6943 (gzdLLzicase6943[0], callResR1);
  assign gzdLLzicase6945 = {__resumption_tag, __in0};
  zdLLzicase6945  zdLLzicase6945 (gzdLLzicase6945[0], callResR2);
  assign gzdLLzicase6947 = {__resumption_tag, __in0};
  zdLLzicase6947  zdLLzicase6947 (gzdLLzicase6947[0], callResR3);
  assign gzdLLzicase6949 = {__resumption_tag, __in0};
  zdLLzicase6949  zdLLzicase6949 (gzdLLzicase6949[0], callResR4);
  assign gzdLLzicase6951 = {__resumption_tag, __in0};
  zdLLzicase6951  zdLLzicase6951 (gzdLLzicase6951[0], callResR5);
  assign gzdLLzicase6953 = {__resumption_tag, __in0};
  zdLLzicase6953  zdLLzicase6953 (gzdLLzicase6953[0], callResR6);
  assign gzdLLzicase6955 = {__resumption_tag, __in0};
  zdLLzicase6955  zdLLzicase6955 (gzdLLzicase6955[0], callResR7);
  assign gzdLLzicase6957 = {__resumption_tag, __in0};
  zdLLzicase6957  zdLLzicase6957 (gzdLLzicase6957[0], callResR8);
  assign gzdLLzicase6959 = {__resumption_tag, __in0};
  zdLLzicase6959  zdLLzicase6959 (gzdLLzicase6959[0], callResR9);
  assign gzdLLzicase6961 = {__resumption_tag, __in0};
  zdLLzicase6961  zdLLzicase6961 (gzdLLzicase6961[0], callResR10);
  assign gzdLLzicase6963 = {__resumption_tag, __in0};
  zdLLzicase6963  zdLLzicase6963 (gzdLLzicase6963[0], callResR11);
  assign gzdLLzicase6965 = {__resumption_tag, __in0};
  zdLLzicase6965  zdLLzicase6965 (gzdLLzicase6965[0], callResR12);
  assign gzdLLzicase6967 = {__resumption_tag, __in0};
  zdLLzicase6967  zdLLzicase6967 (gzdLLzicase6967[0], callResR13);
  assign gzdLLzicase6969 = {__resumption_tag, __in0};
  zdLLzicase6969  zdLLzicase6969 (gzdLLzicase6969[0], callResR14);
  assign gzdLLzicase6973 = {__resumption_tag, __in0};
  zdLLzicase6973  zdLLzicase6973 (gzdLLzicase6973[16:9], gzdLLzicase6973[8:1], gzdLLzicase6973[0], callResR15);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase6973[20:17] == 4'h0) ? callResR15 : ((gzdLLzicase6969[20:17] == 4'h1) ? callResR14 : ((gzdLLzicase6967[20:17] == 4'h2) ? callResR13 : ((gzdLLzicase6965[20:17] == 4'h3) ? callResR12 : ((gzdLLzicase6963[20:17] == 4'h4) ? callResR11 : ((gzdLLzicase6961[20:17] == 4'h5) ? callResR10 : ((gzdLLzicase6959[20:17] == 4'h6) ? callResR9 : ((gzdLLzicase6957[20:17] == 4'h7) ? callResR8 : ((gzdLLzicase6955[20:17] == 4'h8) ? callResR7 : ((gzdLLzicase6953[20:17] == 4'h9) ? callResR6 : ((gzdLLzicase6951[20:17] == 4'ha) ? callResR5 : ((gzdLLzicase6949[20:17] == 4'hb) ? callResR4 : ((gzdLLzicase6947[20:17] == 4'hc) ? callResR3 : ((gzdLLzicase6945[20:17] == 4'hd) ? callResR2 : ((gzdLLzicase6943[20:17] == 4'he) ? callResR1 : callRes))))))))))))));
  initial __resumption_tag <= 20'h10000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 20'h10000;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase6941 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6891;
  logic [0:0] lit;
  assign gzdLLzilambda6891 = arg0;
  assign lit = gzdLLzilambda6891[0];
  assign res = (lit[0] == 1'h1) ? 29'h1080080d : 29'h10d00d15;
endmodule

module zdLLzicase6943 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6887;
  logic [0:0] lit;
  assign gzdLLzilambda6887 = arg0;
  assign lit = gzdLLzilambda6887[0];
  assign res = (lit[0] == 1'h1) ? 29'h10d00d15 : 29'h11501522;
endmodule

module zdLLzicase6945 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6883;
  logic [0:0] lit;
  assign gzdLLzilambda6883 = arg0;
  assign lit = gzdLLzilambda6883[0];
  assign res = (lit[0] == 1'h1) ? 29'h108f0000 : 29'h10de0000;
endmodule

module zdLLzicase6947 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6879;
  logic [28:0] callRes;
  assign gzdLLzilambda6879 = arg0;
  zdLLzilambda6879  zdLLzilambda6879 (gzdLLzilambda6879[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase6949 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6879;
  logic [28:0] callRes;
  assign gzdLLzilambda6879 = arg0;
  zdLLzilambda6879  zdLLzilambda6879 (gzdLLzilambda6879[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase6951 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6875;
  logic [28:0] callRes;
  assign gzdLLzilambda6875 = arg0;
  zdLLzilambda6875  zdLLzilambda6875 (gzdLLzilambda6875[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase6953 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6875;
  logic [28:0] callRes;
  assign gzdLLzilambda6875 = arg0;
  zdLLzilambda6875  zdLLzilambda6875 (gzdLLzilambda6875[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase6955 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6871;
  logic [28:0] callRes;
  assign gzdLLzilambda6871 = arg0;
  zdLLzilambda6871  zdLLzilambda6871 (gzdLLzilambda6871[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase6957 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6871;
  logic [28:0] callRes;
  assign gzdLLzilambda6871 = arg0;
  zdLLzilambda6871  zdLLzilambda6871 (gzdLLzilambda6871[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase6959 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6865;
  logic [28:0] callRes;
  assign gzdLLzilambda6865 = arg0;
  zdLLzilambda6865  zdLLzilambda6865 (gzdLLzilambda6865[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase6961 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6863;
  logic [28:0] callRes;
  assign gzdLLzilambda6863 = arg0;
  zdLLzilambda6863  zdLLzilambda6863 (gzdLLzilambda6863[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase6963 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6865;
  logic [28:0] callRes;
  assign gzdLLzilambda6865 = arg0;
  zdLLzilambda6865  zdLLzilambda6865 (gzdLLzilambda6865[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase6965 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6863;
  logic [28:0] callRes;
  assign gzdLLzilambda6863 = arg0;
  zdLLzilambda6863  zdLLzilambda6863 (gzdLLzilambda6863[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase6967 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6859;
  logic [28:0] callRes;
  assign gzdLLzilambda6859 = arg0;
  zdLLzilambda6859  zdLLzilambda6859 (gzdLLzilambda6859[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase6969 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] gzdLLzilambda6859;
  logic [28:0] callRes;
  assign gzdLLzilambda6859 = arg0;
  zdLLzilambda6859  zdLLzilambda6859 (gzdLLzilambda6859[0], callRes);
  assign res = callRes;
endmodule

module zdLLzicase6973 (input logic [7:0] arg0,
  input logic [7:0] arg1,
  input logic [0:0] arg2,
  output logic [28:0] res);
  logic [16:0] gzdLLzilambda6857;
  logic [16:0] gzdLLzicase6976;
  logic [15:0] gzdLLzicase6850;
  logic [15:0] binOp;
  logic [15:0] gMainzifibgen;
  logic [28:0] callRes;
  logic [16:0] gzdLLzicase6853;
  logic [15:0] gMainzifibgenR1;
  logic [28:0] callResR1;
  assign gzdLLzilambda6857 = {arg0, arg1, arg2};
  assign gzdLLzicase6976 = {gzdLLzilambda6857[0], gzdLLzilambda6857[8:1], gzdLLzilambda6857[16:9]};
  assign gzdLLzicase6850 = {gzdLLzicase6976[15:8], gzdLLzicase6976[7:0]};
  assign binOp = {gzdLLzicase6850[7:0], gzdLLzicase6850[15:8]};
  assign gMainzifibgen = {gzdLLzicase6850[15:8], binOp[15:8] + binOp[7:0]};
  Mainzifibgen  Mainzifibgen (gMainzifibgen[15:8], gMainzifibgen[7:0], callRes);
  assign gzdLLzicase6853 = {gzdLLzilambda6857[0], gzdLLzilambda6857[16:9], gzdLLzilambda6857[8:1]};
  assign gMainzifibgenR1 = {gzdLLzicase6853[15:8], gzdLLzicase6853[7:0]};
  Mainzifibgen  MainzifibgenR1 (gMainzifibgenR1[15:8], gMainzifibgenR1[7:0], callResR1);
  assign res = (gzdLLzicase6853[16] == 1'h1) ? callResR1 : callRes;
endmodule

module zdLLzilambda6859 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10020000 : 29'h10130000;
endmodule

module zdLLzilambda6863 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10150000 : 29'h10140000;
endmodule

module zdLLzilambda6865 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10160000 : 29'h10270000;
endmodule

module zdLLzilambda6871 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h10280000 : 29'h10390000;
endmodule

module zdLLzilambda6875 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h103a0000 : 29'h105b0000;
endmodule

module zdLLzilambda6879 (input logic [0:0] arg0,
  output logic [28:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 29'h105c0000 : 29'h108d0000;
endmodule

module Mainzifibgen (input logic [7:0] arg0,
  input logic [7:0] arg1,
  output logic [28:0] res);
  assign res = {1'h1, arg0, 4'h0, arg0, arg1};
endmodule