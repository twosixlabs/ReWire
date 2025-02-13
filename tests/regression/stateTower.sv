module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] gMainzisig;
  logic [0:0] gReWireziPreludezinot;
  logic [0:0] callRes;
  logic [0:0] gReWireziPreludezinotR1;
  logic [0:0] callResR1;
  logic [2:0] gzdLLziMainzisig113;
  logic [1:0] gzdLLziMainzisig15;
  logic [4:0] gzdLLziMainzisig101;
  logic [4:0] gzdLLziMainzisig99;
  logic [1:0] gzdLLziMainzisig6;
  logic [2:0] gzdLLziMainzisig54;
  logic [2:0] gzdLLziMainzisig52;
  logic [2:0] gzdLLziMainzisig51;
  logic [4:0] gzdLLziMainzisig50;
  logic [4:0] gzdLLziMainzisig48;
  logic [2:0] gzdLLziMainzisig;
  logic [4:0] callResR2;
  logic [2:0] gzdLLziMainzisig112;
  logic [1:0] gzdLLziMainzisig14;
  logic [2:0] gzdLLziMainzisig98;
  logic [2:0] gzdLLziMainzisig21;
  logic [4:0] callResR3;
  logic [4:0] gzdLLziMainzisig94;
  logic [4:0] gzdLLziMainzisig92;
  logic [2:0] gzdLLziMainzisig4;
  logic [2:0] gzdLLziMainzisig47;
  logic [2:0] gzdLLziMainzisig21R1;
  logic [4:0] callResR4;
  logic [5:0] gzdLLziMainzisig43;
  logic [5:0] gzdLLziMainzisig42;
  logic [3:0] gzdLLziMainzisig41;
  logic [3:0] gzdLLziMainzisig40;
  logic [3:0] gzdLLziMainzisig3;
  logic [1:0] gzdLLziMainzisig39;
  logic [1:0] gzdLLziMainzisig28;
  logic [4:0] callResR5;
  logic [6:0] gzdLLziMainzisig35;
  logic [6:0] gzdLLziMainzisig33;
  logic [3:0] gzdLLziMainzisig32;
  logic [3:0] gzdLLziMainzisig2;
  logic [1:0] binOp;
  logic [0:0] msbit;
  logic [1:0] gzdLLziMainzisig31;
  logic [1:0] gzdLLziMainzisig28R1;
  logic [4:0] callResR6;
  logic [4:0] gzdLLziMainzisig27;
  logic [4:0] gzdLLziMainzisig25;
  logic [1:0] gzdLLziMainzisig1;
  logic [2:0] gzdLLziMainzisig24;
  logic [2:0] gzdLLziMainzisig21R2;
  logic [4:0] callResR7;
  logic [4:0] gzdLLziMainzisig20;
  logic [4:0] gzdLLziMainzisig18;
  logic [2:0] gzdLLziMainzisigR1;
  logic [4:0] callResR8;
  logic [0:0] __continue;
  logic [0:0] __padding;
  logic [0:0] __st0;
  logic [0:0] __st1;
  logic [0:0] __st0_next;
  logic [0:0] __st1_next;
  assign gMainzisig = {__in0, {__st0, __st1}};
  assign gReWireziPreludezinot = gMainzisig[2];
  ReWireziPreludezinot  ReWireziPreludezinot (gReWireziPreludezinot[0], callRes);
  assign gReWireziPreludezinotR1 = gMainzisig[2];
  ReWireziPreludezinot  ReWireziPreludezinotR1 (gReWireziPreludezinotR1[0], callResR1);
  assign gzdLLziMainzisig113 = {gMainzisig[1], gMainzisig[0], callResR1};
  assign gzdLLziMainzisig15 = {gzdLLziMainzisig113[2], gzdLLziMainzisig113[1]};
  assign gzdLLziMainzisig101 = {3'h0, gzdLLziMainzisig15[1], gzdLLziMainzisig15[0]};
  assign gzdLLziMainzisig99 = gzdLLziMainzisig101[4:0];
  assign gzdLLziMainzisig6 = {gzdLLziMainzisig99[1], gzdLLziMainzisig99[0]};
  assign gzdLLziMainzisig54 = {gzdLLziMainzisig6[1], gzdLLziMainzisig6[1], gzdLLziMainzisig6[0]};
  assign gzdLLziMainzisig52 = gzdLLziMainzisig54[2:0];
  assign gzdLLziMainzisig51 = {gzdLLziMainzisig52[1], gzdLLziMainzisig52[2], gzdLLziMainzisig52[0]};
  assign gzdLLziMainzisig50 = {2'h1, gzdLLziMainzisig51[1], gzdLLziMainzisig51[2], gzdLLziMainzisig51[0]};
  assign gzdLLziMainzisig48 = gzdLLziMainzisig50[4:0];
  assign gzdLLziMainzisig = {gzdLLziMainzisig48[2], gzdLLziMainzisig48[1], gzdLLziMainzisig48[0]};
  zdLLziMainzisig  zdLLziMainzisig (gzdLLziMainzisig[2], gzdLLziMainzisig[1], gzdLLziMainzisig[0], callResR2);
  assign gzdLLziMainzisig112 = {gMainzisig[1], gMainzisig[0], callRes};
  assign gzdLLziMainzisig14 = {gzdLLziMainzisig112[2], gzdLLziMainzisig112[1]};
  assign gzdLLziMainzisig98 = {gzdLLziMainzisig14[1], gzdLLziMainzisig14[1], gzdLLziMainzisig14[0]};
  assign gzdLLziMainzisig21 = gzdLLziMainzisig98[2:0];
  zdLLziMainzisig21  zdLLziMainzisig21 (gzdLLziMainzisig21[2], gzdLLziMainzisig21[1], gzdLLziMainzisig21[0], callResR3);
  assign gzdLLziMainzisig94 = callResR3;
  assign gzdLLziMainzisig92 = gzdLLziMainzisig94[4:0];
  assign gzdLLziMainzisig4 = {gzdLLziMainzisig92[2], gzdLLziMainzisig92[1], gzdLLziMainzisig92[0]};
  assign gzdLLziMainzisig47 = {gzdLLziMainzisig4[0], gzdLLziMainzisig4[1], gzdLLziMainzisig4[0]};
  assign gzdLLziMainzisig21R1 = gzdLLziMainzisig47[2:0];
  zdLLziMainzisig21  zdLLziMainzisig21R1 (gzdLLziMainzisig21R1[2], gzdLLziMainzisig21R1[1], gzdLLziMainzisig21R1[0], callResR4);
  assign gzdLLziMainzisig43 = {gzdLLziMainzisig4[2], callResR4};
  assign gzdLLziMainzisig42 = {gzdLLziMainzisig43[5], gzdLLziMainzisig43[4:0]};
  assign gzdLLziMainzisig41 = {gzdLLziMainzisig42[5], gzdLLziMainzisig42[2], gzdLLziMainzisig42[1], gzdLLziMainzisig42[0]};
  assign gzdLLziMainzisig40 = {gzdLLziMainzisig41[2], gzdLLziMainzisig41[3], gzdLLziMainzisig41[1], gzdLLziMainzisig41[0]};
  assign gzdLLziMainzisig3 = {gzdLLziMainzisig40[2], gzdLLziMainzisig40[3], gzdLLziMainzisig40[1], gzdLLziMainzisig40[0]};
  assign gzdLLziMainzisig39 = {gzdLLziMainzisig3[2], gzdLLziMainzisig3[0]};
  assign gzdLLziMainzisig28 = gzdLLziMainzisig39[1:0];
  zdLLziMainzisig28  zdLLziMainzisig28 (gzdLLziMainzisig28[1], gzdLLziMainzisig28[0], callResR5);
  assign gzdLLziMainzisig35 = {gzdLLziMainzisig3[2], gzdLLziMainzisig3[3], callResR5};
  assign gzdLLziMainzisig33 = {gzdLLziMainzisig35[6], gzdLLziMainzisig35[5], gzdLLziMainzisig35[4:0]};
  assign gzdLLziMainzisig32 = {gzdLLziMainzisig33[6], gzdLLziMainzisig33[5], gzdLLziMainzisig33[1], gzdLLziMainzisig33[0]};
  assign gzdLLziMainzisig2 = {gzdLLziMainzisig32[2], gzdLLziMainzisig32[3], gzdLLziMainzisig32[1], gzdLLziMainzisig32[0]};
  assign binOp = {gzdLLziMainzisig2[3], gzdLLziMainzisig2[2]};
  assign msbit = binOp[1] ^ binOp[0];
  assign gzdLLziMainzisig31 = {gzdLLziMainzisig2[1], msbit[0]};
  assign gzdLLziMainzisig28R1 = gzdLLziMainzisig31[1:0];
  zdLLziMainzisig28  zdLLziMainzisig28R1 (gzdLLziMainzisig28R1[1], gzdLLziMainzisig28R1[0], callResR6);
  assign gzdLLziMainzisig27 = callResR6;
  assign gzdLLziMainzisig25 = gzdLLziMainzisig27[4:0];
  assign gzdLLziMainzisig1 = {gzdLLziMainzisig25[1], gzdLLziMainzisig25[0]};
  assign gzdLLziMainzisig24 = {gzdLLziMainzisig1[1], gzdLLziMainzisig1[1], gzdLLziMainzisig1[0]};
  assign gzdLLziMainzisig21R2 = gzdLLziMainzisig24[2:0];
  zdLLziMainzisig21  zdLLziMainzisig21R2 (gzdLLziMainzisig21R2[2], gzdLLziMainzisig21R2[1], gzdLLziMainzisig21R2[0], callResR7);
  assign gzdLLziMainzisig20 = callResR7;
  assign gzdLLziMainzisig18 = gzdLLziMainzisig20[4:0];
  assign gzdLLziMainzisigR1 = {gzdLLziMainzisig18[2], gzdLLziMainzisig18[1], gzdLLziMainzisig18[0]};
  zdLLziMainzisig  zdLLziMainzisigR1 (gzdLLziMainzisigR1[2], gzdLLziMainzisigR1[1], gzdLLziMainzisigR1[0], callResR8);
  assign {__continue, __padding, __out0, __st0_next, __st1_next} = (gzdLLziMainzisig112[0] == 1'h1) ? callResR8 : callResR2;
  initial {__st0, __st1} <= 2'h3;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__st0, __st1} <= 2'h3;
    end else begin
      {__st0, __st1} <= {__st0_next, __st1_next};
    end
  end
endmodule

module zdLLziMainzisig (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  output logic [4:0] res);
  assign res = {2'h2, arg0, arg1, arg2};
endmodule

module zdLLziMainzisig21 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  output logic [4:0] res);
  assign res = {2'h1, arg0, arg1, arg2};
endmodule

module zdLLziMainzisig28 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [4:0] res);
  assign res = {3'h0, arg0, arg1};
endmodule

module ReWireziPreludezinot (input logic [0:0] arg0,
  output logic [0:0] res);
  logic [0:0] lit;
  assign lit = arg0;
  assign res = (lit[0] == 1'h1) ? 1'h0 : 1'h1;
endmodule