module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [20:0] __in0,
  output logic [4:0] __out0);
  logic [22:0] gzdLLzicase5692;
  logic [7:0] callRes;
  logic [22:0] gzdLLzicase5692R1;
  logic [7:0] callResR1;
  logic [22:0] gzdLLzicase5692R2;
  logic [7:0] callResR2;
  logic [0:0] __continue;
  logic [1:0] __resumption_tag;
  logic [1:0] __resumption_tag_next;
  assign gzdLLzicase5692 = {__resumption_tag, __in0};
  zdLLzicase5692  zdLLzicase5692 (gzdLLzicase5692[20:0], callRes);
  assign gzdLLzicase5692R1 = {__resumption_tag, __in0};
  zdLLzicase5692  zdLLzicase5692R1 (gzdLLzicase5692R1[20:0], callResR1);
  assign gzdLLzicase5692R2 = {__resumption_tag, __in0};
  zdLLzicase5692  zdLLzicase5692R2 (gzdLLzicase5692R2[20:0], callResR2);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase5692R2[22:21] == 2'h0) ? callResR2 : ((gzdLLzicase5692R1[22:21] == 2'h1) ? callResR1 : callRes);
  initial __resumption_tag <= 2'h2;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 2'h2;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase5671 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [1:0] res);
  assign res = {arg0, arg0};
endmodule

module zdLLzicase5692 (input logic [20:0] arg0,
  output logic [7:0] res);
  logic [20:0] gMainzidev;
  logic [20:0] gzdLLzicase5699;
  logic [19:0] gzdLLzicase5663;
  logic [19:0] gMainzicrossbar;
  logic [19:0] gzdLLzicase5655;
  logic [1:0] gMainziswitch3772;
  logic [1:0] callRes;
  logic [19:0] gzdLLzicase5634;
  logic [1:0] gMainziswitch3772R1;
  logic [1:0] callResR1;
  logic [19:0] gzdLLzicase5613;
  logic [1:0] gMainziswitch3772R2;
  logic [1:0] callResR2;
  logic [19:0] gzdLLzicase5592;
  logic [1:0] gMainziswitch3772R3;
  logic [1:0] callResR3;
  logic [19:0] gzdLLzicase5571;
  logic [2:0] gMainziswitch3650;
  logic [1:0] callResR4;
  logic [17:0] gzdLLzicase5550;
  logic [2:0] gMainziswitch3650R1;
  logic [1:0] callResR5;
  logic [16:0] gzdLLzicase5531;
  logic [2:0] gMainziswitch3650R2;
  logic [1:0] callResR6;
  logic [15:0] gzdLLzicase5513;
  logic [2:0] gMainziswitch3650R3;
  logic [1:0] callResR7;
  logic [14:0] gzdLLzicase5496;
  logic [2:0] gMainziswitch3650R4;
  logic [1:0] callResR8;
  logic [12:0] gzdLLzicase5480;
  logic [2:0] gMainziswitch3650R5;
  logic [1:0] callResR9;
  logic [11:0] gzdLLzicase5466;
  logic [2:0] gMainziswitch3650R6;
  logic [1:0] callResR10;
  logic [10:0] gzdLLzicase5453;
  logic [2:0] gMainziswitch3650R7;
  logic [1:0] callResR11;
  logic [9:0] gzdLLzicase5441;
  logic [2:0] gMainziswitch3650R8;
  logic [1:0] callResR12;
  logic [7:0] gzdLLzicase5430;
  logic [2:0] gMainziswitch3650R9;
  logic [1:0] callResR13;
  logic [6:0] gzdLLzicase5421;
  logic [2:0] gMainziswitch3650R10;
  logic [1:0] callResR14;
  logic [5:0] gzdLLzicase5413;
  logic [2:0] gMainziswitch3650R11;
  logic [1:0] callResR15;
  logic [4:0] gzdLLzicase5406;
  assign gMainzidev = arg0;
  assign gzdLLzicase5699 = gMainzidev[20:0];
  assign gzdLLzicase5663 = {gzdLLzicase5699[19:16], gzdLLzicase5699[15:0]};
  assign gMainzicrossbar = {gzdLLzicase5663[19:16], gzdLLzicase5663[15:0]};
  assign gzdLLzicase5655 = {gMainzicrossbar[19:16], gMainzicrossbar[15:0]};
  assign gMainziswitch3772 = {gzdLLzicase5655[16], gzdLLzicase5655[3]};
  Mainziswitch3772  Mainziswitch3772 (gMainziswitch3772[1], gMainziswitch3772[0], callRes);
  assign gzdLLzicase5634 = {callRes, gzdLLzicase5655[2], gzdLLzicase5655[1], gzdLLzicase5655[0], gzdLLzicase5655[17], gzdLLzicase5655[7], gzdLLzicase5655[6], gzdLLzicase5655[5], gzdLLzicase5655[4], gzdLLzicase5655[18], gzdLLzicase5655[11], gzdLLzicase5655[10], gzdLLzicase5655[9], gzdLLzicase5655[8], gzdLLzicase5655[19], gzdLLzicase5655[15], gzdLLzicase5655[14], gzdLLzicase5655[13], gzdLLzicase5655[12]};
  assign gMainziswitch3772R1 = {gzdLLzicase5634[19], gzdLLzicase5634[17]};
  Mainziswitch3772  Mainziswitch3772R1 (gMainziswitch3772R1[1], gMainziswitch3772R1[0], callResR1);
  assign gzdLLzicase5613 = {callResR1, gzdLLzicase5634[16], gzdLLzicase5634[15], gzdLLzicase5634[14], gzdLLzicase5634[18], gzdLLzicase5634[13], gzdLLzicase5634[12], gzdLLzicase5634[11], gzdLLzicase5634[10], gzdLLzicase5634[9], gzdLLzicase5634[8], gzdLLzicase5634[7], gzdLLzicase5634[6], gzdLLzicase5634[5], gzdLLzicase5634[4], gzdLLzicase5634[3], gzdLLzicase5634[2], gzdLLzicase5634[1], gzdLLzicase5634[0]};
  assign gMainziswitch3772R2 = {gzdLLzicase5613[19], gzdLLzicase5613[17]};
  Mainziswitch3772  Mainziswitch3772R2 (gMainziswitch3772R2[1], gMainziswitch3772R2[0], callResR2);
  assign gzdLLzicase5592 = {callResR2, gzdLLzicase5613[16], gzdLLzicase5613[15], gzdLLzicase5613[14], gzdLLzicase5613[13], gzdLLzicase5613[18], gzdLLzicase5613[12], gzdLLzicase5613[11], gzdLLzicase5613[10], gzdLLzicase5613[9], gzdLLzicase5613[8], gzdLLzicase5613[7], gzdLLzicase5613[6], gzdLLzicase5613[5], gzdLLzicase5613[4], gzdLLzicase5613[3], gzdLLzicase5613[2], gzdLLzicase5613[1], gzdLLzicase5613[0]};
  assign gMainziswitch3772R3 = {gzdLLzicase5592[19], gzdLLzicase5592[17]};
  Mainziswitch3772  Mainziswitch3772R3 (gMainziswitch3772R3[1], gMainziswitch3772R3[0], callResR3);
  assign gzdLLzicase5571 = {callResR3, gzdLLzicase5592[16], gzdLLzicase5592[15], gzdLLzicase5592[14], gzdLLzicase5592[13], gzdLLzicase5592[12], gzdLLzicase5592[18], gzdLLzicase5592[11], gzdLLzicase5592[10], gzdLLzicase5592[9], gzdLLzicase5592[8], gzdLLzicase5592[7], gzdLLzicase5592[6], gzdLLzicase5592[5], gzdLLzicase5592[4], gzdLLzicase5592[3], gzdLLzicase5592[2], gzdLLzicase5592[1], gzdLLzicase5592[0]};
  assign gMainziswitch3650 = {gzdLLzicase5571[17], gzdLLzicase5571[16], gzdLLzicase5571[15]};
  Mainziswitch3650  Mainziswitch3650 (gMainziswitch3650[2], gMainziswitch3650[1], gMainziswitch3650[0], callResR4);
  assign gzdLLzicase5550 = {callResR4, gzdLLzicase5571[14], gzdLLzicase5571[13], gzdLLzicase5571[12], gzdLLzicase5571[11], gzdLLzicase5571[18], gzdLLzicase5571[10], gzdLLzicase5571[9], gzdLLzicase5571[8], gzdLLzicase5571[7], gzdLLzicase5571[6], gzdLLzicase5571[5], gzdLLzicase5571[4], gzdLLzicase5571[3], gzdLLzicase5571[2], gzdLLzicase5571[1], gzdLLzicase5571[0]};
  assign gMainziswitch3650R1 = {gzdLLzicase5550[17], gzdLLzicase5550[15], gzdLLzicase5550[14]};
  Mainziswitch3650  Mainziswitch3650R1 (gMainziswitch3650R1[2], gMainziswitch3650R1[1], gMainziswitch3650R1[0], callResR5);
  assign gzdLLzicase5531 = {callResR5, gzdLLzicase5550[13], gzdLLzicase5550[12], gzdLLzicase5550[11], gzdLLzicase5550[10], gzdLLzicase5550[9], gzdLLzicase5550[16], gzdLLzicase5550[8], gzdLLzicase5550[7], gzdLLzicase5550[6], gzdLLzicase5550[5], gzdLLzicase5550[4], gzdLLzicase5550[3], gzdLLzicase5550[2], gzdLLzicase5550[1], gzdLLzicase5550[0]};
  assign gMainziswitch3650R2 = {gzdLLzicase5531[16], gzdLLzicase5531[14], gzdLLzicase5531[13]};
  Mainziswitch3650  Mainziswitch3650R2 (gMainziswitch3650R2[2], gMainziswitch3650R2[1], gMainziswitch3650R2[0], callResR6);
  assign gzdLLzicase5513 = {callResR6, gzdLLzicase5531[12], gzdLLzicase5531[11], gzdLLzicase5531[10], gzdLLzicase5531[9], gzdLLzicase5531[8], gzdLLzicase5531[15], gzdLLzicase5531[7], gzdLLzicase5531[6], gzdLLzicase5531[5], gzdLLzicase5531[4], gzdLLzicase5531[3], gzdLLzicase5531[2], gzdLLzicase5531[1], gzdLLzicase5531[0]};
  assign gMainziswitch3650R3 = {gzdLLzicase5513[15], gzdLLzicase5513[13], gzdLLzicase5513[12]};
  Mainziswitch3650  Mainziswitch3650R3 (gMainziswitch3650R3[2], gMainziswitch3650R3[1], gMainziswitch3650R3[0], callResR7);
  assign gzdLLzicase5496 = {callResR7, gzdLLzicase5513[11], gzdLLzicase5513[10], gzdLLzicase5513[9], gzdLLzicase5513[8], gzdLLzicase5513[7], gzdLLzicase5513[14], gzdLLzicase5513[6], gzdLLzicase5513[5], gzdLLzicase5513[4], gzdLLzicase5513[3], gzdLLzicase5513[2], gzdLLzicase5513[1], gzdLLzicase5513[0]};
  assign gMainziswitch3650R4 = {gzdLLzicase5496[12], gzdLLzicase5496[11], gzdLLzicase5496[10]};
  Mainziswitch3650  Mainziswitch3650R4 (gMainziswitch3650R4[2], gMainziswitch3650R4[1], gMainziswitch3650R4[0], callResR8);
  assign gzdLLzicase5480 = {callResR8, gzdLLzicase5496[9], gzdLLzicase5496[8], gzdLLzicase5496[7], gzdLLzicase5496[6], gzdLLzicase5496[13], gzdLLzicase5496[5], gzdLLzicase5496[4], gzdLLzicase5496[3], gzdLLzicase5496[2], gzdLLzicase5496[1], gzdLLzicase5496[0]};
  assign gMainziswitch3650R5 = {gzdLLzicase5480[12], gzdLLzicase5480[10], gzdLLzicase5480[9]};
  Mainziswitch3650  Mainziswitch3650R5 (gMainziswitch3650R5[2], gMainziswitch3650R5[1], gMainziswitch3650R5[0], callResR9);
  assign gzdLLzicase5466 = {callResR9, gzdLLzicase5480[8], gzdLLzicase5480[7], gzdLLzicase5480[6], gzdLLzicase5480[5], gzdLLzicase5480[4], gzdLLzicase5480[11], gzdLLzicase5480[3], gzdLLzicase5480[2], gzdLLzicase5480[1], gzdLLzicase5480[0]};
  assign gMainziswitch3650R6 = {gzdLLzicase5466[11], gzdLLzicase5466[9], gzdLLzicase5466[8]};
  Mainziswitch3650  Mainziswitch3650R6 (gMainziswitch3650R6[2], gMainziswitch3650R6[1], gMainziswitch3650R6[0], callResR10);
  assign gzdLLzicase5453 = {callResR10, gzdLLzicase5466[7], gzdLLzicase5466[6], gzdLLzicase5466[5], gzdLLzicase5466[4], gzdLLzicase5466[3], gzdLLzicase5466[10], gzdLLzicase5466[2], gzdLLzicase5466[1], gzdLLzicase5466[0]};
  assign gMainziswitch3650R7 = {gzdLLzicase5453[10], gzdLLzicase5453[8], gzdLLzicase5453[7]};
  Mainziswitch3650  Mainziswitch3650R7 (gMainziswitch3650R7[2], gMainziswitch3650R7[1], gMainziswitch3650R7[0], callResR11);
  assign gzdLLzicase5441 = {callResR11, gzdLLzicase5453[6], gzdLLzicase5453[5], gzdLLzicase5453[4], gzdLLzicase5453[3], gzdLLzicase5453[2], gzdLLzicase5453[9], gzdLLzicase5453[1], gzdLLzicase5453[0]};
  assign gMainziswitch3650R8 = {gzdLLzicase5441[7], gzdLLzicase5441[6], gzdLLzicase5441[5]};
  Mainziswitch3650  Mainziswitch3650R8 (gMainziswitch3650R8[2], gMainziswitch3650R8[1], gMainziswitch3650R8[0], callResR12);
  assign gzdLLzicase5430 = {callResR12, gzdLLzicase5441[4], gzdLLzicase5441[3], gzdLLzicase5441[2], gzdLLzicase5441[1], gzdLLzicase5441[8], gzdLLzicase5441[0]};
  assign gMainziswitch3650R9 = {gzdLLzicase5430[7], gzdLLzicase5430[5], gzdLLzicase5430[4]};
  Mainziswitch3650  Mainziswitch3650R9 (gMainziswitch3650R9[2], gMainziswitch3650R9[1], gMainziswitch3650R9[0], callResR13);
  assign gzdLLzicase5421 = {callResR13, gzdLLzicase5430[3], gzdLLzicase5430[2], gzdLLzicase5430[1], gzdLLzicase5430[0], gzdLLzicase5430[6]};
  assign gMainziswitch3650R10 = {gzdLLzicase5421[6], gzdLLzicase5421[4], gzdLLzicase5421[3]};
  Mainziswitch3650  Mainziswitch3650R10 (gMainziswitch3650R10[2], gMainziswitch3650R10[1], gMainziswitch3650R10[0], callResR14);
  assign gzdLLzicase5413 = {callResR14, gzdLLzicase5421[2], gzdLLzicase5421[1], gzdLLzicase5421[0], gzdLLzicase5421[5]};
  assign gMainziswitch3650R11 = {gzdLLzicase5413[5], gzdLLzicase5413[3], gzdLLzicase5413[2]};
  Mainziswitch3650  Mainziswitch3650R11 (gMainziswitch3650R11[2], gMainziswitch3650R11[1], gMainziswitch3650R11[0], callResR15);
  assign gzdLLzicase5406 = {callResR15, gzdLLzicase5413[1], gzdLLzicase5413[0], gzdLLzicase5413[4]};
  assign res = (gzdLLzicase5699[20] == 1'h0) ? {2'h2, {gzdLLzicase5406[2], gzdLLzicase5406[1], gzdLLzicase5406[0], gzdLLzicase5406[3]}, 2'h0} : 8'hc1;
endmodule

module Mainziswitch3650 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  input logic [0:0] arg2,
  output logic [1:0] res);
  logic [2:0] id;
  logic [2:0] gzdLLzicase5671;
  logic [1:0] callRes;
  assign id = {arg0, arg1, arg2};
  assign gzdLLzicase5671 = {arg0, arg1, arg2};
  zdLLzicase5671  zdLLzicase5671 (gzdLLzicase5671[2], gzdLLzicase5671[1], callRes);
  assign res = (gzdLLzicase5671[0] == 1'h1) ? callRes : {id[2], id[1]};
endmodule

module Mainziswitch3772 (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [1:0] res);
  logic [2:0] id;
  logic [2:0] gzdLLzicase5671;
  logic [1:0] callRes;
  assign id = {arg0, 1'h0, arg1};
  assign gzdLLzicase5671 = {arg0, 1'h0, arg1};
  zdLLzicase5671  zdLLzicase5671 (gzdLLzicase5671[2], gzdLLzicase5671[1], callRes);
  assign res = (gzdLLzicase5671[0] == 1'h1) ? callRes : {id[2], id[1]};
endmodule