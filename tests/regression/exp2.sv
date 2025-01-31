module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [99:0] __in0,
  output logic [99:0] __out0);
  logic [2099:0] gzdLLzicase8373;
  logic [2099:0] gzdLLzilambda8339;
  logic [1099:0] gMainzisszq;
  logic [999:0] id;
  logic [999:0] idR1;
  logic [99:0] gMainzix2;
  logic [99:0] callRes;
  logic [999:0] idR2;
  logic [999:0] idR3;
  logic [99:0] gMainzix2R1;
  logic [99:0] callResR1;
  logic [999:0] idR4;
  logic [999:0] idR5;
  logic [99:0] gMainzix2R2;
  logic [99:0] callResR2;
  logic [999:0] idR6;
  logic [999:0] idR7;
  logic [99:0] gMainzix2R3;
  logic [99:0] callResR3;
  logic [999:0] idR8;
  logic [999:0] idR9;
  logic [99:0] gMainzix2R4;
  logic [99:0] callResR4;
  logic [999:0] idR10;
  logic [999:0] idR11;
  logic [99:0] gMainzix2R5;
  logic [99:0] callResR5;
  logic [999:0] idR12;
  logic [999:0] idR13;
  logic [99:0] gMainzix2R6;
  logic [99:0] callResR6;
  logic [999:0] idR14;
  logic [999:0] idR15;
  logic [99:0] gMainzix2R7;
  logic [99:0] callResR7;
  logic [999:0] idR16;
  logic [999:0] idR17;
  logic [99:0] gMainzix2R8;
  logic [99:0] callResR8;
  logic [99:0] gMainzix2R9;
  logic [99:0] callResR9;
  logic [999:0] gzdLLzicase8381;
  logic [2100:0] gzdLLzilambda8378;
  logic [2100:0] gzdLLzicase8376;
  logic [999:0] gMainzidev7907;
  logic [1999:0] gzdLLzilambda8393;
  logic [1999:0] gzdLLzicase8391;
  logic [2100:0] gzdLLzilambda8388;
  logic [2100:0] gzdLLzicase8386;
  logic [1999:0] gzdLLzilambda8341;
  logic [999:0] idR18;
  logic [0:0] __continue;
  logic [999:0] __resumption_tag;
  logic [999:0] __st0;
  logic [999:0] __resumption_tag_next;
  logic [999:0] __st0_next;
  assign gzdLLzicase8373 = {{__resumption_tag, __st0}, __in0};
  assign gzdLLzilambda8339 = {gzdLLzicase8373[2099:1100], gzdLLzicase8373[99:0], gzdLLzicase8373[1099:100]};
  assign gMainzisszq = {gzdLLzilambda8339[2099:1100], gzdLLzilambda8339[1099:1000]};
  assign id = gMainzisszq[1099:100];
  assign idR1 = {id[899:0], gMainzisszq[99:0]};
  assign gMainzix2 = idR1[999:900];
  Mainzix2  Mainzix2 (gMainzix2[99:0], callRes);
  assign idR2 = gMainzisszq[1099:100];
  assign idR3 = {idR2[899:0], gMainzisszq[99:0]};
  assign gMainzix2R1 = idR3[899:800];
  Mainzix2  Mainzix2R1 (gMainzix2R1[99:0], callResR1);
  assign idR4 = gMainzisszq[1099:100];
  assign idR5 = {idR4[899:0], gMainzisszq[99:0]};
  assign gMainzix2R2 = idR5[799:700];
  Mainzix2  Mainzix2R2 (gMainzix2R2[99:0], callResR2);
  assign idR6 = gMainzisszq[1099:100];
  assign idR7 = {idR6[899:0], gMainzisszq[99:0]};
  assign gMainzix2R3 = idR7[699:600];
  Mainzix2  Mainzix2R3 (gMainzix2R3[99:0], callResR3);
  assign idR8 = gMainzisszq[1099:100];
  assign idR9 = {idR8[899:0], gMainzisszq[99:0]};
  assign gMainzix2R4 = idR9[599:500];
  Mainzix2  Mainzix2R4 (gMainzix2R4[99:0], callResR4);
  assign idR10 = gMainzisszq[1099:100];
  assign idR11 = {idR10[899:0], gMainzisszq[99:0]};
  assign gMainzix2R5 = idR11[499:400];
  Mainzix2  Mainzix2R5 (gMainzix2R5[99:0], callResR5);
  assign idR12 = gMainzisszq[1099:100];
  assign idR13 = {idR12[899:0], gMainzisszq[99:0]};
  assign gMainzix2R6 = idR13[399:300];
  Mainzix2  Mainzix2R6 (gMainzix2R6[99:0], callResR6);
  assign idR14 = gMainzisszq[1099:100];
  assign idR15 = {idR14[899:0], gMainzisszq[99:0]};
  assign gMainzix2R7 = idR15[299:200];
  Mainzix2  Mainzix2R7 (gMainzix2R7[99:0], callResR7);
  assign idR16 = gMainzisszq[1099:100];
  assign idR17 = {idR16[899:0], gMainzisszq[99:0]};
  assign gMainzix2R8 = idR17[199:100];
  Mainzix2  Mainzix2R8 (gMainzix2R8[99:0], callResR8);
  assign gMainzix2R9 = gMainzisszq[99:0];
  Mainzix2  Mainzix2R9 (gMainzix2R9[99:0], callResR9);
  assign gzdLLzicase8381 = {callRes, callResR1, callResR2, callResR3, callResR4, callResR5, callResR6, callResR7, callResR8, callResR9};
  assign gzdLLzilambda8378 = {{101'h00000000000000000000000001, {10'h3e8{1'h0}}}, gzdLLzicase8381[999:0]};
  assign gzdLLzicase8376 = gzdLLzilambda8378[2100:0];
  assign gMainzidev7907 = gzdLLzicase8376[999:0];
  assign gzdLLzilambda8393 = {gMainzidev7907[999:0], gMainzidev7907[999:0]};
  assign gzdLLzicase8391 = gzdLLzilambda8393[1999:0];
  assign gzdLLzilambda8388 = {{7'h65{1'h0}}, gzdLLzicase8391[1999:1000], gzdLLzicase8391[999:0]};
  assign gzdLLzicase8386 = gzdLLzilambda8388[2100:0];
  assign gzdLLzilambda8341 = {gzdLLzicase8386[1999:1000], gzdLLzicase8386[999:0]};
  assign idR18 = gzdLLzilambda8341[1999:1000];
  assign {__continue, __out0, __resumption_tag_next, __st0_next} = {1'h1, idR18[999:900], gzdLLzilambda8341[1999:1000], gzdLLzilambda8341[999:0]};
  initial {__resumption_tag, __st0} <= {11'h7d0{1'h0}};
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= {11'h7d0{1'h0}};
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module Mainzix2 (input logic [99:0] arg0,
  output logic [99:0] res);
  logic [199:0] binOp;
  assign binOp = {arg0, 100'h0000000000000000000000002};
  assign res = binOp[199:100] * binOp[99:0];
endmodule