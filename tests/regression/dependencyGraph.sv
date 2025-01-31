module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [9:0] __in0,
  output logic [15:0] __out0);
  logic [40:0] gzdLLzicase10537;
  logic [77:0] callRes;
  logic [40:0] gzdLLzicase10537R1;
  logic [77:0] callResR1;
  logic [0:0] __continue;
  logic [29:0] __padding;
  logic [0:0] __resumption_tag;
  logic [29:0] __st0;
  logic [0:0] __resumption_tag_next;
  logic [29:0] __st0_next;
  assign gzdLLzicase10537 = {{__resumption_tag, __st0}, __in0};
  zdLLzicase10537  zdLLzicase10537 (gzdLLzicase10537[39:10], gzdLLzicase10537[9:0], callRes);
  assign gzdLLzicase10537R1 = {{__resumption_tag, __st0}, __in0};
  zdLLzicase10537  zdLLzicase10537R1 (gzdLLzicase10537R1[39:10], gzdLLzicase10537R1[9:0], callResR1);
  assign {__continue, __padding, __out0, __resumption_tag_next, __st0_next} = (gzdLLzicase10537R1[40] == 1'h0) ? callResR1 : callRes;
  initial {__resumption_tag, __st0} <= 31'h00000000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0} <= 31'h00000000;
    end else begin
      {__resumption_tag, __st0} <= {__resumption_tag_next, __st0_next};
    end
  end
endmodule

module zdLLzicase10537 (input logic [29:0] arg0,
  input logic [9:0] arg1,
  output logic [77:0] res);
  logic [39:0] gReWireziMonadziiterSt9149;
  logic [69:0] gzdLLzilambda10604;
  logic [69:0] gzdLLzicase10601;
  logic [69:0] gzdLLzilambda10429;
  logic [39:0] gMainziloop;
  logic [29:0] id;
  logic [24:0] gMainziinputToMyState;
  logic [24:0] gzdLLzicase10367;
  logic [24:0] gzdLLzicase10370;
  logic [24:0] gzdLLzicase10374;
  logic [24:0] gzdLLzicase10379;
  logic [24:0] gzdLLzicase10384;
  logic [44:0] gMainziincrPipeline;
  logic [44:0] gzdLLzicase10363;
  logic [29:0] idR1;
  logic [29:0] resizze;
  logic [255:0] binOp;
  logic [127:0] resizzeR1;
  logic [44:0] gzdLLzicase10387;
  logic [14:0] gMainzimyStateToOutput;
  logic [14:0] gzdLLzicase10390;
  logic [14:0] gzdLLzicase10393;
  logic [14:0] gzdLLzicase10396;
  logic [75:0] gzdLLzilambda10597;
  logic [75:0] gzdLLzicase10580;
  logic [77:0] gzdLLzilambda10592;
  logic [77:0] gzdLLzicase10590;
  logic [75:0] gzdLLzilambda10439;
  logic [75:0] gzdLLzicase10437;
  logic [29:0] gzdLLzicase10550;
  logic [93:0] gzdLLzilambda10559;
  logic [93:0] gzdLLzicase10556;
  logic [45:0] gzdLLzilambda10434;
  assign gReWireziMonadziiterSt9149 = {arg1, arg0};
  assign gzdLLzilambda10604 = {gReWireziMonadziiterSt9149[39:30], gReWireziMonadziiterSt9149[29:0], gReWireziMonadziiterSt9149[29:0]};
  assign gzdLLzicase10601 = {gzdLLzilambda10604[59:0], gzdLLzilambda10604[69:60]};
  assign gzdLLzilambda10429 = {gzdLLzicase10601[9:0], gzdLLzicase10601[69:40], gzdLLzicase10601[39:10]};
  assign gMainziloop = {gzdLLzilambda10429[69:60], gzdLLzilambda10429[59:30]};
  assign id = gMainziloop[29:0];
  assign gMainziinputToMyState = {gMainziloop[39:30], id[29:15]};
  assign gzdLLzicase10367 = {gMainziinputToMyState[24:15], gMainziinputToMyState[14:0]};
  assign gzdLLzicase10370 = {gMainziinputToMyState[24:15], gMainziinputToMyState[14:0]};
  assign gzdLLzicase10374 = {gMainziinputToMyState[24:15], gMainziinputToMyState[14:0]};
  assign gzdLLzicase10379 = {gMainziinputToMyState[24:15], gMainziinputToMyState[14:0]};
  assign gzdLLzicase10384 = {gMainziinputToMyState[24:15], gMainziinputToMyState[14:0]};
  assign gMainziincrPipeline = {(gzdLLzicase10384[24:23] == 2'h0) ? {gzdLLzicase10384[14], 1'h1, gzdLLzicase10384[18:15], 5'h00, gzdLLzicase10384[18:15]} : ((gzdLLzicase10379[24:23] == 2'h1) ? {gzdLLzicase10379[14], gzdLLzicase10379[13:9], 1'h1, gzdLLzicase10379[22:15]} : (((gzdLLzicase10374[24:23] == 2'h2) && ((gzdLLzicase10374[15] == 1'h1) && (gzdLLzicase10374[13] == 1'h1))) ? {11'h400, gzdLLzicase10374[12:9]} : (((gzdLLzicase10370[24:23] == 2'h2) && ((gzdLLzicase10370[15] == 1'h1) && (gzdLLzicase10370[13] == 1'h0))) ? {6'h20, gzdLLzicase10370[8:0]} : {6'h00, gzdLLzicase10367[8:0]}))), gMainziloop[29:0]};
  assign gzdLLzicase10363 = gMainziincrPipeline[44:0];
  assign idR1 = gzdLLzicase10363[29:0];
  assign resizze = gzdLLzicase10363[29:0];
  assign binOp = {128'(resizze[29:0]), {8'h80{1'h0}}};
  assign resizzeR1 = binOp[255:128] >> binOp[127:0];
  assign gzdLLzicase10387 = {gzdLLzicase10363[44:30], idR1[29:15], resizzeR1[14:0]};
  assign gMainzimyStateToOutput = gzdLLzicase10387[14:0];
  assign gzdLLzicase10390 = gMainzimyStateToOutput[14:0];
  assign gzdLLzicase10393 = gMainzimyStateToOutput[14:0];
  assign gzdLLzicase10396 = gMainzimyStateToOutput[14:0];
  assign gzdLLzilambda10597 = {{((gzdLLzicase10396[14] == 1'h1) && (gzdLLzicase10396[8] == 1'h0)) ? {2'h3, gzdLLzicase10396[13:9], 5'h00, gzdLLzicase10396[3:0]} : (((gzdLLzicase10393[14] == 1'h1) && (gzdLLzicase10393[8] == 1'h1)) ? {2'h1, gzdLLzicase10393[13:9], 1'h1, gzdLLzicase10393[7:0]} : {7'h00, gzdLLzicase10390[8:0]}), gzdLLzicase10387[44:15]}, gzdLLzilambda10429[29:0]};
  assign gzdLLzicase10580 = gzdLLzilambda10597[75:0];
  assign gzdLLzilambda10592 = {2'h0, gzdLLzicase10580[75:30], gzdLLzicase10580[29:0]};
  assign gzdLLzicase10590 = gzdLLzilambda10592[77:0];
  assign gzdLLzilambda10439 = {gzdLLzicase10590[75:30], gzdLLzicase10590[29:0]};
  assign gzdLLzicase10437 = {gzdLLzilambda10439[75:30], gzdLLzilambda10439[29:0]};
  assign gzdLLzicase10550 = gzdLLzicase10437[59:30];
  assign gzdLLzilambda10559 = {gzdLLzicase10437[75:60], {{2'h1, {6'h2e{1'h0}}}, gzdLLzicase10550[29:0]}};
  assign gzdLLzicase10556 = {gzdLLzilambda10559[77:0], gzdLLzilambda10559[93:78]};
  assign gzdLLzilambda10434 = {gzdLLzicase10556[15:0], gzdLLzicase10556[45:16]};
  assign res = {31'h40000000, gzdLLzilambda10434[45:30], 1'h1, gzdLLzilambda10434[29:0]};
endmodule