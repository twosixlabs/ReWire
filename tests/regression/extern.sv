module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [16:0] gzdLLzicase4693;
  logic [9:0] callRes;
  logic [16:0] gzdLLzicase4693R1;
  logic [9:0] callResR1;
  logic [127:0] resizzeS0;
  logic [15:0] mymodS0;
  logic [7:0] extResS0;
  logic [9:0] gzdLLzilambda4703S0;
  logic [9:0] gzdLLzicase4701S0;
  logic [7:0] gzdLLzilambda4677S0;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign resizzeS0 = {8'h80{1'h0}};
  assign mymodS0 = resizzeS0[15:0];
  mymod  instS0 (.clk(clk), .rst(rst), .x(mymodS0[15:0]), .out(extResS0[7:0]));
  assign gzdLLzilambda4703S0 = {2'h0, extResS0};
  assign gzdLLzicase4701S0 = gzdLLzilambda4703S0[9:0];
  assign gzdLLzilambda4677S0 = gzdLLzicase4701S0[7:0];
  assign gzdLLzicase4693 = {__resumption_tag, __in0};
  zdLLzicase4693  zdLLzicase4693 (clk, rst, gzdLLzicase4693[15:0], callRes);
  assign gzdLLzicase4693R1 = {__resumption_tag, __in0};
  zdLLzicase4693  zdLLzicase4693R1 (clk, rst, gzdLLzicase4693R1[15:0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase4693R1[16] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h0;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h0;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase4693 (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [15:0] arg0,
  output logic [9:0] res);
  logic [15:0] gMainziloop;
  logic [15:0] mymod;
  logic [7:0] extRes;
  logic [9:0] gzdLLzilambda4699;
  logic [9:0] gzdLLzicase4697;
  logic [7:0] gzdLLzilambda4673;
  assign gMainziloop = arg0;
  assign mymod = gMainziloop[15:0];
  mymod  inst (.clk(clk), .rst(rst), .x(mymod[15:0]), .out(extRes[7:0]));
  assign gzdLLzilambda4699 = {2'h0, extRes};
  assign gzdLLzicase4697 = gzdLLzilambda4699[9:0];
  assign gzdLLzilambda4673 = gzdLLzicase4697[7:0];
  assign res = {1'h1, gzdLLzilambda4673[7:0], 1'h0};
endmodule