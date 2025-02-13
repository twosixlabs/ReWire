module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] gMainziloop;
  logic [15:0] mymod;
  logic [7:0] extRes;
  logic [8:0] gzdLLziMainziloop2;
  logic [8:0] gzdLLziMainziloop;
  logic [8:0] callRes;
  logic [127:0] resizzeS0;
  logic [15:0] mymodS0;
  logic [7:0] extResS0;
  logic [8:0] gzdLLziMainziloop3S0;
  logic [8:0] gzdLLziMainziloopS0;
  logic [0:0] __continue;
  logic [15:0] __resumption_tag;
  logic [15:0] __resumption_tag_next;
  assign resizzeS0 = {8'h80{1'h0}};
  assign mymodS0 = resizzeS0[15:0];
  mymod  instS0 (.clk(clk), .rst(rst), .x(mymodS0[15:0]), .out(extResS0[7:0]));
  assign gzdLLziMainziloop3S0 = {1'h0, extResS0};
  assign gzdLLziMainziloopS0 = gzdLLziMainziloop3S0[8:0];
  assign gMainziloop = __resumption_tag;
  assign mymod = gMainziloop[15:0];
  mymod  inst (.clk(clk), .rst(rst), .x(mymod[15:0]), .out(extRes[7:0]));
  assign gzdLLziMainziloop2 = {1'h0, extRes};
  assign gzdLLziMainziloop = gzdLLziMainziloop2[8:0];
  zdLLziMainziloop  zdLLziMainziloop (gzdLLziMainziloop[7:0], callRes);
  assign {__continue, __out0, __resumption_tag_next} = callRes;
  initial __resumption_tag <= 16'h0000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 16'h0000;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLziMainziloop (input logic [7:0] arg0,
  output logic [8:0] res);
  assign res = {1'h1, arg0};
endmodule