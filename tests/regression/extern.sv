module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [15:0] __in0,
  output logic [7:0] __out0);
  logic [15:0] main_loop_in;
  logic [15:0] mymod_in;
  logic [7:0] extres;
  logic [8:0] zll_main_loop2_in;
  logic [8:0] zll_main_loop3_in;
  logic [127:0] resize_inS0;
  logic [15:0] mymod_inS0;
  logic [7:0] extresS0;
  logic [8:0] zll_main_loop2_inS0;
  logic [8:0] zll_main_loop3_inS0;
  logic [0:0] __continue;
  logic [15:0] __resumption_tag;
  logic [15:0] __resumption_tag_next;
  assign resize_inS0 = {8'h80{1'h0}};
  assign mymod_inS0 = resize_inS0[15:0];
  mymod  instS0 (.clk(clk), .rst(rst), .x(mymod_inS0[15:0]), .out(extresS0[7:0]));
  assign zll_main_loop2_inS0 = {1'h0, extresS0};
  assign zll_main_loop3_inS0 = zll_main_loop2_inS0[8:0];
  assign main_loop_in = __resumption_tag;
  assign mymod_in = main_loop_in[15:0];
  mymod  inst (.clk(clk), .rst(rst), .x(mymod_in[15:0]), .out(extres[7:0]));
  assign zll_main_loop2_in = {1'h0, extres};
  assign zll_main_loop3_in = zll_main_loop2_in[8:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, zll_main_loop3_in[7:0]};
  initial __resumption_tag <= 16'h0000;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 16'h0000;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule