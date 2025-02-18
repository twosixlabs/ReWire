module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [7:0] __in0,
  output logic [15:0] __out0);
  logic [7:0] main_loop1_in;
  logic [7:0] resize_in;
  logic [31:0] binop_in;
  logic [16:0] zll_main_loop1_in;
  logic [16:0] zll_main_loop3_in;
  logic [0:0] __continue;
  logic [7:0] __resumption_tag;
  logic [7:0] __resumption_tag_next;
  assign main_loop1_in = __resumption_tag;
  assign resize_in = main_loop1_in[7:0];
  assign binop_in = {16'(resize_in[7:0]), 16'h1};
  assign zll_main_loop1_in = {1'h0, binop_in[31:16] ^ binop_in[15:0]};
  assign zll_main_loop3_in = zll_main_loop1_in[16:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, zll_main_loop3_in[15:0]};
  initial __resumption_tag <= 8'hfe;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 8'hfe;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule