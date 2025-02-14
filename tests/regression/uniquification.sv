module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [1:0] __in0,
  output logic [1:0] __out0);
  logic [0:0] __continue;
  logic [1:0] __resumption_tag;
  logic [1:0] __resumption_tag_next;
  assign {__continue, __out0, __resumption_tag_next} = 2'h1;
  initial __resumption_tag <= 2'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 2'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule