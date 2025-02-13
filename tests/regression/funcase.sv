module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [1:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] main_startp_in;
  logic [1:0] main_proc_in;
  logic [1:0] lit_in;
  logic [1:0] lit_inR1;
  logic [1:0] lit_inR2;
  logic [0:0] __continue;
  logic [1:0] __resumption_tag;
  logic [1:0] __resumption_tag_next;
  assign main_startp_in = __resumption_tag;
  assign main_proc_in = main_startp_in[1:0];
  assign lit_in = main_proc_in[1:0];
  assign lit_inR1 = main_proc_in[1:0];
  assign lit_inR2 = main_proc_in[1:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, (lit_inR2[1:0] == 2'h0) ? 1'h0 : ((lit_inR1[1:0] == 2'h1) ? 1'h0 : ((lit_in[1:0] == 2'h2) ? 1'h0 : 1'h1))};
  initial __resumption_tag <= 2'h2;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 2'h2;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule