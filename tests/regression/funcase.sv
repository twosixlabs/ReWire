module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [1:0] __in0,
  output logic [0:0] __out0);
  logic [1:0] gMainzistartp;
  logic [1:0] gMainziproc;
  logic [1:0] lit;
  logic [1:0] litR1;
  logic [1:0] litR2;
  logic [0:0] __continue;
  logic [1:0] __resumption_tag;
  logic [1:0] __resumption_tag_next;
  assign gMainzistartp = __resumption_tag;
  assign gMainziproc = gMainzistartp[1:0];
  assign lit = gMainziproc[1:0];
  assign litR1 = gMainziproc[1:0];
  assign litR2 = gMainziproc[1:0];
  assign {__continue, __out0, __resumption_tag_next} = {1'h1, (litR2[1:0] == 2'h0) ? 1'h0 : ((litR1[1:0] == 2'h1) ? 1'h0 : ((lit[1:0] == 2'h2) ? 1'h0 : 1'h1))};
  initial __resumption_tag <= 2'h2;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 2'h2;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule