module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [1:0] __in0,
  output logic [0:0] __out0);
  logic [2:0] gzdLLzicase2573;
  logic [2:0] callRes;
  logic [2:0] gzdLLzicase2573R1;
  logic [2:0] callResR1;
  logic [0:0] __continue;
  logic [0:0] __resumption_tag;
  logic [0:0] __resumption_tag_next;
  assign gzdLLzicase2573 = {__resumption_tag, __in0};
  zdLLzicase2573  zdLLzicase2573 (gzdLLzicase2573[1:0], callRes);
  assign gzdLLzicase2573R1 = {__resumption_tag, __in0};
  zdLLzicase2573  zdLLzicase2573R1 (gzdLLzicase2573R1[1:0], callResR1);
  assign {__continue, __out0, __resumption_tag_next} = (gzdLLzicase2573R1[2] == 1'h0) ? callResR1 : callRes;
  initial __resumption_tag <= 1'h1;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      __resumption_tag <= 1'h1;
    end else begin
      __resumption_tag <= __resumption_tag_next;
    end
  end
endmodule

module zdLLzicase2573 (input logic [1:0] arg0,
  output logic [2:0] res);
  logic [1:0] gMainzistartp;
  logic [1:0] gMainziproc;
  logic [1:0] lit;
  logic [1:0] litR1;
  logic [1:0] litR2;
  assign gMainzistartp = arg0;
  assign gMainziproc = gMainzistartp[1:0];
  assign lit = gMainziproc[1:0];
  assign litR1 = gMainziproc[1:0];
  assign litR2 = gMainziproc[1:0];
  assign res = {1'h1, (litR2[1:0] == 2'h0) ? 1'h0 : ((litR1[1:0] == 2'h1) ? 1'h0 : ((lit[1:0] == 2'h2) ? 1'h0 : 1'h1)), 1'h0};
endmodule