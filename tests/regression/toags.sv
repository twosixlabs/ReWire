module top_level (input logic [0:0] clk,
  input logic [0:0] rst,
  input logic [0:0] __in0,
  output logic [0:0] __out0);
  logic [5:0] gzdLLzicase2749;
  logic [5:0] gzdLLzicase2753;
  logic [2:0] gzdLLzilambda2690;
  logic [2:0] gzdLLzicase2787;
  logic [1:0] gzdLLzicase2675;
  logic [2:0] gzdLLzilambda2764;
  logic [2:0] gzdLLzicase2762;
  logic [2:0] gzdLLzicase2790;
  logic [1:0] gzdLLzicase2676;
  logic [7:0] gzdLLzilambda2784;
  logic [7:0] gzdLLzicase2781;
  logic [3:0] gzdLLzilambda2688;
  logic [1:0] gzdLLzicase2678;
  logic [1:0] gReWireziPreludezizaza;
  logic [0:0] callRes;
  logic [1:0] id;
  logic [1:0] gReWireziPreludezizazaR1;
  logic [0:0] callResR1;
  logic [5:0] gzdLLzicase2758;
  logic [3:0] gzdLLzilambda2685;
  logic [1:0] gzdLLzilambda2776;
  logic [1:0] gzdLLzicase2774;
  logic [6:0] gzdLLzilambda2770;
  logic [6:0] gzdLLzicase2768;
  logic [1:0] gMainzigo;
  logic [0:0] __continue;
  logic [2:0] __resumption_tag;
  logic [0:0] __st0;
  logic [0:0] __st1;
  logic [2:0] __resumption_tag_next;
  logic [0:0] __st0_next;
  logic [0:0] __st1_next;
  assign gzdLLzicase2749 = {{__resumption_tag, __st0, __st1}, __in0};
  assign gzdLLzicase2753 = {{__resumption_tag, __st0, __st1}, __in0};
  assign gzdLLzilambda2690 = {gzdLLzicase2753[0], gzdLLzicase2753[2], gzdLLzicase2753[1]};
  assign gzdLLzicase2787 = {gzdLLzilambda2690[2], gzdLLzilambda2690[1], gzdLLzilambda2690[0]};
  assign gzdLLzicase2675 = {gzdLLzicase2787[1], gzdLLzicase2787[0]};
  assign gzdLLzilambda2764 = {gzdLLzicase2675[1], gzdLLzicase2675[1], gzdLLzicase2675[0]};
  assign gzdLLzicase2762 = gzdLLzilambda2764[2:0];
  assign gzdLLzicase2790 = {gzdLLzilambda2690[2], gzdLLzilambda2690[1], gzdLLzilambda2690[0]};
  assign gzdLLzicase2676 = {gzdLLzicase2790[1], gzdLLzicase2790[0]};
  assign gzdLLzilambda2784 = {gzdLLzilambda2690[2], (gzdLLzicase2790[2] == 1'h1) ? {5'h14, gzdLLzicase2676[1], gzdLLzicase2676[0]} : {4'h1, gzdLLzicase2762[2], gzdLLzicase2762[1], gzdLLzicase2762[0]}};
  assign gzdLLzicase2781 = {gzdLLzilambda2784[6:0], gzdLLzilambda2784[7]};
  assign gzdLLzilambda2688 = {gzdLLzicase2781[0], gzdLLzicase2781[3], gzdLLzicase2781[2], gzdLLzicase2781[1]};
  assign gzdLLzicase2678 = {gzdLLzilambda2688[2], gzdLLzilambda2688[3]};
  assign gReWireziPreludezizaza = {gzdLLzicase2678[0], gzdLLzicase2678[0]};
  ReWireziPreludezizaza  ReWireziPreludezizaza (gReWireziPreludezizaza[1], gReWireziPreludezizaza[0], callRes);
  assign id = {gzdLLzilambda2688[2], gzdLLzilambda2688[3]};
  assign gReWireziPreludezizazaR1 = {(id[1] == 1'h1) ? id[0] : callRes, gzdLLzilambda2688[2]};
  ReWireziPreludezizaza  ReWireziPreludezizazaR1 (gReWireziPreludezizazaR1[1], gReWireziPreludezizazaR1[0], callResR1);
  assign gzdLLzicase2758 = {{__resumption_tag, __st0, __st1}, __in0};
  assign gzdLLzilambda2685 = {gzdLLzicase2758[3], gzdLLzicase2758[0], gzdLLzicase2758[2], gzdLLzicase2758[1]};
  assign gzdLLzilambda2776 = {gzdLLzilambda2685[1], gzdLLzilambda2685[3]};
  assign gzdLLzicase2774 = gzdLLzilambda2776[1:0];
  assign gzdLLzilambda2770 = {5'h00, gzdLLzicase2774[1], gzdLLzicase2774[0]};
  assign gzdLLzicase2768 = gzdLLzilambda2770[6:0];
  assign gMainzigo = {gzdLLzicase2768[1], gzdLLzicase2768[0]};
  assign {__continue, __out0, __resumption_tag_next, __st0_next, __st1_next} = (gzdLLzicase2758[5:4] == 2'h0) ? {5'h12, gMainzigo[1], gMainzigo[0]} : ((gzdLLzicase2753[5:4] == 2'h1) ? {1'h1, callResR1, 2'h0, gzdLLzilambda2688[2], gzdLLzilambda2688[1], gzdLLzilambda2688[0]} : {4'h1, gzdLLzicase2749[0], gzdLLzicase2749[2], gzdLLzicase2749[1]});
  initial {__resumption_tag, __st0, __st1} <= 5'h09;
  always @ (posedge clk or posedge rst) begin
    if (rst == 1'h1) begin
      {__resumption_tag, __st0, __st1} <= 5'h09;
    end else begin
      {__resumption_tag, __st0, __st1} <= {__resumption_tag_next, __st0_next, __st1_next};
    end
  end
endmodule

module ReWireziPreludezizaza (input logic [0:0] arg0,
  input logic [0:0] arg1,
  output logic [0:0] res);
  logic [1:0] lit;
  logic [1:0] id;
  assign lit = {arg0, arg1};
  assign id = {arg0, arg1};
  assign res = (id[1] == 1'h1) ? id[0] : 1'h0;
endmodule