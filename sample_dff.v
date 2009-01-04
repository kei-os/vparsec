module sample_dff(
	clk,
	rst_n,
	en,
	d, q	
);

input clk, rst_n;
input en;
input [3:0]	d;
output [3:0] q;

reg [3:0] q;

always@(posedge clk or negedge rst_n) begin
	if (!rst_n)		q <= 4'd0;
	else if (en)	q <= d;
	else			q <= q;
end

endmodule
