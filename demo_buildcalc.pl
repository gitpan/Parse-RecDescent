#! /usr/local/bin/perl -ws

use Parse::RecDescent;


Parse::RecDescent->Precompile(<<'EndGrammar', "Calc", $0 );

	main: expr /\s*\Z/ { $item[1]->() }
	    | <error>

	expr: /for(each)?/ lvar range expr
			{ my ($vname,$expr) = @item[2,4];
			  my ($from, $to) = @{$item[3]};
			  sub { my $val;
				no strict "refs";
				for $$vname ($from->()..$to->())
					{ $val = $expr->() }
				return $val;
			      }
			}
	    | lvar '=' addition
			{ my ($vname, $expr) = @item[1,3];
			  sub { no strict 'refs'; $$vname = $expr->() }
			}
	    | addition

	range: "(" expr ".." expr ")"
			{ [ @item[2,4] ] }

	addition: <leftop:multiplication add_op multiplication>
			{ my $add = $item[1]; sub { ::evalop($add) } }

	add_op: '+'	{ sub { $_[0] += $_[1] } }
	      | '-'	{ sub { $_[0] -= $_[1] } }

	multiplication: <leftop:factor mult_op factor>
			{ my $mult = $item[1]; sub { ::evalop($mult) } }
  
	mult_op: '*'	{ sub { $_[0] *= $_[1] } }
	       | '/'	{ sub { $_[0] /= $_[1] } }

	factor: number
	      | rvar
	      | '(' expr ')' { $item[2] }

	number: /[-+]?\d+(\.\d+)?/	{ sub { $item[1] } }

	lvar:	/\$([a-z]\w*)/		{ $1 }

	rvar:	lvar			{ sub { no strict 'refs'; ${$item[1]} } } 

EndGrammar