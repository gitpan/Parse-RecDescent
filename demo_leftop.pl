#! /usr/local/bin/perl -ws

# THE COMMONEST REASON FOR WANTING LEFT RECURSION

use strict;
use Parse::RecDescent; $::RD_HINT = 1;

sub Parse::RecDescent::evalop
{
	my ($list) = @_;
	my $val = shift @$list;
	while (@$list)
	{
		my ($op, $arg2) = splice @$list, 0, 2;
		$op->($val,$arg2);
	}
	return $val;
}

my $parse = Parse::RecDescent->new(<<'EndGrammar');

	main: expr /\Z/ { $item[1] }
	    | <error>

	expr: <leftop:term add_op term>
			{ evalop($item[1]) }

	add_op: '+'	{ sub { $_[0] += $_[1] } }
	      | '-'	{ sub { $_[0] -= $_[1] } }

	term: <leftop:factor mult_op factor>
			{ evalop($item[1]) }
  
	mult_op: '*'	{ sub { $_[0] *= $_[1] } }
	       | '/'	{ sub { $_[0] /= $_[1] } }

	factor: number
	      | '(' expr ')' { $item[2] }

	number: /[-+]?\d+(\.\d+)?/

EndGrammar

while (<DATA>) {
  print $parse->main($_), "\n";
}

__DATA__
+1-1+1-1+1-1+1-1+1
7*7-6*8
121/(121/11)/121*11
1/(10-1/(1/(10-1)))
