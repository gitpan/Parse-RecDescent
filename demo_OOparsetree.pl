#! /usr/local/bin/perl -sw

# PARSE LOGICAL EXPRESSIONS TO A OO PARSE TREE

package binary_node;
	sub new { bless { left=>$_[1], right=>$_[3] }, ref($_[0])||$_[0] }
	sub printme  { print "\n", "    " x $_[1], ref $_[0], ":\n";
		       $_[0]->{left}->printme($_[1]+1) if $_[0]->{left};
		       $_[0]->{right}->printme($_[1]+1) if $_[0]->{right};
		     }

package unary_node;
	sub new { bless { child=>$_[2] }, ref($_[0])||$_[0] }
	sub printme  { print "\n", "    " x $_[1], ref $_[0], ":\n";
		       $_[0]->{child}->printme($_[1]+1) if $_[0]->{child};
		     }

package assign_node;
	@ISA = qw { binary_node };

package expr_node;
	@ISA = qw { binary_node };

package disj_node;
	@ISA = qw { binary_node };

package conj_node;
	@ISA = qw { binary_node };

package not_node;
	@ISA = qw { unary_node };

package brack_node;
	@ISA = qw { unary_node };

package atom_node;
	sub new { bless { name=>$_[1] } }
	sub printme { print "    " x $_[1], "atom: $_[0]->{name}\n" }

package main;

use Parse::RecDescent;

$RD_AUTOACTION = q
	  { $#item==1 ? $item[1] : new ${\"$item[0]_node"} (@item[1..$#item]) };


$grammar =
q{
	expr	:	assign
	assign  :	disj '=' assign | disj
	disj	:	conj 'or' disj | conj
	conj	:	unary 'and' conj | unary
	unary	:	not | brack | atom
	not     :	'not' atom
	brack	:	'(' expr ')'
	atom	:	/[a-z]+/i
				{ new atom_node ($item[1]) }
};

$parse = new Parse::RecDescent ($grammar);

while (<DATA>)
{
	my $tree = $parse->expr($_);
	$tree->printme(0) if $tree;
}

__DATA__
a and b and not c
(c or d) and f

a = b or (c and d)
