#! /usr/local/bin/perl -sw

# PARSE AND EVALUATE LOGICAL EXPRESSIONS WITH A OO PARSE TREE

$::RD_AUTOACTION = 
	q{ bless [$item[-1]], $item[0] };

use Parse::RecDescent;

my $parse = Parse::RecDescent->new(<<'EOG');

	expr	:	set | clear | disj
	set	:	'set' atom
	clear	:	'clear' atom
	disj	:	<leftop: conj 'or' conj>
	conj	:	<leftop: unary 'and' unary>
	unary	:	neg | atom
	neg	:	'not' atom
	atom	:	/[a-z]+/i
EOG

while (<>)
{
	my $tree = $parse->expr($_);
	print $tree->eval() if $tree;
}

BEGIN {@var{qw(a c e)} = (1,1,1);}

sub returning
{
 	local $^W;
	print +(caller(1))[3], " returning ($_[0])\n";
	$_[0];
}

sub expr::eval     { returning $_[0][0]->eval() }
sub disj::eval     { returning join '', map {$_->eval()} @{$_[0][0]} }
sub conj::eval     { returning ! join '', map {! $_->eval()} @{$_[0][0]} }
sub unary::eval    { returning $_[0][0]->eval() }
sub negation::eval { returning ! $_[0][0]->eval() }
sub set::eval      { returning $::var{$_[0][0]->name()} = 1 }
sub clear::eval    { returning $::var{$_[0][0]->name()} = 0 }
sub atom::eval     { returning $::var{$_[0][0]} }
sub atom::name     { returning $_[0][0] }
