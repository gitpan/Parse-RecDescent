#! /usr/local/bin/perl -sw

use Parse::RecDescent;

$grammar = q {
		list: 	  <matchrule:$arg{rule}> /$arg{sep}/ list[@arg]
				{ $return = [ $item[1], @{$item[3]} ] }
		    | 	  <matchrule:$arg{rule}>
				{ $return = [ $item[1]] }

		function: 'func' ident '(' list[rule=>'param',sep=>';'] ')'

		param:	  list[rule=>'ident',sep=>','] ':' typename

		ident:	  /\w+/

		typename: /\w+/
	     };

unless( $parser = new Parse::RecDescent( $grammar ))
{
    die "bad grammar; bailing\n";
}

while (defined($input = <DATA>))
{
	print STDERR "parsing... ";
	unless( defined $parser->function( $input ))
	{
	    die "error in input; bailing\n";
	}
	print STDERR "parsed\n";
}

__DATA__
func f (a,b,c:int; d:float; e,f:string)
func g (x:int)
func h (y;x)
