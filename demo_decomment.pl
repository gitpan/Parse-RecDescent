#!/usr/local/bin/perl -w

# REMOVE COMMENTS FROM C++ CODE

# ORIGINAL BY Helmut Jarausch 
# EXTENDED BY Damian Conway

use strict;
use Parse::RecDescent;

use vars qw/ $Grammar /;

my $parser = new Parse::RecDescent $Grammar  or  die "invalid grammar";

undef $/;
my $text = <DATA>;

$parser->program($text);

print $text;

BEGIN
{ $Grammar=<<'EOF';

program	: part(s)

part	: ptext
	| string
	| comment

ptext	: /[^"\/]+/		{ print $item[1]; }
	| m{/[^*/]}		{ print $item[1]; }

string	: '"' s_char(s) '"'	{ print '"', @{$item[2]}, '"'; }

s_char  : 			{ $thisrule->{tokensep} = ''; } <reject>
	| /[^"\\]+/
	| /(?:\\.)+/



comment	: m{//.*$}m
	| m{/\*		# COMMENT OPENER, THEN...
	    .*?		# ANY NUMBER OF CHARS UNTIL THE FIRST...
	    \*/		# COMMENT CLOSER
	   }sx		# (OVER MULTIPLE LINES)
EOF
}
__DATA__
// test program for decomment
// using Parse::RecDescent

int main()
{
/* this ***should***
   be removed
*/
  int i;  // a counter
  int k;
  int l;  /* a loop
             variable */

  char* str = "/* Ceci n'est pas un commentaire! */";

  return 0;
}
