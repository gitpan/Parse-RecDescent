#!/usr/local/bin/perl -ws

# REMOVE COMMENTS FROM C++ CODE

# ORIGINAL BY Helmut Jarausch 
# EXTENDED BY Damian Conway AND Helmut Jarausch 

use strict;
use Parse::RecDescent;

use vars qw/ $Grammar /;

my $parser = new Parse::RecDescent $Grammar  or  die "invalid grammar";

undef $/;
my $text = <DATA>;

$parser->program($text);

BEGIN
{ $Grammar=<<'EOF';

{ my $WithinComment; }

program	: { $thisparser->{tokensep}= ''; } <reject> # no token sep
        | part(s)

part	: comment
        | ptext
		 { $WithinComment= 0; }
        | string

ptext   : m|[^"/]+|
		{ print "$item[1]"; $WithinComment= 0; }
        | m|/[^*/]|
		{ print "$item[1]"; $WithinComment= 0; }

string	: '"' s_char(s) '"'
		{ print '"',@{$item[2]},'"'; }

s_char	: /[^"\\]+/
	| /(?:\\.)+/

comment	: m|\s*//[^\n]*\n|
		{ print "\n"  unless $WithinComment++;  }
	| m{\s*/\*(?:[^*]+|\*(?!/))*\*/[ \t]*}
		{ print " "; }
	| m{\s*/\*		# opt. white space comment opener, then...
	    (?:[^*]+|\*(?!/))*	# anything except */ ...
	    \*/		        # comment closer
            ([ \t]*)?           # trailing blanks or tabs
	   }x	
		{ print "\n" unless $WithinComment++;  }

EOF
}
__DATA__
program test; // for decomment

// using Parse::RecDescent

int main()
{
/* this should
   be removed
*/
  int i;  // a counter
          // remove this line altogehter
  int k;  
      int more_indented;  // keep indentation
      int l;  /* a loop
             variable */
      // should be completely removed

  char *str = "/* this is no comment */";
  return 0;
}





