#!/usr/local/bin/perl -ws

# REMOVE COMMENTS FROM C++ CODE

# ORIGINAL BY Helmut Jarausch 
# EXTENDED BY Damian Conway AND Helmut Jarausch 

use strict;
use Parse::RecDescent;

use vars qw/ $Grammar /;

my $parser = new Parse::RecDescent $Grammar  or  die "invalid grammar";

undef $/;
my $text = @ARGV ? <> : <DATA>;

my $parts = $parser->program($text) or die "malformed C program";

print "Comments\n========\n$parts->{comments}\n";
print "\nCode\n====\n$parts->{code}\n";

BEGIN
{ $Grammar=<<'EOF';

program : <rulevar: local $WithinComment=0>
program : <rulevar: local $Comments = "">
program : <rulevar: local $Code = "">

program	: <skip:''> part(s) { { code=>$Code, comments=>$Comments} }

part	: comment
        | ptext
		 { $WithinComment=0; }
        | string

ptext   : m|[^"/]+|
		{ $WithinComment=0; $Code .= $item[1] }
        | m|/[^*/]|
		{ $WithinComment=0; $Code .= $item[1] }

string	: /"(\\.|[^"\\])+"/
		{ $Code .= $item[1] }


comment	: m|\s*//[^\n]*\n|
		{ $Code .= "\n"  unless $WithinComment++; $Comments .= $item[1] }
	| m{\s*/\*(?:[^*]+|\*(?!/))*\*/[ \t]*}
		{ $Comments .= $item[1]; $Code .= " "; }
	| m{\s*/\*		# opt. white space comment opener, then...
	    (?:[^*]+|\*(?!/))*	# anything except */ ...
	    \*/		        # comment closer
            ([ \t]*)?           # trailing blanks or tabs
	   }x	
		{ $Comments .= $item[1]; $Code .= "\n" unless $WithinComment++;  }

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

  char *str = "/* ceci n'est pas un commentaire */";
  return 0;
}





