#! /usr/local/bin/perl -sw

# THE ONLY TRUE MISTAKES ARE THE ONES YOU NEVER MAKE

use Parse::RecDescent;

$grammar =
q{
	Para:	  Sentence(s)

	Sentence: Noun Verb 
		| Verb Noun
		| <error>

	Noun:     Fish
	    |	  Cat
	    |	  'dog'

	Verb:	  'runs'

	Fish:	  'fish'
	    |	  <error:Expected a fish!
			 But didn't get one>

	Cat:	  'cat'
	    |	  <error:I wanna cat!>
};

$parse = new Parse::RecDescent ($grammar);

while (<DATA>)
{
	chomp;
	print "$_...\n";
	$parse->Para($_);
}

__DATA__
rat runs
dog runs
cat purrs
cat runs
