# GENERATE RECURSIVE DESCENT PARSER OBJECTS FROM A GRAMMAR
# SEE RecDescent.pod FOR FULL DETAILS

use strict;

package Parse::RecDescent;

use Text::Balanced qw ( extract_codeblock extract_bracketed extract_quotelike );

use vars qw ( $skip );

   *defskip  = \ '\s*';	# DEFAULT SEPARATOR IS OPTIONAL WHITESPACE
   $skip  = '\s*';		# UNIVERSAL SEPARATOR IS OPTIONAL WHITESPACE
my $MAXREP  = 100_000_000;	# REPETITIONS MATCH AT MOST 100,000,000 TIMES

package Parse::RecDescent::LineCounter;

sub TIESCALAR	# ($classname, \$text, $thisparser, $prevflag)
{
	bless {
		text    => $_[1],
		parser  => $_[2],
		prev	=> $_[3]?1:0,
	      }, $_[0];
}

sub FETCH    
{
	my $parser = $_[0]->{parser};
	my $from = $parser->{fulltextlen}-length(${$_[0]->{text}})-$_[0]->{prev};
	$parser->{lastlinenum} = $parser->{offsetlinenum}
			   - Parse::RecDescent::_linecount(substr($parser->{fulltext},$from))
			   + 1;
}

sub STORE
{
	my $parser = $_[0]->{parser};
	$parser->{offsetlinenum} -= $parser->{lastlinenum} - $_[1];
	return undef;
}

sub resync       # ($linecounter)
{
        my $self = tied($_[0]);
        die "Tried to alter something other than a LineCounter\n"
                unless $self =~ /Parse::RecDescent::LineCounter/;
	
	my $parser = $self->{parser};
	my $apparently = $parser->{offsetlinenum}
			 - Parse::RecDescent::_linecount(${$self->{text}})
			 + 1;

	$parser->{offsetlinenum} += $parser->{lastlinenum} - $apparently;
	return 1;
}

package Parse::RecDescent::ColCounter;

sub TIESCALAR	# ($classname, \$text, $thisparser, $prevflag)
{
	bless {
		text    => $_[1],
		parser  => $_[2],
		prev    => $_[3]?1:0,
	      }, $_[0];
}

sub FETCH    
{
	my $parser = $_[0]->{parser};
	my $missing = $parser->{fulltextlen}-length(${$_[0]->{text}})-$_[0]->{prev}+1;
	substr($parser->{fulltext},0,$missing) =~ m/^(.*)\Z/m;
	return length($1);
}

sub STORE
{
	die "Can't set column number via \$thiscolumn\n";
}


package Parse::RecDescent::OffsetCounter;

sub TIESCALAR	# ($classname, \$text, $thisparser)
{
	bless {
		text    => $_[1],
		parser  => $_[2],
	      }, $_[0];
}

sub FETCH    
{
	my $parser = $_[0]->{parser};
	return $parser->{fulltextlen}-length(${$_[0]->{text}});
}

sub STORE
{
	die "Can't set current offset via \$thisoffset\n";
}



package Parse::RecDescent::Rule;

sub new ($$$$$)
{
	my $class = ref($_[0]) || $_[0];
	my $name  = $_[1];
	my $owner = $_[2];
	my $line  = $_[3];
	my $replace = $_[4];

	if (defined $owner->{"rules"}{$name})
	{
		my $self = $owner->{"rules"}{$name};
		if ($replace && !$self->{"changed"})
		{
			$self->reset;
		}
		return $self;
	}
	else
	{
		return $owner->{"rules"}{$name} =
			bless
			{
				"name"     => $name,
				"prods"    => [],
				"calls"    => [],
				"changed"  => 0,
				"line"     => $line,
				"impcount" => 0,
				"vars"	   => "",
			}, $class;
	}
}

sub reset($)
{
	@{$_[0]->{"prods"}} = ();
	@{$_[0]->{"calls"}} = ();
	$_[0]->{"changed"}  = 0;
	$_[0]->{"impcount"}  = 0;
	$_[0]->{"vars"}  = "";
}

sub DESTROY {}

sub hasleftmost($$)
{
	my ($self, $ref) = @_;

	my $prod;
	foreach $prod ( @{$self->{"prods"}} )
	{
		return 1 if $prod->hasleftmost($ref);
	}

	return 0;
}

sub leftmostsubrules($)
{
	my $self = shift;
	my @subrules = ();

	my $prod;
	foreach $prod ( @{$self->{"prods"}} )
	{
		push @subrules, $prod->leftmostsubrule();
	}

	return @subrules;
}

sub expected($)
{
	my $self = shift;
	my @expected = ();

	my $prod;
	foreach $prod ( @{$self->{"prods"}} )
	{
		my $next = $prod->expected();
		unless (! $next or _contains($next,@expected) )
		{
			push @expected, $next;
		}
	}

	return join ' or ', @expected;
}

sub _contains($@)
{
	my $target = shift;
	my $item;
	foreach $item ( @_ ) { return 1 if $target eq $item; }
	return 0;
}

sub addcall($$)
{
	my ( $self, $subrule ) = @_;
	unless ( _contains($subrule, @{$self->{"calls"}}) )
	{
		push @{$self->{"calls"}}, $subrule;
	}
}

sub addprod($$)
{
	my ( $self, $prod ) = @_;
	push @{$self->{"prods"}}, $prod;
	$self->{"changed"} = 1;
	$self->{"impcount"} = 0;
	$prod->{"number"} = $#{$self->{"prods"}};
	return $prod;
}

sub addvar($$)
{
	my ( $self, $var ) = @_;
	$self->{"vars"} .= "my $var;\n";
	$self->{"changed"} = 1;
	return 1;
}

sub nextimplicit($)
{
	my $self = shift;
	my $prodcount = scalar @{$self->{"prods"}};
	my $impcount = ++$self->{"impcount"};
	return "_alternation_${impcount}_of_production_${prodcount}_of_rule_$self->{name}";
}


sub code($$)
{
	my ($self, $namespace, $parser) = @_;

eval 'undef &' . $namespace . '::' . $self->{"name"};

	my $code =
'
# ARGS ARE: ($parser, $text; $repeating, $_noactions, \@args)
sub ' . $namespace . '::' . $self->{"name"} .  '
{
	my $thisparser = $_[0];
	$ERRORS = 0;
	my $thisrule = $thisparser->{"rules"}{"' . $self->{"name"} . '"};
	
	Parse::RecDescent::_trace(q{Trying rule: [' . $self->{"name"} . ']},
				  Parse::RecDescent::_tracefirst($_[1]),
				  q{' . $self->{"name"} . '})
					if defined $::RD_TRACE;

	' . ($parser->{deferrable}
		? 'my $def_at = @{$thisparser->{deferred}};'
		: '') .
	'
	my $err_at = @{$thisparser->{errors}};

	my $_tok;
	my $return = undef;
	my $_matched=0;
	my $commit=0;
	my @item = ();
	my $repeating =  defined($_[2]) && $_[2];
	my $_noactions = defined($_[3]) && $_[3];
 	my @arg =        defined $_[4] ? @{ &{$_[4]} } : ();
	my %arg =        ($#arg & 01) ? @arg : (@arg, undef);
	my $text;
	my $lastsep="";
	my $expectation = new Parse::RecDescent::Expectation($thisrule->expected());
	$expectation->at($_[1]);

	my $thisoffset;
	tie $thisoffset, q{Parse::RecDescent::OffsetCounter}, \$text, $thisparser;
	my ($thiscolumn, $prevcolumn);
	tie $thiscolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser;
	tie $prevcolumn, q{Parse::RecDescent::ColCounter}, \$text, $thisparser, 1;
	my ($thisline, $prevline);
	tie $thisline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser;
	tie $prevline, q{Parse::RecDescent::LineCounter}, \$text, $thisparser, 1;

	'. $self->{vars} .'

';

	my $prod;
	foreach $prod ( @{$self->{"prods"}} )
	{
		next if $prod->mustfail();
		$code .= $prod->code($namespace,$self,$parser);

		$code .= $parser->{deferrable}
				? '		splice
				@{$thisparser->{deferred}}, $def_at unless $_matched;
				  '
				: '';
	}

	$code .=
'
        unless ( $_matched || $return )
	{
		' .($parser->{deferrable}
			? '		splice @{$thisparser->{deferred}}, $def_at;
			  '
			: '') . '

		$_[1] = $text;	# NOT SURE THIS IS NEEDED
		Parse::RecDescent::_trace(q{<<Didn\'t match rule>>},
					 Parse::RecDescent::_tracefirst($_[1]),
					 q{' . $self->{"name"} .'})
					if defined $::RD_TRACE;
		return undef;
	}
	splice @{$thisparser->{errors}}, $err_at;
	$return = $item[$#item] unless defined $return;
	if (defined $::RD_TRACE)
	{
		Parse::RecDescent::_trace(q{>>Matched rule<< (return value: [} .
					  $return . q{])}, "",
					  q{' . $self->{"name"} .'});
		Parse::RecDescent::_trace(q{(consumed: [} .
					  Parse::RecDescent::_tracemax(substr($_[1],0,-length($text))) . q{])}, 
					  Parse::RecDescent::_tracefirst($text),
					  , q{' . $self->{"name"} .'})
	}
	$_[1] = $text;
	return $return;
}
';

	return $code;
}

my @left;
sub isleftrec($$)
{
	my ($self, $rules) = @_;
	my $root = $self->{"name"};
	@left = $self->leftmostsubrules();
	my $next;
	foreach $next ( @left )
	{
		next unless defined $rules->{$next}; # SKIP NON-EXISTENT RULES
		return 1 if $next eq $root;
		my $child;
		foreach $child ( $rules->{$next}->leftmostsubrules() )
		{
		    push(@left, $child)
			if ! _contains($child, @left) ;
		}
	}
	return 0;
}

package Parse::RecDescent::Production;

sub describe ($;$)
{
	return join ' ', map { $_->describe($_[1]) or () } @{$_[0]->{items}};
}

sub new ($$;$$)
{
	my ($self, $line, $uncommit, $error) = @_;
	my $class = ref($self) || $self;

	bless
	{
		"items"    => [],
		"uncommit" => $uncommit,
		"error"    => $error,
		"line"     => $line,
	}, $class;
}

sub expected ($)
{
	my $itemcount = scalar @{$_[0]->{"items"}};
	return ($itemcount) ? $_[0]->{"items"}[0]->describe(1) : '';
}

sub hasleftmost ($$)
{
	my ($self, $ref) = @_;
	return ${$self->{"items"}}[0] eq $ref  if scalar @{$self->{"items"}};
	return 0;
}

sub leftmostsubrule($)
{
	my $self = shift;

	if ( $#{$self->{"items"}} >= 0 )
	{
		my $subrule = $self->{"items"}[0]->issubrule();
		return $subrule if defined $subrule;
	}

	return ();
}

sub mustfail($)
{
	my @items = @{$_[0]->{"items"}};
	if (@items && ref($items[0]) =~ /\AParse::RecDescent::UncondReject/)
	{
		Parse::RecDescent::_warn(1,"Optimizing away production: [". $_[0]->describe ."]")
		and
		Parse::RecDescent::_hint("The production starts with an unconditional <reject>,
		       or a <rulevar> (which is equivalent to a <reject>). In either case
		       the production can never successfully match.");
		return 1;
	}
	return 0;
}

sub changesskip($)
{
	my $item;
	foreach $item (@{$_[0]->{"items"}})
	{
		if (ref($item) =~ /Parse::RecDescent::(Action|Directive)/)
		{
			return 1 if $item->{code} =~ /\$skip/;
		}
	}
	return 0;
}

sub additem($$)
{
	my ( $self, $item ) = @_;
	push @{$self->{"items"}}, $item;
	return $item;
}


sub preitempos
{
	return q
	{
		push @itempos, {'offset' => {'from'=>$thisoffset, 'to'=>undef},
				'line'   => {'from'=>$thisline,   'to'=>undef},
				'column' => {'from'=>$thiscolumn, 'to'=>undef} };
	}
}

sub incitempos
{
	return q
	{
		$itempos[$#itempos]{'offset'}{'from'} += length($1);
		$itempos[$#itempos]{'line'}{'from'}   = $thisline;
		$itempos[$#itempos]{'column'}{'from'} = $thiscolumn;
	}
}

sub postitempos
{
	return q
	{
		$itempos[$#itempos]{'offset'}{'to'} = $thisoffset-1;
		$itempos[$#itempos]{'line'}{'to'}   = $prevline;
		$itempos[$#itempos]{'column'}{'to'} = $prevcolumn;
	}
}

sub code($$$$)
{
	my ($self,$namespace,$rule,$parser) = @_;
	my $code =
'
	while (!$_matched'
	. (defined $self->{"uncommit"} ? '' : ' && !$commit')
	. ')
	{
		' .
		($self->changesskip()
			? 'local $skip = defined($skip) ? $skip : $Parse::RecDescent::skip;'
			: '') .'
		Parse::RecDescent::_trace(q{Trying production: ['
					  . $self->describe . ']},
					  Parse::RecDescent::_tracefirst($_[1]),
					  q{' . $rule ->{name}. '})
						if defined $::RD_TRACE;
		my $thisprod = $thisrule->{"prods"}[' . $self->{"number"} . '];
		' . (defined $self->{"error"} ? '' : '$text = $_[1];' ) . '
		my $_savetext;
		@item = ("' . $rule->{"name"} . '");
';
	$code .= 
'		my @itempos = ({});
'			if $parser->{_checkitempos};

	my $item;
	my $i;
	for ($i = 0; $i < @{$self->{"items"}}; $i++)
	{
		$item = ${$self->{items}}[$i];

		$code .= preitempos() if $parser->{_checkitempos};

		$code .= $item->code($namespace,$rule,$parser->{_checkitempos});

		$code .= postitempos() if $parser->{_checkitempos};

	}

	if ($parser->{_AUTOACTION} && !$item->isa("Parse::RecDescent::Action"))
	{
		$code .= $parser->{_AUTOACTION}->code($namespace,$rule);
		Parse::RecDescent::_warn(1,"Autogenerating action in rule
					   \"$rule->{name}\":
					    $parser->{_AUTOACTION}{code}")
		and
		Parse::RecDescent::_hint("The \$::RD_AUTOSTUB was defined,
					  so any production not ending in an
					  explicit action has the specified
		       			  \"auto-action\" automatically
					  appended.");
	}

	$code .= 
'

		Parse::RecDescent::_trace(q{>>Matched production: ['
					  . $self->describe . ']<<},
					  Parse::RecDescent::_tracefirst($text),
					  q{' . $rule->{name} . '})
						if defined $::RD_TRACE;
		$_matched = 1;
		last;
	}

';
	return $code;
}

1;

package Parse::RecDescent::Action;

sub describe { undef }

sub new ($$$$)
{
	my $class = ref($_[0]) || $_[0];
	bless 
	{
		"code"      => $_[1],
		"lookahead" => $_[2],
		"line"      => $_[3],
	}, $class;
}

sub issubrule { undef }
sub isterminal { 0 }

sub code($$$$)
{
	my ($self, $namespace, $rule) = @_;
	
'
		Parse::RecDescent::_trace(q{Trying action},
					  Parse::RecDescent::_tracefirst($text),
					  q{' . $rule->{name} . '})
						if defined $::RD_TRACE;
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) .'

		$_tok = ($_noactions) ? 0 : do ' . $self->{"code"} . ';
		' . ($self->{"lookahead"}<0?'if':'unless') . ' (defined $_tok)
		{
			Parse::RecDescent::_trace(q{<<Didn\'t match action>> (return value: [undef])})
					if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched action<< (return value: [}
					  . $_tok . q{])}, $text)
						if defined $::RD_TRACE;
		push @item, $_tok;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'
'
}

1;

package Parse::RecDescent::Directive;

sub issubrule { undef }
sub isterminal { 0 }
sub describe { $_[1] ? '' : $_[0]->{name} } 

sub new ($$$$$)
{
	my $class = ref($_[0]) || $_[0];
	bless 
	{
		"code"      => $_[1],
		"lookahead" => $_[2],
		"line"      => $_[3],
		"name"      => $_[4],
	}, $class;
}

sub code($$$$)
{
	my ($self, $namespace, $rule) = @_;
	
'
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) .'

		Parse::RecDescent::_trace(q{Trying directive: ['
					. $self->describe . ']},
					Parse::RecDescent::_tracefirst($text),
					  q{' . $rule->{name} . '})
						if defined $::RD_TRACE; ' .'
		$_tok = do { ' . $self->{"code"} . ' };
		if (defined($_tok))
		{
			Parse::RecDescent::_trace(q{>>Matched directive<< (return value: [}
						. $_tok . q{])},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		else
		{
			Parse::RecDescent::_trace(q{<<Didn\'t match directive>>},
						Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		}
		' . ($self->{"lookahead"} ? '$text = $_savetext and ' : '' ) .'
		last '
		. ($self->{"lookahead"}<0?'if':'unless') . ' defined $_tok;
		push @item, $_tok;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'
'
}

1;

package Parse::RecDescent::UncondReject;

sub issubrule { undef }
sub isterminal { 0 }
sub describe { $_[1] ? '' : $_[0]->{name} }

sub new ($$$;$)
{
	my $class = ref($_[0]) || $_[0];
	bless 
	{
		"lookahead" => $_[1],
		"line"      => $_[2],
		"name"      => $_[3],
	}, $class;
}

# MARK, YOU MAY WANT TO OPTIMIZE THIS.


sub code($$$$)
{
	my ($self, $namespace, $rule) = @_;
	
'
		Parse::RecDescent::_trace(q{>>Rejecting production<< (found '
					 . $self->describe . ')},
					 Parse::RecDescent::_tracefirst($text),
					  q{' . $rule->{name} . '})
						if defined $::RD_TRACE;
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) .'

		$_tok = undef;
		' . ($self->{"lookahead"} ? '$text = $_savetext and ' : '' ) .'
		last '
		. ($self->{"lookahead"}<0?'if':'unless') . ' defined $_tok;
'
}

1;

package Parse::RecDescent::Error;

sub issubrule { undef }
sub isterminal { 0 }
sub describe { $_[1] ? '' : '<error...>' }

sub new ($$$$$)
{
	my $class = ref($_[0]) || $_[0];
	bless 
	{
		"msg"        => $_[1],
		"lookahead"  => $_[2],
		"commitonly" => $_[3],
		"line"       => $_[4],
	}, $class;
}

sub code($$$$)
{
	my ($self, $namespace, $rule) = @_;
	
	my $action = '';
	
	if ($self->{"msg"})  # ERROR MESSAGE SUPPLIED
	{
		#WAS: $action .= "Parse::RecDescent::_error(qq{$self->{msg}}" .  ',$thisline);'; 
		$action .= 'push @{$thisparser->{errors}}, [qq{'.$self->{msg}.'},$thisline];'; 

	}
	else	  # GENERATE ERROR MESSAGE DURING PARSE
	{
		$action .= '
		my $rule = $item[0];
		   $rule =~ s/\Aimplicit_subrule_.\d*\Z/implicit subrule/;
		   $rule =~ s/_/ /g;
		#WAS: Parse::RecDescent::_error("Invalid $rule: " . $expectation->message() ,$thisline);
		push @{$thisparser->{errors}}, ["Invalid $rule: " . $expectation->message() ,$thisline];
		'; 
	}

	my $dir =
	      new Parse::RecDescent::Directive('if (' .
		($self->{"commitonly"} ? '$commit' : '1') . 
		") { do {$action} unless ".' $_noactions; undef } else {0}',
	        			$self->{"lookahead"},0,"<error...>"); 
	return $dir->code($namespace, $rule, 0);
}

1;

package Parse::RecDescent::Token;

sub issubrule { undef }
sub isterminal { 1 }
sub describe ($) { shift->{'description'}}


# ARGS ARE: $self, $pattern, $left_delim, $modifiers, $lookahead, $linenum
sub new ($$$$$$)
{
	my $class = ref($_[0]) || $_[0];
	my $pattern = $_[1];
	my $pat = $_[1];
	my $ldel = $_[2];
	my $rdel = $ldel;
	$rdel =~ tr/{[(</}])>/;

	my $mod = $_[3];

	my $desc;

	if ($ldel eq '/') { $desc = "$ldel$pattern$rdel$mod" }
	else		  { $desc = "m$ldel$pattern$rdel$mod" }
	$desc =~ s/\\/\\\\/g;
	$desc =~ s/\$$/\\\$/g;
	$desc =~ s/}/\\}/g;
	$desc =~ s/{/\\{/g;

	if (!eval "no strict;
		   local \$SIG{__WARN__} = sub {0};
		   '' =~ m$ldel$pattern$rdel" and $@)
	{
		Parse::RecDescent::_error("Token pattern \"m$ldel$pattern$rdel\"
					   is not a valid regular expression",
					   $_[5]);
		$@ =~ s/ at \(eval.*/./;
		Parse::RecDescent::_hint($@);
	}


	bless 
	{
		"pattern"   => $pattern,
		"ldelim"      => $ldel,
		"rdelim"      => $rdel,
		"mod"         => $mod,
		"lookahead"   => $_[4],
		"line"        => $_[5],
		"description" => $desc,
	}, $class;
}


sub code($$$$)
{
	my ($self, $namespace, $rule, $checkitempos) = @_;
	my $ldel = $self->{"ldelim"};
	my $rdel = $self->{"rdelim"};
	my $sdel = $ldel;
	my $mod  = $self->{"mod"};

	$sdel =~ s/[[{(<]/{}/;
	
my $code = '
		Parse::RecDescent::_trace(q{Trying terminal: [' . $self->describe
					  . ']}, Parse::RecDescent::_tracefirst($text),
					  q{' . $rule->{name} . '})
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{' . ($rule->hasleftmost($self) ? ''
				: $self->describe ) . '})->at($text);
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) . '

		' . ($self->{"lookahead"}<0?'if':'unless')
		. ' ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and '
		. ($checkitempos? 'do {'.Parse::RecDescent::Production::incitempos().' 1} and ' : '')
		. '  $text =~ s' . $ldel . '\A(?:' . $self->{"pattern"} . ')'
				 . $rdel . $sdel . $mod . ')
		{
			'.($self->{"lookahead"} ? '$text = $_savetext;' : '').'
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn\'t match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;

			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
					if defined $::RD_TRACE;
		push @item, $&;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'
';

	return $code;
}

1;

package Parse::RecDescent::Literal;

sub issubrule { undef }
sub isterminal { 1 }
sub describe ($) { shift->{'description'} }

sub new ($$$$)
{
	my $class = ref($_[0]) || $_[0];

	my $pattern = $_[1];

	my $desc = $pattern;
	$desc=~s/\\/\\\\/g;
	$desc=~s/}/\\}/g;
	$desc=~s/{/\\{/g;

	bless 
	{
		"pattern"     => $pattern,
		"lookahead"   => $_[2],
		"line"        => $_[3],
		"description" => "'$desc'",
	}, $class;
}


sub code($$$$)
{
	my ($self, $namespace, $rule, $checkitempos) = @_;
	
my $code = '
		Parse::RecDescent::_trace(q{Trying terminal: [' . $self->describe
					  . ']},
					  Parse::RecDescent::_tracefirst($text),
					  q{' . $rule->{name} . '})
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{' . ($rule->hasleftmost($self) ? ''
				: $self->describe ) . '})->at($text);
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) . '

		' . ($self->{"lookahead"}<0?'if':'unless')
		. ' ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and '
		. ($checkitempos? 'do {'.Parse::RecDescent::Production::incitempos().' 1} and ' : '')
		. '  $text =~ s/\A' . quotemeta($self->{"pattern"}) . '//)
		{
			'.($self->{"lookahead"} ? '$text = $_savetext;' : '').'
			$expectation->failed();
			Parse::RecDescent::_trace(qq{<<Didn\'t match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $& . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $&;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'
';

	return $code;
}

1;

package Parse::RecDescent::InterpLit;

sub issubrule { undef }
sub isterminal { 1 }
sub describe ($) { shift->{'description'} }

sub new ($$$$)
{
	my $class = ref($_[0]) || $_[0];

	my $pattern = $_[1];
	$pattern =~ s#/#\\/#g;

	my $desc = $pattern;
	$desc=~s/\\/\\\\/g;
	$desc=~s/}/\\}/g;
	$desc=~s/{/\\{/g;

	bless 
	{
		"pattern"   => $pattern,
		"lookahead" => $_[2],
		"line"      => $_[3],
		"description" => "'$desc'",
	}, $class;
}

sub code($$$$)
{
	my ($self, $namespace, $rule, $checkitempos) = @_;
	
my $code = '
		Parse::RecDescent::_trace(q{Trying terminal: [' . $self->describe
					  . ']},
					  Parse::RecDescent::_tracefirst($text),
					  q{' . $rule->{name} . '})
						if defined $::RD_TRACE;
		$lastsep = "";
		$expectation->is(q{' . ($rule->hasleftmost($self) ? ''
				: $self->describe ) . '})->at($text);
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) . '

		' . ($self->{"lookahead"}<0?'if':'unless')
		. ' ($text =~ s/\A($skip)/$lastsep=$1 and ""/e and '
		. ($checkitempos? 'do {'.Parse::RecDescent::Production::incitempos().' 1} and ' : '')
		. '  do { $_tok = "' . $self->{"pattern"} . '"; 1 } and
		     substr($text,0,length($_tok)) eq $_tok and
		     do { substr($text,0,length($_tok)) = ""; 1; }
		)
		{
			'.($self->{"lookahead"} ? '$text = $_savetext;' : '').'
			$expectation->failed();
			Parse::RecDescent::_trace(q{<<Didn\'t match terminal>>},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched terminal<< (return value: [}
						. $_tok . q{])},
						  Parse::RecDescent::_tracefirst($text))
							if defined $::RD_TRACE;
		push @item, $_tok;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'
';

	return $code;
}

1;

package Parse::RecDescent::Subrule;

sub issubrule ($) { return $_[0]->{"subrule"} }
sub isterminal { 0 }

sub describe ($)
{
	my $desc = $_[0]->{"implicit"} || $_[0]->{"subrule"};
	$desc = "<matchrule:$desc>" if $_[0]->{"matchrule"};
	return $desc;
}

sub callsyntax($$)
{
	if ($_[0]->{"matchrule"})
	{
		return "&{'$_[1]'.qq{$_[0]->{subrule}}}";
	}
	else
	{
		return $_[1].$_[0]->{"subrule"};
	}
}

sub new ($$$$;$$$)
{
	my $class = ref($_[0]) || $_[0];
	bless 
	{
		"subrule"   => $_[1],
		"lookahead" => $_[2],
		"line"      => $_[3],
		"implicit"  => $_[4] || undef,
		"matchrule" => $_[5],
		"argcode"   => $_[6] || undef,
	}, $class;
}


sub code($$$$)
{
	my ($self, $namespace, $rule) = @_;
	
'
		Parse::RecDescent::_trace(q{Trying subrule: [' . $self->{"subrule"} . ']},
				  Parse::RecDescent::_tracefirst($text),
				  q{' . $rule->{"name"} . '})
					if defined $::RD_TRACE;
		if (1) { no strict qw{refs};
		$expectation->is(' . ($rule->hasleftmost($self) ? 'q{}'
				: 'qq{'.$self->describe.'}' ) . ')->at($text);
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' )
		. ($self->{"lookahead"}<0?'if':'unless')
		. ' (defined ($_tok = '
		. $self->callsyntax($namespace.'::')
		. '($thisparser,$text,$repeating,'
		. ($self->{"lookahead"}?'1':'$_noactions')
		. ($self->{argcode} ? ",sub { return $self->{argcode} }"
				   : ',undef')
		. ')))
		{
			'.($self->{"lookahead"} ? '$text = $_savetext;' : '').'
			Parse::RecDescent::_trace(q{<<Didn\'t match subrule: ['
			. $self->{subrule} . ']>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{' . $rule->{"name"} .'})
							if defined $::RD_TRACE;
			$expectation->failed();
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched subrule: ['
					. $self->{subrule} . ']<< (return value: [}
					. $_tok . q{]},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{' . $rule->{"name"} .'})
						if defined $::RD_TRACE;
		push @item, $_tok;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'
		}
'
}

1;

package Parse::RecDescent::Repetition;

sub issubrule ($) { return $_[0]->{"subrule"} }
sub isterminal { 0 }

sub describe ($)
{
	my $desc = $_[0]->{"expected"} || $_[0]->{"subrule"};
	$desc = "<matchrule:$desc>" if $_[0]->{"matchrule"};
	return $desc;
}

sub callsyntax($$)
{
	if ($_[0]->{matchrule})
		{ return "sub { goto &{''.qq{$_[1]$_[0]->{subrule}}} }"; }
	else
		{ return "\\&$_[1]$_[0]->{subrule}"; }
}

sub new ($$$$$$$$$$)
{
	my ($self, $subrule, $repspec, $min, $max, $lookahead, $line, $parser, $matchrule, $argcode) = @_;
	my $class = ref($self) || $self;
	($max, $min) = ( $min, $max) if ($max<$min);

	my $desc;
	if ($subrule=~/\A_alternation_\d+_of_production_\d+_of_rule/)
		{ $desc = $parser->{"rules"}{$subrule}->expected }

	if ($lookahead)
	{
		if ($min>0)
		{
		   return new Parse::RecDescent::Subrule($subrule,$lookahead,$line,$desc,$matchrule,$argcode);
		}
		else
		{
			Parse::RecDescent::_error("Not symbol (\"!\") before
				            \"$subrule\" doesn't make
					    sense.",$line);
			Parse::RecDescent::_hint("Lookahead for negated optional
					   repetitions (such as
					   \"!$subrule($repspec)\" can never
					   succeed, since optional items always
					   match (zero times at worst). 
					   Did you mean a single \"!$subrule\", 
					   instead?");
		}
	}
	bless 
	{
		"subrule"   => $subrule,
		"repspec"   => $repspec,
		"min"       => $min,
		"max"       => $max,
		"lookahead" => $lookahead,
		"line"      => $line,
		"expected"  => $desc,
		"argcode"   => $argcode || undef,
		"matchrule" => $matchrule,
	}, $class;
}

sub code($$$$)
{
	my ($self, $namespace, $rule) = @_;
	
	my ($subrule, $repspec, $min, $max, $lookahead) =
		@{$self}{ qw{subrule repspec min max lookahead} };

'
		Parse::RecDescent::_trace(q{Trying repeated subrule: [' . $self->describe . ']},
				  Parse::RecDescent::_tracefirst($text),
				  q{' . $rule->{"name"} . '})
					if defined $::RD_TRACE;
		$expectation->is(' . ($rule->hasleftmost($self) ? 'q{}'
				: 'qq{'.$self->describe.'}' ) . ')->at($text);
		' . ($self->{"lookahead"} ? '$_savetext = $text;' : '' ) .'
		unless (defined ($_tok = $thisparser->_parserepeat($text, '
		. $self->callsyntax($namespace.'::')
		. ', ' . $min . ', ' . $max . ', '
		. ($self->{"lookahead"}?'1':'$_noactions')
		. ',$expectation,'
		. ($self->{argcode} ? "sub { return $self->{argcode} }"
				   : 'undef')
		. '))) 
		{
			Parse::RecDescent::_trace(q{<<Didn\'t match repeated subrule: ['
			. $self->describe . ']>>},
						  Parse::RecDescent::_tracefirst($text),
						  q{' . $rule->{"name"} .'})
							if defined $::RD_TRACE;
			last;
		}
		Parse::RecDescent::_trace(q{>>Matched repeated subrule: ['
					. $self->{subrule} . ']<< (}
					. @$_tok . q{ times)},
					  
					  Parse::RecDescent::_tracefirst($text),
					  q{' . $rule->{"name"} .'})
						if defined $::RD_TRACE;
		push @item, $_tok;
		' . ($self->{"lookahead"} ? '$text = $_savetext;' : '' ) .'

'
}

1;

package Parse::RecDescent::Expectation;

sub new ($)
{
	bless {
		"failed"	  => 0,
		"expected"	  => "",
		"unexpected"	  => "",
		"lastexpected"	  => "",
		"lastunexpected"  => "",
		"defexpected"	  => $_[1],
	      };
}

sub is ($$)
{
	$_[0]->{lastexpected} = $_[1]; return $_[0];
}

sub at ($$)
{
	$_[0]->{lastunexpected} = $_[1]; return $_[0];
}

sub failed ($)
{
	return unless $_[0]->{lastexpected};
	$_[0]->{expected}   = $_[0]->{lastexpected}   unless $_[0]->{failed};
	$_[0]->{unexpected} = $_[0]->{lastunexpected} unless $_[0]->{failed};
	$_[0]->{failed} = 1;
}

sub message ($)
{
	my ($self) = @_;
	$self->{expected} = $self->{defexpected} unless $self->{expected};
	$self->{expected} =~ s/_/ /g;
	if (!$self->{unexpected} || $self->{unexpected} =~ /\A\s*\Z/s)
	{
		return "Was expecting $self->{expected}";
	}
	else
	{
		$self->{unexpected} =~ /\s*(.*)/;
		return "Was expecting $self->{expected} but found \"$1\" instead";
	}
}

1;

package Parse::RecDescent;

use Carp;
use vars qw ( $AUTOLOAD $VERSION );

my $ERRORS = 0;

$VERSION = '1.61';

# BUILDING A PARSER

my $nextnamespace = "namespace000001";

sub _nextnamespace()
{
	return "Parse::RecDescent::" . $nextnamespace++;
}

sub new ($$)
{
	my $class = ref($_[0]) || $_[0];
	my $self =
	{
		"rules"     => {},
		"namespace" => _nextnamespace(),
		"startcode" => '',
		"_AUTOACTION" => undef,
	};
	if ($::RD_AUTOACTION)
	{
		my $sourcecode = $::RD_AUTOACTION;
		$sourcecode = "{ $sourcecode }"
			unless $sourcecode =~ /\A\s*\{.*\}\s*\Z/;
		$self->{_AUTOACTION}
			= new Parse::RecDescent::Action($sourcecode,0,-1)
	}
	
	bless $self, $class;
	shift;
	return $self->Replace(@_)
}

sub Compile($$$$) {

	die "Compilation of Parse::RecDescent grammars not yet implemented\n";
}

sub DESTROY {}  # SO AUTOLOADER IGNORES IT

# BUILDING A GRAMMAR....

sub Replace ($$)
{
	splice(@_, 2, 0, 1);
	return _generate(@_);
}

sub Extend ($$)
{
	splice(@_, 2, 0, 0);
	return _generate(@_);
}

sub _no_rule ($$;$)
{
	_error("Ruleless $_[0] at start of grammar.",$_[1]);
	my $desc = $_[2] ? "\"$_[2]\"" : "";
	_hint("You need to define a rule for the $_[0] $desc
	       to be part of.");
}

my $NEGLOOKAHEAD	= '\A(\s*\.\.\.!)';
my $POSLOOKAHEAD	= '\A(\s*\.\.\.)';
my $RULE		= '\A\s*(\w+)[ \t]*:';
my $PROD		= '\A\s*([|])';
#my $TOKEN		= q{\A\s*/((\\\\/|[^/])+)/([gimsox]*)};
my $TOKEN		= q{\A\s*/((\\\\/|[^/])*)/([gimsox]*)};
my $MTOKEN		= q{\A\s*m[^\w\s]};
my $LITERAL		= q{\A\s*'((\\\\'|[^'])+)'};
my $INTERPLIT		= q{\A\s*"((\\\\"|[^"])+)"};
my $SUBRULE		= '\A\s*(\w+)';
my $MATCHRULE		= '\A(\s*<matchrule:)';
my $OPTIONAL		= '\A\((\?)\)';
my $ANY			= '\A\((s\?)\)';
my $MANY 		= '\A\((s|\.\.)\)';
my $EXACTLY		= '\A\(([1-9]\d*)\)';
my $BETWEEN		= '\A\((\d+)\.\.([1-9]\d*)\)';
my $ATLEAST		= '\A\((\d+)\.\.\)';
my $ATMOST		= '\A\(\.\.([1-9]\d*)\)';
my $BADREP		= '\A\((-?\d+)?\.\.(-?\d+)?\)';
my $ACTION		= '\A\s*\{';
my $IMPLICITSUBRULE	= '\A\s*\(';
my $COMMENT		= '\A\s*(#.*)';
my $COMMITMK		= '\A\s*<commit>';
my $UNCOMMITMK		= '\A\s*<uncommit>';
my $REJECTMK		= '\A\s*<reject>';
my $CONDREJECTMK	= '\A\s*<reject:';
my $SKIPMK		= '\A\s*<skip:';
my $RESYNCMK		= '\A\s*<resync>';
my $RESYNCPATMK		= '\A\s*<resync:';
my $RULEVARPATMK	= '\A\s*<rulevar:';
my $DEFERPATMK		= '\A\s*<defer:';
my $TOKENPATMK		= '\A\s*<token:';
my $AUTOERRORMK		= '\A\s*<error(\??)>';
my $MSGERRORMK		= '\A\s*<error(\??):';
my $UNCOMMITPROD	= $PROD.'\s*(?=<uncommit)';
my $ERRORPROD		= $PROD.'\s*(?=<error)';
my $LONECOLON		= '\A\s*:';
my $OTHER		= '\A\s*([^\s]+)';

my $lines = 0;


sub _generate($$$;$)
{
	my ($self, $grammar, $replace, $isimplicit) = (@_, 0);

	my $aftererror = 0;
	my $lookahead = 0;
	my $lookaheadspec = "";
	$lines = _linecount($grammar) unless $lines;
	$self->{_checkitempos} = ($grammar =~ /\@itempos\b|\$itempos\s*\[/)
		unless $self->{_checkitempos};
	my $line;

	my $rule = undef;
	my $prod = undef;
	my $item = undef;
	my $lastgreedy = '';

	while ($grammar !~ /^\s*$/)
	{
		$line = $lines - _linecount($grammar) + 1;
		my $commitonly;
		my $code = "";
		my @components = ();
		if ($grammar =~ s/$COMMENT//)
		{
			_parse("a comment",0,$line);
			next;
		}
		elsif ($grammar =~ s/$NEGLOOKAHEAD//)
		{
			_parse("a negative lookahead",$aftererror,$line);
			$lookahead = $lookahead ? -$lookahead : -1;
			$lookaheadspec .= $1;
			next;	# SKIP LOOKAHEAD RESET AT END OF while LOOP
		}
		elsif ($grammar =~ s/$POSLOOKAHEAD//)
		{
			_parse("a positive lookahead",$aftererror,$line);
			$lookahead = $lookahead ? $lookahead : 1;
			$lookaheadspec .= $1;
			next;	# SKIP LOOKAHEAD RESET AT END OF while LOOP
		}
		elsif ($grammar =~ m/$ACTION/
			and do {($code,$grammar) = extract_codeblock($grammar);
				$code })
		{
			_parse("an action", $aftererror, $line, $code);
			$item = new Parse::RecDescent::Action($code,$lookahead,$line);
			$prod and $prod->additem($item)
			      or  $self->_addstartcode($code);
		}
		elsif ($grammar =~ m/$IMPLICITSUBRULE/
			and do {($code,$grammar) = extract_codeblock($grammar,'{(',undef,1); $code })
		{
			$code =~ s/\A\s*\(|\)\Z//g;
			_parse("an implicit subrule", $aftererror, $line,
				"( $code )");
			my $implicit = $rule->nextimplicit;
			$self->_generate("$implicit : $code",0,1);
			$grammar = $implicit . $grammar;
		}
		elsif ($grammar =~ s/$UNCOMMITPROD//)
		{
			_parseunneg("a new (uncommitted) production",
				    0, $lookahead, $line) or next;

			$prod = new Parse::RecDescent::Production($line,1,0);
			$rule and $rule->addprod($prod)
			      or  _no_rule("<uncommit>",$line);
			$aftererror = 0;
		}
		elsif ($grammar =~ s/$ERRORPROD//)
		{
			_parseunneg("a new (error) production", $aftererror,
				    $lookahead,$line) or next;
			$prod = new Parse::RecDescent::Production($line,0,1);
			$rule and $rule->addprod($prod)
			      or  _no_rule("<error>",$line);
			$aftererror = 0;
		}
		elsif ($grammar =~ s/$UNCOMMITMK//)
		{
			_parse("an uncommit marker", $aftererror,$line);
			$item = new Parse::RecDescent::Directive('$commit=0;1',
							  $lookahead,$line,"<uncommit>");
			$prod and $prod->additem($item)
			      or  _no_rule("<uncommit>",$line);
		}
		elsif ($grammar =~ s/$REJECTMK//)
		{
			_parse("an reject marker", $aftererror,$line);
			$item = new Parse::RecDescent::UncondReject($lookahead,$line,"<reject>");
			$prod and $prod->additem($item)
			      or  _no_rule("<reject>",$line);
		}
		elsif ($grammar =~ m/$CONDREJECTMK/
			and do { ($code,$grammar)
					= extract_codeblock($grammar,'<{');
				  $code })
		{
			_parse("a (conditional) reject marker", $aftererror,$line);
			$code =~ /\A\s*<reject:(.*)>\Z/s;
			$item = new Parse::RecDescent::Directive(
				      "($1) ? undef : 1", $lookahead,$line,"<reject:$code>");
			$prod and $prod->additem($item)
			      or  _no_rule("<reject:$code>",$line);
		}
		elsif ($grammar =~ s/$RESYNCMK//)
		{
			_parse("a resync to newline marker", $aftererror,$line);
			$item = new Parse::RecDescent::Directive(
				      'if ($text =~ s/\A[^\n]*\n//) { $return = 0; $& } else { undef }',
				      $lookahead,$line,"<resync>");
			$prod and $prod->additem($item)
			      or  _no_rule("<resync>",$line);
		}
		elsif ($grammar =~ m/$RESYNCPATMK/
			and do { ($code,$grammar)
					= extract_bracketed($grammar,'<');
				  $code })
		{
			_parse("a resync with pattern marker", $aftererror,$line);
			$code =~ /\A\s*<resync:(.*)>\Z/s;
			$item = new Parse::RecDescent::Directive(
				      'if ($text =~ s/\A'.$1.'//) { $return = 0; $& } else { undef }',
				      $lookahead,$line,$code);
			$prod and $prod->additem($item)
			      or  _no_rule($code,$line);
		}
		elsif ($grammar =~ m/$SKIPMK/
			and do { ($code,$grammar)
					= extract_codeblock($grammar,'<');
				  $code })
		{
			_parse("a skip marker", $aftererror,$line);
			$code =~ /\A\s*<skip:(.*)>\Z/s;
			$item = new Parse::RecDescent::Directive(
				      'my $oldskip = $skip; $skip='.$1.'; $oldskip',
				      $lookahead,$line,$code);
			$prod and $prod->additem($item)
			      or  _no_rule($code,$line);
		}
		elsif ($grammar =~ m/$RULEVARPATMK/
			and do { $code = extract_codeblock($grammar,'<>{}') } )
		{
			_parse("a rule variable specifier", $aftererror,$line,$code);
			$code =~ /\A\s*<rulevar:(.*)>\Z/s;

			$rule and $rule->addvar($1)
			      or  _no_rule($code,$line);

			$item = new Parse::RecDescent::UncondReject($lookahead,$line,$code);
			$prod and $prod->additem($item)
			      or  _no_rule($code,$line);
		}
		elsif ($grammar =~ m/$DEFERPATMK/
			and do { $code = extract_codeblock($grammar,'<>{}') } )
		{
			_parse("a deferred action specifier", $aftererror,$line,$code);
			$code =~ s/\A\s*<defer:(.*)>\Z/$1/s;
			if ($code =~ /\A\s*[^{]|[^}]\s*\Z/)
			{
				$code = "{ $code }"
			}

			$item = new Parse::RecDescent::Directive(
				      "push \@{\$thisparser->{deferred}}, sub $code;",
				      $lookahead,$line,"<defer:$code>");
			$prod and $prod->additem($item)
			      or  _no_rule("<defer:$code>",$line);

			$self->{deferrable} = 1;
		}
		elsif ($grammar =~ m/$TOKENPATMK/
			and do { $code = extract_codeblock($grammar,'<>{}') } )
		{
			_parse("a token constructor", $aftererror,$line,$code);
			$code =~ s/\A\s*<token:(.*)>\Z/$1/s;

			my $types = eval 'no strict; local $SIG{__WARN__} = sub {0}; my @arr=('.$code.'); @arr' || (); 
			if (!$types)
			{
				_error("Incorrect token specification: \"$@\"", $line);
				_hint("The <token:...> directive requires a list
				       of one or more strings representing possible
				       types of the specified token. For example:
				       <token:NOUN,VERB>");
			}
			else
			{
				$item = new Parse::RecDescent::Directive(
					      'no strict;
					       $return = { text => $item[-1] };
					       @{$return->{type}}{'.$code.'} = (1..'.$types.');',
					      $lookahead,$line,"<token:$code>");
				$prod and $prod->additem($item)
				      or  _no_rule("<token:$code>",$line);
			}
		}
		elsif ($grammar =~ s/$COMMITMK//)
		{
			_parse("an commit marker", $aftererror,$line);
			$item = new Parse::RecDescent::Directive('$commit = 1',
							  $lookahead,$line,"<commit>");
			$prod and $prod->additem($item)
			      or  _no_rule("<commit>",$line);
		}
		elsif ($grammar =~ s/$AUTOERRORMK//)
		{
			$commitonly = $1;
			_parse("an error marker", $aftererror,$line);
			$item = new Parse::RecDescent::Error('',$lookahead,$1,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("<error>",$line);
			$aftererror = !$commitonly;
		}
		elsif ($grammar =~ m/$MSGERRORMK/
			and do { $commitonly = $1;
				($code,$grammar)
					= extract_bracketed($grammar,'<');
				$code })
		{
			_parse("an error marker", $aftererror,$line,$code);
			$code =~ /\A\s*<error\??:(.*)>\Z/s;
			$item = new Parse::RecDescent::Error($1,$lookahead,$commitonly,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("$code",$line);
			$aftererror = !$commitonly;
		}
		elsif ($grammar =~ s/$RULE//)
		{
			_parseunneg("a rule declaration", 0,
				    $lookahead,$line) or next;
			my $rulename = $1;
			if ($rulename =~ /Replace|Extend|Compile/ )
			{	
				_warn(2,"Rule \"$rulename\" hidden by method
				       Parse::RecDescent::$rulename",$line)
				and
				_hint("The rule named \"$rulename\" cannot be directly
                                       called through the Parse::RecDescent object
                                       for this grammar (although it may still
                                       be used as a subrule of other rules).
                                       It can't be directly called because
				       Parse::RecDescent::$rulename is already defined (it
				       is the standard method of all
				       parsers).");
			}
			$rule = new Parse::RecDescent::Rule($rulename,$self,$line,$replace);
			$prod = $rule->addprod( new Parse::RecDescent::Production );
			$aftererror = 0;
		}
		elsif ($grammar =~ s/$PROD//)
		{
			_parseunneg("a new production", 0,
				    $lookahead,$line) or next;
			$rule
			  and $prod = $rule->addprod(new Parse::RecDescent::Production($line))
			or  _no_rule("production",$line);
			$aftererror = 0;
		}
		elsif ($grammar =~ s/$LITERAL//)
		{
			_parse("a literal terminal", $aftererror,$line);
			$item = new Parse::RecDescent::Literal($1,$lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("literal terminal",$line,"'$1'");
		}
		elsif ($grammar =~ s/$INTERPLIT//)
		{
			_parse("an interpolated literal terminal", $aftererror,$line);
			$item = new Parse::RecDescent::InterpLit($1,$lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("interpolated literal terminal",$line,"'$1'");
		}
		elsif ($grammar =~ s/$TOKEN//)
		{
			_parse("a /../ pattern terminal", $aftererror,$line);
			$item = new Parse::RecDescent::Token($1,'/',$3?$3:'',$lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("pattern terminal",$line,"/$1/");
		}
		elsif ($grammar =~ m/$MTOKEN/
			and do { ($code,$grammar,@components)
					= extract_quotelike($grammar);
				 $code }
		      )

		{
			_parse("an m/../ pattern terminal", $aftererror,$line,$code);
			$item = new Parse::RecDescent::Token(@components[3,2,8],
							     $lookahead,$line);
			$prod and $prod->additem($item)
			      or  _no_rule("pattern terminal",$line,$code);
		}
		elsif ($grammar =~ m/$MATCHRULE/
				and $code = extract_bracketed($grammar,'<')
		       or $grammar =~ s/$SUBRULE//
				and $code = $1)
		{
			my $name = $code;
			my $matchrule = 0;
			if (substr($name,0,1) eq '<')
			{
				$name =~ s/$MATCHRULE//;
				$name =~ s/>\Z//;
				$matchrule = 1;
			}

		# EXTRACT TRAILING ARG LIST (IF ANY)

			my $argcode = extract_codeblock($grammar, "[]",'') || '';

		# EXTRACT TRAILING REPETITION SPECIFIER (IF ANY)

			if ($grammar =~ s/$OPTIONAL//)
			{
				_parse("an zero-or-one subrule match", $aftererror,$line,"$code$argcode($1)");
				$item = new Parse::RecDescent::Repetition($name,$1,0,1,
								   $lookahead,$line,
								   $self,
								   $matchrule,
								   $argcode);
				$prod and $prod->additem($item)
				      or  _no_rule("repetition",$line,"$code$argcode($1)");

				!$matchrule and $rule and $rule->addcall($name);
			}
			elsif ($grammar =~ s/$ANY//)
			{
				_parse("a zero-or-more subrule match", $aftererror,$line,"$code$argcode($1)");
				$item = new Parse::RecDescent::Repetition($name,$1,0,$MAXREP,
								   $lookahead,$line,
								   $self,
								   $matchrule,
								   $argcode);
				$prod and $prod->additem($item)
				      or  _no_rule("repetition",$line,"$code$argcode($1)");

				!$matchrule and $rule and $rule->addcall($name);

				_check_insatiable($name,$1,$grammar,$line);
			}
			elsif ($grammar =~ s/$MANY//)
			{
				_parse("a one-or-more subrule match", $aftererror,$line,"$code$argcode($1)");
				$item = new Parse::RecDescent::Repetition($name,$1,1,$MAXREP,
								   $lookahead,$line,
								   $self,
								   $matchrule,
								   $argcode);
								   
				$prod and $prod->additem($item)
				      or  _no_rule("repetition",$line,"$code$argcode($1)");

				!$matchrule and $rule and $rule->addcall($name);

				_check_insatiable($name,$1,$grammar,$line);
			}
			elsif ($grammar =~ s/$EXACTLY//)
			{
				_parse("an exactly-$1-times subrule match", $aftererror,$line,"$code$argcode($1)");
				$item = new Parse::RecDescent::Repetition($name,$1,$1,$1,
								   $lookahead,$line,
								   $self,
								   $matchrule,
								   $argcode);
				$prod and $prod->additem($item)
				      or  _no_rule("repetition",$line,"$code$argcode($1)");

				!$matchrule and $rule and $rule->addcall($name);
			}
			elsif ($grammar =~ s/$BETWEEN//)
			{
				_parse("a $1-to-$2 subrule match", $aftererror,$line,"$code$argcode($1..$2)");
				$item = new Parse::RecDescent::Repetition($name,"$1..$2",$1,$2,
								   $lookahead,$line,
								   $self,
								   $matchrule,
								   $argcode);
				$prod and $prod->additem($item)
				      or  _no_rule("repetition",$line,"$code$argcode($1..$2)");

				!$matchrule and $rule and $rule->addcall($name);
			}
			elsif ($grammar =~ s/$ATLEAST//)
			{
				_parse("a $1-or-more subrule match", $aftererror,$line,"$code$argcode($1..)");
				$item = new Parse::RecDescent::Repetition($name,"$1..",$1,$MAXREP,
								   $lookahead,$line,
								   $self,
								   $matchrule,
								   $argcode);
				$prod and $prod->additem($item)
				      or  _no_rule("repetition",$line,"$code$argcode($1..)");

				!$matchrule and $rule and $rule->addcall($name);

				_check_insatiable($name,"$1..",$grammar,$line);
			}
			elsif ($grammar =~ s/$ATMOST// 			)
			{
				_parse("a one-to-$1 subrule match", $aftererror,$line,"$code$argcode(..$1)");
				$item = new Parse::RecDescent::Repetition($name,"..$1",1,$1,
								   $lookahead,$line,
								   $self,
								   $matchrule,
								   $argcode);
				$prod and $prod->additem($item)
				      or  _no_rule("repetition",$line,"$code$argcode(..$1)");

				!$matchrule and $rule and $rule->addcall($name);
			}
			elsif ($grammar =~ s/$BADREP// 			)
			{
				_parse("an subrule match with invalid repetition specifier", 0,$line);
				_error("Incorrect specification of a repeated subrule",
				       $line);
				_hint("Repeated subrules like \"$code$argcode$&\" cannot have
				       a maximum repetition of zero, nor can they have
				       negative components in their ranges.");
			}
			else
			{
				_parse("a subrule match", $aftererror,$line,$code);
				my $desc;
				if ($name=~/\A_alternation_\d+_of_production_\d+_of_rule/)
					{ $desc = $self->{"rules"}{$name}->expected }
				$item = new Parse::RecDescent::Subrule($name,
								       $lookahead,
								       $line,
								       $desc,
								       $matchrule,
								       $argcode);
	 
				$prod and $prod->additem($item)
				      or  _no_rule("(sub)rule",$line,$name);

				!$matchrule and $rule and $rule->addcall($name);
			}
		}
		elsif ($grammar =~ s/$LONECOLON//   )
		{
			_error("Unexpected colon encountered", $line);
			_hint("Did you mean \"|\" (to start a new production)?
			           Or perhaps you forgot that the colon
				   in a rule definition must be
				   on the same line as the rule name?");
		}
		elsif ($grammar =~ s/$OTHER//   )
		{
			_error("Untranslatable item encountered: \"$1\"",
			       $line);
			_hint("Did you misspell \"$1\"
			           or forget to comment it out?");
		}

		if ($lookaheadspec =~ tr /././ > 3)
		{
			$lookaheadspec =~ s/\A\s+//;
			$lookahead = $lookahead<0
					? 'a negative lookahead ("...!")'
					: 'a positive lookahead ("...")' ;
			_warn(1,"Found two or more lookahead specifiers in a
			       row.",$line)
			and
			_hint("Multiple positive and/or negative lookaheads
			       are simply multiplied together to produce a
			       single positive or negative lookahead
			       specification. In this case the sequence
			       \"$lookaheadspec\" was reduced to $lookahead.
			       Was this your intention?");
		}
		$lookahead = 0;
		$lookaheadspec = "";
	}

	unless ($ERRORS or $isimplicit)
	{
		$self->_check_grammar();
	}

	unless ($ERRORS or $isimplicit or $self->{'compiling'})
	{
		my $code = $self->_code();
		if (defined $::RD_TRACE)
		{
			print STDERR "printing code (", length($code),") to RD_TRACE\n";
			open TRACE_FILE, ">RD_TRACE"
			and print TRACE_FILE $code
			and close TRACE_FILE;
		}

		unless ( eval "$code 1" )
		{
			_error("Internal error in generated parser code!");
			$@ =~ s/at grammar/in grammar at/;
			_hint($@);
		}
	}

	if ($ERRORS and !_verbosity("HINT"))
	{
		local $::RD_HINT = 1;
		_hint('Set $::RD_HINT (or -RD_HINT if you\'re using "perl -s")
		       for hints on fixing these problems.');
		# undef $::RD_HINT;
	}
	return $ERRORS ? undef : $self;
}


sub _addstartcode($$)
{
	my ($self, $code) = @_;
	$code =~ s/\A\s*\{(.*)\}\Z/$1/s;

	$self->{"startcode"} .= "$code;\n";
}

# CHECK FOR GRAMMAR PROBLEMS....

sub _check_insatiable($$$$)
{
	my ($subrule,$repspec,$grammar,$line) = @_;
	#return unless $repspec =~ /s|\.\.\Z/;
	return if $grammar =~ /$OPTIONAL/ || $grammar =~ /$ANY/;
	my $min = 1;
	if ( $grammar =~ /$MANY/
	  || $grammar =~ /$EXACTLY/
	  || $grammar =~ /$ATMOST/ 
	  || $grammar =~ /$BETWEEN/ && do { $min=$2; 1 }
	  || $grammar =~ /$ATLEAST/ && do { $min=$2; 1 }
	  || $grammar =~ /$SUBRULE(?!\s*:)/
	   )
	{
		return unless $1 eq $subrule && $min > 0;
		_warn(3,"Subrule sequence \"$subrule($repspec) $&\" will
		       (almost certainly) fail.",$line)
		and
		_hint("Unless subrule \"$subrule\" performs some cunning
		       lookahead, the repetition \"$subrule($repspec)\" will
		       insatiably consume as many matches of \"$subrule\" as it
		       can, leaving none to match the \"$&\" that follows.");
	}
}

sub _check_grammar ($)
{
	my $self = shift;
	my $rules = $self->{"rules"};
	my $rule;
	foreach $rule ( values %$rules )
	{
		next if ! $rule->{"changed"};

	# CHECK FOR UNDEFINED RULES

		my $call;
		foreach $call ( @{$rule->{"calls"}} )
		{
			if (!defined ${$rules}{$call})
			{
				if (!defined $::RD_AUTOSTUB)
				{
					_warn(3,"Undefined (sub)rule \"$call\"
					      used in a production.")
					and
					_hint("Will you be providing this rule
					       later, or did you perhaps
					       misspell \"$call\"? Otherwise
					       it will be treated as an 
					       immediate <reject>.");
					eval "sub $self->{namespace}::$call {undef}";
				}
				else	# EXPERIMENTAL
				{
					_warn(1,"Autogenerating rule: $call")
					and
					_hint("A call was made to a subrule
					       named \"$call\", but no such
					       rule was specified. However,
					       since \$::RD_AUTOSTUB
					       was defined, a rule stub
					       ($call : '$call') was
					       automatically created.");

					$self->_generate("$call : '$call'",0,1);
				}
			}
		}

	# CHECK FOR LEFT RECURSION

		if ($rule->isleftrec($rules))
		{
			_error("Rule \"$rule->{name}\" is left-recursive.");
			_hint("Redesign the grammar so it's not left-recursive.
			       That will probably mean you need to re-implement
			       repetitions using the '(s)' notation.
			       For example: \"$rule->{name}(s)\".");
			next;
		}
	}
}
	
# GENERATE ACTUAL PARSER CODE

sub _code($)
{
	my $self = shift;
	my $code = "package $self->{namespace};\nuse strict;\nuse vars qw(\$skip);\n\$skip = '$skip'\n;$self->{startcode}";
	$code .= "push \@$self->{namespace}\::ISA, 'Parse::RecDescent';";
	$self->{"startcode"} = '';

	my $rule;
	foreach $rule ( values %{$self->{"rules"}} )
	{
		if ($rule->{"changed"})
		{
			$code .= $rule->code($self->{"namespace"},$self);
			$rule->{"changed"} = 0;
		}
	}

	return $code;
}


# EXECUTING A PARSE....

sub AUTOLOAD	# ($parser, $text; $linenum, @args)
{
	die "Could not find method: $AUTOLOAD\n" unless ref $_[0];
	my $class = ref($_[0]) || $_[0];
	my $text = ref($_[1]) ? ${$_[1]} : $_[1];
	$_[0]->{lastlinenum} = $_[2]||_linecount($_[1]);
	$_[0]->{offsetlinenum} = $_[0]->{lastlinenum};
	$_[0]->{fulltext} = $_[1];
	$_[0]->{fulltextlen} = length $_[1];
	$_[0]->{deferred} = [];
	$_[0]->{errors} = [];
	my @args = @_[3..$#_];
	my $args = sub { [ @args ] };
				 
	$AUTOLOAD =~ s/$class/$_[0]->{namespace}/;
	no strict "refs";
	my $retval = &{$AUTOLOAD}($_[0],$text,undef,undef,$args);

	if (defined $retval)
	{
		foreach ( @{$_[0]->{deferred}} ) { &$_; }
	}
	else
	{
		foreach ( @{$_[0]->{errors}} ) { _error(@$_); }
	}

	if (ref $_[1]) { ${$_[1]} = $text }

	return $retval;
}

sub _parserepeat($$$$$$$$$$)	# RETURNS A REF TO AN ARRAY OF MATCHES
{
	my ($parser, $text, $prod, $min, $max, $_noactions, $expectation, $argcode) = @_;
	my @tokens = ();
	
	my $reps;
	for ($reps=0; $reps<$max;)
	{
		$_[6]->at($text);	 # $_[6] IS $expectation FROM CALLER
		my $_savetext = $text;
		my $prevtextlen = length $text;
		my $_tok;
		if (! defined ($_tok = &$prod($parser,$text,1,$_noactions,$argcode)))
		{
			$text = $_savetext;
			last;
		}
		push @tokens, $_tok if defined $_tok;
		last if ++$reps >= $min and $prevtextlen == length $text;
	}

	do { $_[6]->failed(); return undef} if $reps<$min;

	$_[1] = $text;
	return [@tokens];
}


# ERROR REPORTING....

my $errortext;
my $errorprefix;

open (ERROR, ">&STDERR");
format ERROR =
@>>>>>>>>>>>>>>>>>>>>: ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$errorprefix,          $errortext
~~                     ^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                       $errortext
.

select ERROR;
$| = 1;

# TRACING

my $tracemsg;
my $tracecontext;
my $tracerulename;

open (TRACE, ">&STDERR");
format TRACE =
|@|||||||||@^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<|
$tracerulename, '|', $tracemsg
| ~~       |^<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<|
	    $tracemsg
.

select TRACE;
$| = 1;

open (TRACECONTEXT, ">&STDERR");
format TRACECONTEXT =
|@|||||||||@                                      |^<<<<<<<<<<<<<<<<<<<<<<<<<<<<
 $tracerulename, '|',				   $tracecontext
| ~~       |                                      |^<<<<<<<<<<<<<<<<<<<<<<<<<<<<
						   $tracecontext
.


select TRACECONTEXT;
$| = 1;

select STDOUT;

sub _verbosity($)
{
	   defined $::RD_TRACE
	or defined $::RD_HINT    and  $_[0] =~ /ERRORS|WARN|HINT/
	or defined $::RD_WARN    and  $_[0] =~ /ERRORS|WARN/
	or defined $::RD_ERRORS  and  $_[0] =~ /ERRORS/
}

sub _error($;$)
{
	$ERRORS++;
	return 0 if ! _verbosity("ERRORS");
	$errortext   = $_[0];
	$errorprefix = "ERROR" .  ($_[1] ? " (line $_[1])" : "");
	$errortext =~ s/\s+/ /g;
	print ERROR "\n" if _verbosity("WARN");
	write ERROR;
	return 1;
}

sub _warn($$;$)
{
	return 0 if !_verbosity("WARN") || $_[0] < ($::RD_WARN||1);
	$errortext   = $_[1];
	$errorprefix = "Warning" .  ($_[2] ? " (line $_[2])" : "");
	print ERROR "\n";
	$errortext =~ s/\s+/ /g;
	write ERROR;
	return 1;
}

sub _hint($)
{
	return 0 unless defined $::RD_HINT;
	$errortext = "$_[0])";
	$errorprefix = "(Hint";
	$errortext =~ s/\s+/ /g;
	write ERROR;
	return 1;
}

sub _tracemax($)
{
	if (defined $::RD_TRACE
	    && $::RD_TRACE =~ /\d+/
	    && $::RD_TRACE>1
	    && $::RD_TRACE+10<length($_[0]))
	{
		my $count = length($_[0]) - $::RD_TRACE;
		return substr($_[0],0,$::RD_TRACE/2)
			. "...<$count>..."
			. substr($_[0],-$::RD_TRACE/2);
	}
	else
	{
		return $_[0];
	}
}

sub _tracefirst($)
{
	if (defined $::RD_TRACE
	    && $::RD_TRACE =~ /\d+/
	    && $::RD_TRACE>1
	    && $::RD_TRACE+10<length($_[0]))
	{
		my $count = length($_[0]) - $::RD_TRACE;
		return substr($_[0],0,$::RD_TRACE) . "...<+$count>";
	}
	else
	{
		return $_[0];
	}
}

my $lastcontext = '';
my $lastrulename = '';

sub _trace($;$$)
{
	$tracemsg      = $_[0];
	$tracecontext  = $_[1]||$lastcontext;
	$tracerulename = $_[2]||$lastrulename;
	if ($tracerulename) { $lastrulename = $tracerulename }

	$tracecontext =~ s/\n/\\n/g;
	$tracecontext =~ s/\s+/ /g;
	$tracerulename = qq{$tracerulename};
	write TRACE;
	if ($tracecontext && $tracecontext ne $lastcontext)
	{
		$lastcontext = $tracecontext;
		$tracecontext = qq{"$tracecontext"};
		write TRACECONTEXT;
	}
}

sub _parseunneg($$$$)
{
	_parse($_[0],$_[1],$_[3]);
	if ($_[2]<0)
	{
		_error("Can't negate \"$&\".",$_[3]);
		_hint("You can't negate $_[0]. Remove the \"...!\" before
		       \"$&\".");
		return 0;
	}
	return 1;
}

sub _parse($$$;$)
{
	my $what = $_[3] || $&;
	   $what =~ s/^\s+//;
	if ($_[1])
	{
		_warn(3,"Found $_[0] ($what) after an unconditional <error>",$_[2])
		and
		_hint("An unconditional <error> always causes the
		       production containing it to immediately fail.
		       \u$_[0] which follows an <error>
		       will never be reached.  Did you mean to use
		       <error?> instead?");
	}

	return if ! _verbosity("TRACE");
	$errortext = "Treating \"$what\" as $_[0]";
	$errorprefix = "Parse::RecDescent";
	$errortext =~ s/\s+/ /g;
	write ERROR;
}

sub _linecount($)
{
	my $string = $_[0];
	return scalar ( $string =~ tr/\n/\n/ );
}

package main;

use vars qw ( $RD_ERRORS $RD_WARN $RD_HINT $RD_TRACE );
$::RD_ERRORS = 1;

1;

