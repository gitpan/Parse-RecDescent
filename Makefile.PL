
use ExtUtils::MakeMaker;
WriteMakefile(
		NAME	=> q[Parse::RecDescent],
		VERSION => q[1.90],
                PREREQ_PM => { 'Text::Balanced'=> 0 },

	     );
