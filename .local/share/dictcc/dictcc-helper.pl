#! /usr/bin/perl

# before you start: download dictionary file from dict.cc an rename it as wb.txt
# put it in the same directory as this script.
# ensure that it's encoding is the same as your systems!

# by matthias.bloch at puffin dot ch

# because the code is in German I wrote some comments in English :-)

open (LISTE, "dict.txt") or die "can't load dictionary: $!";
@woerterbuch= <LISTE>;
close LISTE;

# replace all ß by ss. German users may want to comment this line out. :-)
#$_ =~ s/ß/ss/ foreach (@woerterbuch);

$hits_per_page = 15;
$pad="true";

while (1){
	if (length($next_input)==0 || $next_input =~ /^n$/i) {
		%treffer = undef;
		$laengstes = 0;
		print "\n\nWelches Wort suchen? ";	# Search which word?
		$input = <STDIN>;
	} else {
		$input = $next_input;
	}
	undef $next_input;
	chomp ($input);
	if(length($input) == 0) {
		exit;
	}
	
	print "*"x(21+length($input));
	print "\n\n";
	
	foreach $e (@woerterbuch) {
		if ($e =~ /$input/i) {
			($englisch, $deutsch) = split (/::/, $e);	# the dictionary is in the form: english_word::german_word
			s/\s$//g foreach ($englisch, $deutsch);   # remove space
			$deutsch =~ s/^\s//g;
			chomp $deutsch;
			$treffer{$englisch}.=", " if (exists $treffer{$englisch});
			$treffer{$englisch}.=$deutsch;
			
			$laengstes = length ($englisch) if ($laengstes < length ($englisch));
		}
	}
	
	$treffer_anzahl = 0;
	SUCHE:{
		foreach $match ("^$input\$", "^$input\\S", "^$input\\s", ".+\\s$input", "\\S$input"
		) {
			# first it looks for a perfect match, then for a match at the beginning, then a match anywere
			foreach (sort keys %treffer){
				if ($_ =~  /$match/i or $treffer{$_} =~ /$match/i){
					$treffer_anzahl ++;
					if ($treffer_anzahl > $hits_per_page) {
						print "weitere Treffer anzeigen? (J / n)"; # ask whether to continue printing after a few matches
						$next_input = <STDIN>;
						chomp ($next_input);
						last SUCHE if ($next_input !~ /^[yj]?$/i);
						$treffer_anzahl = 0;
					}
					treffer_drucken($_)
				}
			}
		}
	}
	
}

# print the findings
sub treffer_drucken {
	($erste_spalte = $_) =~ s/($input)/\e[4m$1\e[0m/gi;
	print $erste_spalte, "\n";
}
