#!/usr/bin/perl
#
# usestats.pl
#############

# (c) 10/2003-10/2004 Thomas Hochstein  <thh@inter.net>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option)
# any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
# more details.

# Modules #############################
# use Data::Dumper;
use Date::Manip qw(Date_Init ParseDate Date_Cmp DateCalc UnixDate Delta_Format Date_DaysInMonth);
use File::Find;
use Getopt::Std;
use Locale::Recode;
use Mail::Address;
use MIME::Words qw(decode_mimewords);

# Versionsnummer ######################
$ver = '0.17 beta (20041003)';

# Konstanten  #########################

$hex_nibb     = '[0-9a-fA-F]';
$gt_hex_nibb  = '[0-9A-F]';
$lt_hex_nibb  = '[0-9a-f]';
$alpha_num    = '[0-9a-zA-Z]';
$lt_alpha_num = '[0-9a-z]';
$gt_alpha_num = '[0-9A-Z]';
%reporttitel = ('newsgroups' => 'Newsgroupnutzung',
                'poster'     => 'Poster (strukturiert)',
                'posterraw'  => 'Poster (unstrukturiert)',
		'subject'    => 'Subjects (strukturiert)',
		'newsreader' => 'Newsreader (strukturiert)',
                'nruser'     => 'Nutzer pro Newsreader (strukturiert)');

# Variablen ###########################

# Defaults
%config=();

# $config{'newsgroup'} = '';
# $config{'start'} = '';
# $config{'stop'}  = '';
$config{'recursive'} = 0;
$config{'pipeformat'} = 'mbox';
$config{'charset'} = 'ISO-8859-1';
$config{'tz'} = '+0200';
$config{'graphchar'} = '#';

$config{'day'} = 1;
@reports = ('newsgroups','poster','posterraw','subject','newsreader','nruser');
foreach $report (@reports) {
 $config{$report} = 1;
 $config{$report.'_width'}  = 30;
 $config{$report.'_indent'} = 2;
 $config{$report.'_lines'}  = 0;
 $config{$report.'_cutoff'} = 0;
 $config{$report.'_graph'}  = 20;
};
$config{'newsgroups'} = 0;
$config{'posterraw'} = 0;
$config{'unknownreader'} = 'show';

# Hauptprogramm #######################
# Konfiguration einlesen
my %options;
getopts('hqc:r:w:', \%options);
if ($options{'h'}) {
 print "$0 v $ver\nUsage: $0 [-hq] [-w|r <savefile>] [-c <configfile>]\n";
 exit(0);
};
print STDERR "$0 v $ver [" . scalar(gmtime) . "]\n(c) 10/2003-10/2004 Thomas Hochstein * <thh\@inter.net>\n" if (!$options{'q'});
if ($options{'c'}) {
  &readconfig($options{'c'});
};
if (length($config{'graphchar'}) > 1 or $config{'graphchar'} eq '') {
 $config{'graphchar'} = '#';
};
# Charset prüfen
$config{'charset'} = uc($config{'charset'});
$supported = Locale::Recode->getSupported;
if (!scalar(grep /$config{'charset'}/, @{$supported})) {
 print STDERR "ERROR: unknown charset $config{'charset'}\n";
 $config{'charset'} = 'ISO-8859-1';
};

&Date_Init("TZ = $config{'tz'}");

if ($options{'r'}) {
 @postings = @{&readdata($options{'r'})};
 print STDERR "\n>>>>> Data loaded.\n\n" if (!$options{'q'});
} else {
 # Postings parsen
 if (exists($config{'spooldir'})) {
  if ($config{'recursive'}) {
   @postings = @{&get_spool_recursive($config{'spooldir'})};
  } else {
   @postings = @{&get_spool($config{'spooldir'})};
  };
 } else {
  @postings = @{&get_stdin($config{'pipeformat'})};
 };
 print STDERR "\n>>>>> Data parsed.\n\n" if (!$options{'q'});

 # Daten speichern, falls verlangt
 if ($options{'w'}) {
  &writedata($options{'w'},\@postings);
 }
};

# Postings auswerten
$newsgrouplgth = 0;
$posterrawlgth = 0;
$posterlgth = 0;
$subjectlgth = 0;
$newsreaderlgth = 0;

foreach $posting (@postings) {
 %header = %{$posting};
 &day($header{'date'}) if $config{'day'};
 &newsgroups($header{'newsgroups'}) if $config{'newsgroups'};
 &poster($header{'from'}) if ($config{'poster'} or $config{'posterraw'});
 &subject($header{'subject'}) if $config{'subject'};
 &client($posting) if ($config{'newsreader'} or $config{'nruser'});
};

# Nutzer pro Newsreader ermitteln
if ($config{'nruser'}) {
 foreach $user (keys %userreader) {
  $nruser{$userreader{$user}}++;
  if (length($userreader{$user}) > $nruserlgth) { $nruserlgth = length($userreader{$user}); };
  $nrusersum++;
  #D print "!> $user: $userreader{$user}\n";
 };
};

print STDERR "\n>>>>> Data analyzed.\n\n" if (!$options{'q'});

# Ausgabe generieren
$output = "Auswertung";
if ($config{'newsgroup'}) { $output .= " für $config{'newsgroup'}"; };
print center($output);
if ($config{'start'} || $config{'stop'}) {
 $output = '';
 if ($config{'start'}) { $output .= "von $config{'start'} "; };
 if ($config{'stop'}) { $output .= "bis $config{'stop'}"; };
 print center($output);
};
print center("=" x length($output))."\n";

# Postings pro Tag ermitteln und ausgeben
if ($config{'day'}) {
 print center("Postings pro Tag");
 print center("----------------");
 &Date_Init("Language = German");
 $postcount = 0; $firstday = 0; $lastday = 0; $lastkw = 0; $lastmonat = 0;
 foreach $day (sort keys %datecount) {
  $firstday = $day if ($lastday == 0);
  $monat = UnixDate($day,"%B %Y");
  if ($monat ne $lastmonat) {
   if ($postcount > 0) {
    printf "\n\nPostings %-14s: %d (%.2f pro Tag)\n",$lastmonat,$postcount,($postcount / Date_DaysInMonth(UnixDate($lastday,"%m"),UnixDate($lastday,"%Y")));
   };
   $postcount = 0;
   print "\n\n".center($monat,50)."\n";
   print " KW :   Mo :  Di :  Mi :  Do :  Fr :  Sa :  So :\n";
   print "------------------------------------------------";
  };
  $kw = UnixDate($day,"%W");
  if ($kw != $lastkw && $lastkw > 0 && $monat eq $lastmonat) {
   for ($i=$lastkw+1; $i < $kw; $i++) {
    printf "\n %2.2d :",$i;
   };
  };
  if ($kw != $lastkw || $monat ne $lastmonat) {
   printf "\n %2.2d :",$kw;
   print " " x (6*(UnixDate($day,"%w")-1));
  } elsif ($lastday != 0 && $day-$lastday > 1) {
   print " " x (6*($day-$lastday-1));
  };
  printf " %5.5s",$datecount{$day};
  $postcount += $datecount{$day}; 
  $lastday = $day;
  $lastkw = $kw;
  $lastmonat = $monat;
 };
 if ($postcount > 0) {
  printf "\n\nPostings %-14s: %d (%.2f pro Tag)\n",$lastmonat,$postcount,($postcount / Date_DaysInMonth(UnixDate($lastday,"%m"),UnixDate($lastday,"%Y")));
 };
 $daycount = Delta_Format(DateCalc(ParseDate($firstday),ParseDate($lastday)),0,'%dh');
 #D print STDERR "$daycount = $lastday - $firstday\n";
 print "\n\n";
 if (scalar(@postings)) {
  printf "Postings insgesamt: %d (%.2f pro Tag)\n\n\n",scalar(@postings),scalar(@postings)/$daycount;
 } else {
  print "Keine Postings erfasst.\n\n\n";
 };
};
#D print &format_table(\%datecount,scalar(@postings),8,50,2,0,0);

# restliche Reports ausgeben
foreach $report (@reports) {
 if ($config{$report}) {
  print center($reporttitel{$report});
  print center("-" x length($reporttitel{$report}))."\n";
  $reportlgth = $report.'lgth';
  $reportsum  = $report.'sum';
  $$reportsum = scalar(@postings) unless defined($$reportsum);
  print &format_table(\%$report,$$reportsum,$$reportlgth,$config{$report.'_width'},$config{$report.'_indent'},$config{$report.'_lines'},$config{$report.'_cutoff'},$config{$report.'_graph'});
  print "\n\n";
 };
};

print "-- \n$0 v $ver [" . scalar(gmtime) . "]\n(c) 10/2003-10/2004 Thomas Hochstein * <thh\@inter.net>\n";
exit;

# Subroutinen #########################

sub get_stdin {
 # Header aus Postings auslesen (gepipete Datei)
 #  IN: $format
 #      (MBOX-File oder data.dat, gepiped in STDIN)
 # OUT: Referenz auf @postings, das jeweils
 #      Referenzen auf %header (die Header) enthält
 my $format = shift; # Format der zu verarbeitenden Datei
 my ($trigger,$hname,$hcontents,%header,@postings,%midcache,$exit);
 if ($format eq 'hamster') {
  $trigger = '^.{4}X-Hamster-Info: ';
 } else {
  # Default: mbox
  $trigger = '^From \S+\@\S+ ';
 };
 while (<>) {
  # Neues Posting beginnt?
  if (/$trigger/o) {
   $exit = 0;
   PARSELOOP:
   while (<>) {
    # Leerzeile = Header beendet
    if (/^$/) {last PARSELOOP; }
    chomp;
    # Gefoldete Header an letzten anhängen
    if (/^\s+/) {
     s/^\s*(.+)\s*$/$1/;
     # $header{lc($hname)} .= "\n\t$_";
     if (/^=\?/) {
      $header{lc($hname)} .= $_;
     } else {
      $header{lc($hname)} .= " $_";
     };
    # Normale Header
    } else {
     ($hname,$hcontents) = split /:/,$_,2;
     $hcontents =~ s/^\s*(.+)/$1/;
     # richtige Newsgroup, falls gesetzt?
     if (exists($config{'newsgroup'}) && $config{'newsgroup'} ne '' && lc($hname) eq 'newsgroups' && lc($hcontents) !~ /$config{'newsgroup'}/) {
      #D print STDERR "ERROR: not $config{'newsgroup'}\n";
      $exit = 1;
      last PARSELOOP;
     # richtiger Datumsbereich, falls gesetzt?
     } elsif (lc($hname) eq 'date' && ((exists($config{'start'}) && $config{'start'} ne '' && Date_Cmp($hcontents,$config{'start'}) < 0) or (exists($config{'stop'}) && $config{'stop'} ne '' && Date_Cmp($hcontents,$config{'stop'}.' 23:59:59') > 0))) {
      #D print STDERR "ERROR: $hcontents not between $config{'start'} and $config{'stop'}\n";
      $exit = 1;
      last PARSELOOP;
     # Posting schonmal erfasst? MID-cache
     } elsif (lc($hname) eq 'message-id' && exists($midcache{$hcontents})) {
      #D print STDERR "ERROR: $hcontents was already cached\n";
      $exit = 1;
      last PARSELOOP;
     };
     $header{lc($hname)} = $hcontents;
    };
   };
   #D $count++; print "--$count--".$header{'message-id'}."\n";
   # Referenz auf Kopie von %header in @postings einhängen & MID cachen
   if (!$exit) {
    #D print "! $header{'message-id'}\n";
    #D print "> $header{'from'}\n";
    #D print "> $header{'newsgroups'}\n";
    #D print "> $header{'date'}\n";
    #D print "-----\n";
    $midcache{$header{'message-id'}}++;
    push @postings, { %header };
   };
   undef %header;
  };
 };
 return \@postings;
};

sub get_spool {
 # Header aus Postings auslesen (Dateien im Spool)
 #  IN: $verzeichnis
 # OUT: Referenz auf @postings, das jeweils
 #      Referenzen auf %header (die Header) enthält
 # Globale Variablen, die gesetzt werden:
 #      %midcache;
 my $verzeichnis = shift;
 my ($file,$hname,$hcontents,%header,@postings,$exit);
 opendir(SPOOL,$verzeichnis) || print STDERR "ERROR: Could not open spool directory $verzeichnis: $!";
 FILELOOP:
 while(defined($file = readdir(SPOOL))) {
  if($file =~/^\./) { next FILELOOP; };
  #D print "!-> $file\n";
  open POSTING, "<$verzeichnis/$file" || print STDERR "ERROR: Could not open article file $verzeichnis/$file for reading: $!";
  $exit = 0;
  PARSELOOP:
  while(<POSTING>) {
   # Leerzeile = Header beendet
   if (/^$/) {last PARSELOOP; }
   chomp;
   # Gefoldete Header an letzten anhängen
   if (/^\s+/) {
    s/^\s*(.+)\s*$/$1/;
    # $header{lc($hname)} .= "\n\t$_";
    if (/^=\?/) {
     $header{lc($hname)} .= $_;
    } else {
     $header{lc($hname)} .= " $_";
    };
   # Normale Header
   } else {
    ($hname,$hcontents) = split /:/,$_,2;
    $hcontents =~ s/^\s*(.+)/$1/;
    # richtige Newsgroup, falls gesetzt?
    if (exists($config{'newsgroup'}) && $config{'newsgroup'} ne '' && lc($hname) eq 'newsgroups' && lc($hcontents) !~ /$config{'newsgroup'}/) {
     #D print STDERR "ERROR: not $config{'newsgroup'}\n";
     $exit = 1;
     last PARSELOOP;
    # richtiger Datumsbereich, falls gesetzt?
    } elsif (lc($hname) eq 'date' && ((exists($config{'start'}) && $config{'start'} ne '' && Date_Cmp($hcontents,$config{'start'}) < 0) or (exists($config{'stop'}) && $config{'stop'} ne '' && Date_Cmp($hcontents,$config{'stop'}).' 23:59:59' > 0))) {
     #D print STDERR "ERROR: $hcontents not between $config{'start'} and $config{'stop'}\n";
     $exit = 1;
     last PARSELOOP;
    # Posting schonmal erfasst? MID-cache
    } elsif (lc($hname) eq 'message-id' && exists($midcache{$hcontents})) {
     #D print STDERR "ERROR: $hcontents was already cached\n";
     $exit = 1;
     last PARSELOOP;
    };
    $header{lc($hname)} = $hcontents;
   };
  };
  # Referenz auf Kopie von %header in @postings einhängen & MID cachen
  if (!$exit) {
   $midcache{$header{'message-id'}} = 1 ;
   push @postings, { %header };
  };
  undef %header;
  # print "! $header{'message-id'}\n";
  # print "> $header{'from'}\n";
  close(POSTING);
 };
 closedir(DIR);
 return \@postings;
};

sub get_spool_recursive {
 # Header aus Postings auslesen (Dateien im Spool, mehrere Spoolverzeichnisse)
 # ruft &get_spool rekursiv auf
 #  IN: $wurzelverzeichnis
 # OUT: Referenz auf @postings, das jeweils
 #      Referenzen auf %header (die Header) enthält
 my $wurzelverzeichnis = shift;
 my (@verzeichnisse, $verzeichnis);
 find sub { -d $File::Find::name && push @verzeichnisse, $File::Find::name }, $wurzelverzeichnis;
 foreach $verzeichnis (@verzeichnisse) {
  push @postings, @{&get_spool($verzeichnis)};  
 };
 return \@postings;
};

sub day {
 # Postings pro Tag ermitteln
 #  IN: Date:-Header des jeweiligen Postings
 # OUT: ---
 # Globale Variablen, die gesetzt werden:
 #    %datecount;
 my $datum = shift;
 if ($datum = UnixDate($datum,"%Y%m%d")) {
  $datecount{$datum}++;
 };
 #D print ">> $datum\n";
 return;
};

sub poster {
 # Poster (mit vollem From + nur mit Namen) auswerten
 #  IN: From:-Header des jeweiligen Postings
 # OUT: ---
 # Globale Variablen, die gesetzt werden:
 #    %posterraw;
 #    %poster;
 #    $posterrawlgth;
 #    $posterlgth;
 my $from = shift;
 my $name;
 ($from) = Mail::Address->parse($from);
 if (defined($from)) {
  # Postings pro Poster (nach Namen im From:)
  $name = $from->format;
 } else {
  $name = '(unbekannt)';
 };
 if ($name eq '') { $name = '(unbekannt)'; };
 $posterraw{$name}++;
 if (length($name) > $posterrawlgth) { $posterrawlgth = length($name); };
 if (defined($from)) {
  # Postings pro Poster (nach From:)
  $name = &mime_decode($from->name);
 };
 #D print "::>> $name\n";
 $poster{$name}++;
 if (length($name) > $posterlgth) { $posterlgth = length($name); };
 #D print "!> $address : $name\n";
 return;
};

sub subject {
 # Subject auswerten
 #  IN: Subject:-Header des jeweiligen Postings
 # OUT: ---
 # Globale Variablen, die gesetzt werden:
 #    %subject;
 #    $subjectlgth;
 # Subject decodieren
 my $subject = shift;
 $subject = &mime_decode($subject);
 $subject =~ s/^(re|aw): (.+)/$2/i;
 # Postings mit entsprechendem Subject
 $subject{$subject}++;
 if (length($subject) > $subjectlgth) { $subjectlgth = length($subject); };
 #D print "!> $subject\n";
 return;
};

sub newsgroups {
 # Newsgroup auswerten
 #  IN: Newsgroup:-Header des jeweiligen Postings
 # OUT: ---
 # Globale Variablen, die gesetzt werden:
 #    %newsgroups;
 #    $newsgroupslgth;
 #    $newsgroupssum:
 # Newsgroup decodieren - wohl nicht erforderlich?
 my $newsgroup;
 my $newsgroups = shift;
 #D print "!> $newsgroup\n";
 # $newsgroups = &mime_decode($newsgroups);
 my @newsgroups = split /,/,$newsgroups;
 # Postings in entsprechende Newsgroups
 foreach $newsgroup (@newsgroups) {
  # auch Crossposts als Inkarnation zählen
  $newsgroupssum++;
  chomp($newsgroup);
  $newsgroups{$newsgroup}++;
  if (length($newsgroup) > $newsgroupslgth) { $$newsgroupslgth = length($newsgroup); };
  #D print "--> $newsgroup\n";
 }
 return;
};

sub client {
 # Newsreader zu ermitteln versuchen
 #  IN: Referenz auf %header des jeweiligen Postings
 # OUT: ---
 # Globale Variablen, die gesetzt werden:
 #    %newsreader;
 #    $newsreaderlgth;
 #    %userreader;
 my $posting = shift;
 my %header = %{$posting};
 my ($newsreader);
 # zutreffenden Header ermitteln, mit Präzedenz User-Agent -> X-Newsreader -> X-Mailer
 if (defined($header{'user-agent'}) && $header{'user-agent'} !~ /^hamster/i) {
  $newsreader = &mime_decode($header{'user-agent'});
 } elsif(defined($header{'x-newsreader'})) {
  $newsreader = &mime_decode($header{'x-newsreader'});
 } elsif(defined($header{'x-mailer'})) {
  $newsreader = &mime_decode($header{'x-mailer'});
 };
 # Newsreader zu ermitteln versuchen
 if ((defined($newsreader)) and ($newsreader ne '')) {
  #D print "! $newsreader\n";
  KNOWN: {
   $newsreader = 'Outlook Express', last KNOWN if $newsreader=~/Outlook Express/i;
   $newsreader = 'Mozilla', last KNOWN if ($newsreader=~/Mozilla/i and $newsreader!~/StarOffice/i);
   $newsreader = 'Star Office', last KNOWN if ($newsreader=~/Mozilla/i and $newsreader =~/StarOffice/i);
   $newsreader = 'Forté Agent', last KNOWN if ($newsreader=~/Forte.*Agent/i or $header{'message-id'}=~/^[a-zA-Z0-9=+]{28,34}\@/ or $header{'message-id'}=~/^$lt_alpha_num{8}\.\d{7,9}\@/o or $header{'message-id'}=~/^$lt_alpha_num{7}\.\d{2,3}\.\d\@/o);
   $newsreader = 'Xnews', last KNOWN if ($newsreader=~/Xnews/i or $header{'message-id'}=~/^$lt_alpha_num{6}\.$lt_alpha_num{2}\.\d\@/o);
   $newsreader = 'Gnus', last KNOWN if ($newsreader=~/Gnus/i or $header{'message-id'}=~/^$lt_alpha_num{10,11}\.fsf\@/o);
   $newsreader = 'slrn', last KNOWN if ($newsreader=~/slrn/i or $header{'message-id'}=~/^slrn$lt_alpha_num{6}\.$lt_alpha_num{2,3}\.\w+\@/o);
   $newsreader = 'MacSOUP', last KNOWN if ($newsreader=~/MacSOUP/i or $header{'message-id'}=~/^$lt_alpha_num{7}\.$lt_alpha_num{13,14}[A-Z]\%[a-zA-Z\.]+\@/o);
   $newsreader = 'Gravity', last KNOWN if ($newsreader=~/Gravity/i or $header{'message-id'}=~/^MPG\.$lt_hex_nibb{22}\@/o);
   $newsreader = 'Pine', last KNOWN if ($newsreader=~/Pine/i or $header{'message-id'}=~/^Pine\.$gt_alpha_num{3}\.\d\.\d{2}\.\d{14}\.\d{4,5}-\d{6}\@/o);
   $newsreader = 'Crosspoint', last KNOWN if ($newsreader=~/Crosspoint/i or $header{'message-id'}=~/^[a-zA-Z0-9\$\-]{11}\@/o);
   $newsreader = 'Crosspoint/OpenXP', last KNOWN if ($newsreader=~/OpenXP/);
   $newsreader = 'Pan', last KNOWN if ($newsreader=~/Pan/);
   $newsreader = 'Fortitude Dialog', last KNOWN if ($newsreader=~/Dialog/i);
   $newsreader = 'tin', last KNOWN if ($newsreader=~/tin\//);
   $newsreader = 'KNode', last KNOWN if ($newsreader=~/KNode/);
   $newsreader = 'Opera', last KNOWN if ($newsreader=~/Opera/);
   $newsreader = 'Freenet-Webnews', last KNOWN if ($newsreader=~/Freenet-Webnews/i);
   $newsreader = 'NewsFleX', last KNOWN if ($newsreader=~/NewsFleX/i);
   $newsreader = 'knews', last KNOWN if ($newsreader=~/knews/i);
   $newsreader = 'WinVN', last KNOWN if ($newsreader=~/WinVN/i);
   $newsreader = 'nn', last KNOWN if ($newsreader=~/nn\//);
   $newsreader = 'Lotus Notes', last KNOWN if ($newsreader=~/Lotus.*Notes/);
   $newsreader = 'NewsPortal', last KNOWN if ($newsreader=~/NewsPortal/i);
   $newsreader = 'ProNews', last KNOWN if ($newsreader=~/ProNews/i);
   $newsreader = 'Sylpheed', last KNOWN if ($newsreader=~/Sylpheed/i);
   print STDERR "Unknown Newsreader: $newsreader\n" if (!$options{'q'});
   if($config{'unknownreader'} eq 'show')  {
    # $newsreader ist ja schon gesetzt
   } else {
    $newsreader = '(Sonstiger)';
   };
  }
 } else {
  $newsreader = '(keine Angabe)'
 };
 # Usernamen ermitteln und Newsreader dieses Nutzers setzen
 # (bei versch. Newsreadern zählt also der letzte)
 ($from) = Mail::Address->parse($header{'from'});
 if (defined($from)) {
  $name = $from->format;
  $userreader{$name} = $newsreader;
  #D print "> $userreader{$name}\n";
 };
 #D print "> $newsreader\n";
 if (length($newsreader) > $newsreaderlgth) { $newsreaderlgth = length($newsreader); };
 $newsreader{$newsreader}++;
 return;
}

sub center {
 # Text mittig formatieren
 #  IN: $text, $width
 # OUT: (zentrierter Text)
 my $text  = shift; # Text
 my $width = shift; # Breite der Spalte
 chomp($text);
 $width ||= 70; # Defaultbreite: 70
 return ' 'x(($width-length($text))/2).$text."\n";
};

sub format_table {
 # Tabelle formatieren
 #  IN: \%values, $sum, $longest, $maxwidth, $indent, $lines, $cutoff, $graph
 # OUT: $output (formatierte Tabelle)
 my $values     = shift; # Referenz auf %values
 my $sum        = shift; # Gesamtsumme (fuer Prozentwert)
 my $longest    = shift; # Längster Index von %values
 my $maxwidth   = shift; # Maximale Spaltenbreite für den Index
 my $indent     = shift; # Einzug links
 my $lines      = shift; # Maximale Zeilenzahl der Tabelle
 my $cutoff     = shift; # Minimaler anzuzeigender Wert
 my $graphwidth = shift; # Spaltenbreite für Graph
 my %values = %{$values};
 my ($output,$value,$outputvalue,$valuelongest,$place,$outputplace,$lastvalue,$notdisplayed,$percent,$maxpercent,$graph,$graphlength);
 # $maxwidth auf minimal 10 setzen und $longest an $maxwidth anpassen
 if ($maxwidth > 0) {
  if ($maxwidth < 10) { $maxwidth = 10; };
  if ($longest > $maxwidth) { $longest = $maxwidth; };
 };
 # $lines auf minimal 3 setzen
 if ($lines > 0 && $lines < 3) { $lines = 3; };
 # Tabelle ausgeben
 $lastvalue = 0;
 foreach $value (sort {$values{$b} <=> $values{$a}} keys %values) {
  # Prozentzahl berechnen
  $percent = 100*$values{$value}/$sum;
  # Platzziffer setzen, ggf. unterdrücken, falls selber Wert wie zuvor
  $place++; $outputplace = '';
  if ($lastvalue != $values{$value}) { $outputplace = $place.'.'; };
  $outputvalue = $value;
  # Platz 1 hat größten Wert -> höchste Breite für Wertespalte und höchsten Prozentwert ermitteln
  if ($place == 1) {
   $valuelongest = length($values{$value});
   $maxpercent = $percent;
  };
  # falls Index zu breit: kürzen und "[...]" anhängen
  if ($maxwidth > 0 && length($value) > $maxwidth) {
   $outputvalue = substr($value,0,$maxwidth-5)."[...]";
  };
  # Balkengrafik generieren
  if ($graphwidth > 0) {
   $graphlength = int($percent*$graphwidth/$maxpercent);
   $graph = '|'.$config{'graphchar'}x$graphlength.' 'x($graphwidth-$graphlength).'|';
  } else {
   $graph = ''
  };
  # Zeilen ausgeben, bis maximale Zeilenzahl oder Grenzwert erreicht (in letzterem Fall aber mind. 1 Zeile)
  if (($lines < 1 || $place <= $lines) && ($cutoff < 1 || $place < 2 || $values{$value} >= $cutoff)) {
   $output .= ' 'x$indent.sprintf "%4s %-${longest}s: %${valuelongest}s (%5.2f%%) %${graphwide}s\n", $outputplace, $outputvalue, $values{$value}, $percent, $graph;
  # falls Zeile nicht mehr ausgegeben: mitzählen
  } else {
   $notdisplayed++;
  };
  $lastvalue = $values{$value};
 };
 # "(+ ... weitere)"
 if (($lines > 0 && $place > $lines) or ($cutoff > 0 && $lastvalue < $cutoff)) {
  $output .= ' 'x($indent+4)."(+ $notdisplayed weitere";
  if ($cutoff > 0 || $values{$value} < $cutoff) {
   $output .= " mit < $cutoff";
  };
  $output .= ")\n";
 };
 $output .= ' 'x$indent.'-'x(16+$longest+$valuelongest)."\n";
 $output .= ' 'x($indent+6+$longest).sprintf "%${valuelongest}s ( 100% )\n", $sum;
 return $output;
};

sub readconfig {
 # Konfigurationsdatei auslesen
 #  IN: Name der Konfigurationsdatei
 # OUT: ---
 # Globale Variablen, die gesetzt werden:
 #    %config;
 my $conffile = shift;
 if (-e $conffile) {
  open CONFIG, "<$conffile" || print STDERR "ERROR: Could not open config file $conffile for reading: $!";
 } else {
  print STDERR "ERROR: Config file $conffile does not exist!\n";
  return;
 };
 while(<CONFIG>) {
  chomp;
  s/#.*//;
  s/^\s+//;
  s/\s+$//;
  next unless length;
  my($option,$value) = split(/\s*=\s*/, $_, 2);
  $config{$option} = $value;
  #D print "$option: $value\n;"
 };
 close CONFIG;
 return;
};

sub mime_decode {
 # Header mit MIME-Words decodieren
 #  IN: $mimeheader - potentiell codierter Header
 # OUT: $decoded - decodierte Zeile
 my $mimeheader = shift;
 my ($mimeword, $decoded);
 foreach $mimeword (decode_mimewords($mimeheader)) {
  my ($data, $encoding) = @{$mimeword};
  #D printf "Enc: %12s %s \n", $encoding, $data;
  if ($encoding ne '') {
   $encoding = uc($encoding);
   my $convert = Locale::Recode->new (from => $encoding,
                                      to   => $config{'charset'});
   print STDERR 'Recode: '.$convert->getError."\n" if $convert->getError;
   $convert->recode ($data) or print STDERR 'Recode: '.$convert->getError."\n";
   #D printf " >>> %12s %s \n", $encoding, $data;
  };
  $decoded .= $data;
 };
 return $decoded;
};

sub writedata {
 # --> Perl-Kochbuch, Rezept 11.10
 # Datendatei (Header) schreiben
 #  IN: $datei - Name der Datendatei
 #      Referenz auf @postings
 # OUT: ---
 my $datei = shift;
 my $postingr = shift;
 my @postings = @{$postingr};
 open DATEI, ">$datei" || print STDERR "ERROR: Could not open file $datei for writing: $!";
 foreach $posting (@postings) {
  for $header (sort keys %{$posting}) {
   print DATEI "$header: $posting->{$header}\n";
  }
  print DATEI "\n";
 };
 close DATEI;
 return;
};

sub readdata {
 # --> Perl-Kochbuch, Rezept 11.10
 # Datendatei (Header) einlesen
 #  IN: $datei - Name der Datendatei
 # OUT: Referenz auf @postings, das jeweils
 #      Referenzen auf %header (die Header) enthält
 my $datei = shift;
 open DATEI, "<$datei" || print STDERR "ERROR: Could not open file $datei for reading: $!";
 $/ = '';
 while (<DATEI>) {
  my @fields = split /^([^:]+):\s*/m;
  shift @fields;
  push(@postings, {map/(.*)/, @fields});
 };
 close DATEI;
 return \@postings;
};