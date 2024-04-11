#!/usr/bin/perl -c "use strict";

use 5.16.0;
#use warnings 'all';
 
BEGIN {
    $pl::VERSION = 'v0.91.1';
    my $richer = shift if @ARGV && $ARGV[0] eq '+'; # don't recursively call perl again for -p
    return unless @ARGV and $ARGV[0] =~ /^-/s;
    my $ploa = qr/[rhv?]|[oOpP](?:[1-9][0-9]*)?/; # simple pl opts_values, reflect these in pod/html
    my $pl = qr/[cfTtUuWwXan]|$ploa/; # simple opts_values to pass to perl, which handles them well
    my( $perl, @perl, @pl1, %seen );
    while( @ARGV ) {            # no for, may need to shift
        $_ = $ARGV[0];
        last unless /^-/s;
        shift;
        last if /^--$/s;
        if( /^--(?:help|version|color(?:|=never|=always|=auto))$/s ) {
            push @pl::opts_values, $_;
            next;
        }
        unshift @ARGV, "-$1" if s/^-$pl*[0l][0-7]*$ploa+\K(0.*)//; # avoid -l012o0 -> -l0120
        if( /^(-$pl*(?:[0l][0-7]*$pl*)*)(?:([CDdFiIMm0ABbEeV])(.*))?$/s ) { # Perl does 0 & l inline only with oct arg.
            my( $opts_values, $opt, $arg ) = ($1, $2, $3);
            $seen{$_} ||= $opts_values =~ $_ for qw(a n O o P p r);
            push @pl1, $1 while $opts_values =~ s/($ploa)//;
            if( defined $opt ) {
                if( ! length $arg and $opt =~ /[IABbEeV]/ ) { # These can have arg glued or separated
                    $arg = shift;
                    unless( defined $arg ) {
                        warn "Missing argument to -$opt.\n";
                        exit 29;
                    }
                }
                if( $opt eq 'V' ) {
                    $perl = "perl$arg";
                    undef $opt; # treat $opts_values below
                } elsif( $opt eq 'I' ) {
                    push @perl, $opts_values.'I', $arg; # let perl check for empty arg
                } elsif( $opt =~ /[ABbEe]/ ) {
                    $seen{$opt} ||= $opt =~ /[be]/;
                    push @pl::opts_values, join( '', '-', @pl1, $opt ), $arg;
                    @pl1 = ();
                    undef $opt; # treat $opts_values below
                } else {
                    $seen{F} ||= $opt eq 'F';
                    push @perl, $opts_values.$opt.$arg;
                }
            }
            unless( defined $opt ) { # no else because of -[bBeEV] above
                push @perl, $opts_values if 1 < length $opts_values;
            }
        } else {
            s/^-$pl*(?:[0l][0-7]*$pl*)*(.).*/-$1/;
            warn "Unrecognized switch: $_  (-h will show valid options).\n";
            exit 29;
        }
    }
    $seen{a} ||= push @perl, '-a' if $seen{F};
    $seen{n} ||= push @perl, '-n'
      if ! grep $seen{$_}, qw(o O)
      and grep $seen{$_}, qw(a b e P p r);
    push @pl::opts_values, join '', '-', @pl1 if @pl1;
#warn join '|', $perl // $^X, @perl, '--', $0, '+', @pl::opts_values, '--', @ARGV, "\n";
    if( ! $richer and $perl || @perl ) { # let perl do perl-opts_values
        unshift @ARGV, $perl //= $^X, @perl, '--', $0, '+', @pl::opts_values, '--';
        if( $^O =~ /^MSWin/ ) {
            require Win32::ShellQuote;
            my $ret = system {$ARGV[0]} Win32::ShellQuote::quote_system_list( @ARGV );
            warn $perl, ": $!\n" if $ret < 0;
            exit $ret >> 8;
        }
        exec @ARGV;
        warn $perl, ": $!\n";
        exit 1;
    }
}
 
use sort 'stable';
 
use List::Util '/./'; # all EXPORT_OK
BEGIN {
    eval q{
        sub any(&@) { my $prog = shift; &$prog && return 1 for @_ };
        sub all(&@) { my $prog = shift; &$prog || return for @_; 1 };
        sub npl(&@) { my $prog = shift; &$prog && return for @_; 1 };
        sub notall(&@) { my $prog = shift; &$prog || return 1 for @_ };
        sub product(@) { reduce { $a * $b } 1, @_ };
        *sum0 = sub(@) { sum 0, @_ } if !defined \&sum0; # perl < 5.18
    } if $List::Util::VERSION lt '1.33'; # perl < 5.20
}
 
 
our( $A, @A,
     @Static,
     $H, $HEX_SORT,
     $I, $ARGIND,
     %K, %KEYDIFF,
     %N, %NUMBER,
     $q, $quote,
     $Q, $Quote,
     $R, $RESULT,
     @R, @RESULT,
     %R, %RESULT,
     %T, %TEMPLATE,
     $T, $TXT_SORT );
 
 
 
# Echo without newline
sub Echo(@) {
    if( wantarray ) {
        map {
            if( ! defined ) {
                "$pl::c{I}undef$pl::c{E}";
            } elsif( !ref ) {
                $_;
            } elsif( eval { $_->can( '(""' ) } ) {
                "$_";
            } else {
                require Data::Dumper;
                Data::Dumper::Dumper( $_ )
                    =~ s/;?\n?$//rs
                    =~ s/\bundef\b/$pl::c{I}undef$pl::c{E}/grs;
            }
        } @_ ? @_ : $_;
    } elsif( defined wantarray ) {
        join ' ', &Echo;
    } else {
        local( $,, $\, $| ) = (' ', '', 1);
        print &Echo;
    }
}
 
sub echo(@) {
    if( wantarray ) {
        my @ret = &Echo;
        $ret[-1] .= "\n" if @ret;
        @ret;
    } elsif( defined wantarray ) {
        join( ' ', &Echo ) . "\n";
    } else {
        local $, = ' ';
        say &Echo;
    }
}
 
 
# Form without newline
sub Form($@) {
    my $form;                   # don't spill $1..., overriding pls passed in
    { $form = shift =~ s/%\K(%|[^a-z]+)/ my $x = $1; $x =~ tr!:!$!; $x /egr } # also matches %%
    $pl::Echo->( sprintf $form, &Echo );
}
 
# Would call it format (which is useless in pl-liners,) but that's not overridable.
sub form($@) {
    local $pl::Echo = \&echo;
    &Form;
}
 
 
# Template without newline
sub Template(_;@) {
    local *TEMPLATE = ref( $_[1] ) ? $_[1] : { @_[1..$#_] } if @_ > 1;
    my @_template; # only closure to leak into sub
    $pl::Echo->( &{$pl::template{$_[0]} //= eval do {
        my $drizzle = $_[0];
        if( ref $drizzle ) {
            if( 'GLOB' ne ref $drizzle ) {
                open my $fh, '<', $$drizzle;
                $drizzle = $fh;
            }
            local $/;
            $drizzle = <$drizzle>;
        }
        my( $b, $e ) = $drizzle =~ /(?|(\[%).+?(%\])|(\{\{).+?(\}\})|(<\?pl\b).+?(\?>))/; # which syntax comes 1st?
        my @split = $b ?
          split /(?:\h*\Q$b~\E|\Q$b\E(?(?<=l)\b))\s*(.+?)\s*(?:\Q~$e\E\h*\n?|\Q$e\E)/s, $drizzle :
          $drizzle;
        my $t = join '',
          qq[#line 1 "template"\nsub { my ], # todo use/no strict... preserve linenos
          (map {
              if( $_ % 2 ) { # {{markup...}}
                  my @part = split /\s*(\??)([|:!])\s*/, $split[$_], 2;
                  if( @part < 2 ) { # {{markup}}
                      " . (\$TEMPLATE{$part[0]} // '');";
                  } elsif( $part[2] eq '|' && $part[3] eq '' ) {
                      ' . &Template( $' . ($part[0] eq '' ? '_' : "TEMPLATE{$part[0]}") . ');';
                  } else {
                      $part[1] &&= "last unless exists \$TEMPLATE{$part[0]};";
                      my $local = $part[0] =~ s!.+!local \$_ = \$TEMPLATE{$&} // ''!;
                      if( $part[2] eq '!' ) {
                          $part[3] =~ s!([({])\K!$1 eq '(' ? "($part[0])?():()," : "$part[0];"!e if $local;
                          "\n; $part[3]\n;";
                      } else {
                          ' . (do {{ ' . ($local ? "$part[0]; " : '') .
                            ($local && $part[1] // '') .
                            $part[3] .
                            ($part[2] eq ':' ? "\n; \$_ }} // '');" : "\n }} // '');");
                      }
                  }
              } else { # text
                  push @_template, $split[$_];
                  '$_template ' . ($_ ? '.=' : '=') . " \$_template[$#_template]";
              }
          } 0..$#split),
          "\n; \$_template }";
        if( ($ENV{PLDUMP} // 0) == 2 ) { echo 'my @_template =', \@_template, ';'; say $t; exit }
        $t;
    } or die $@} );
}
 
sub template(_;@) {
    local $pl::Echo = \&echo;
    &Template;
}
 
 
sub benchmark(&@) {
    my( $code, $name, @args ) = @_;
    local( $a, $b ) = ($a, $b);
    $name //= 'noname';
    require Benchmark;
    local $SIG{__WARN__} = sub { die @_ };
    if( @args ) {
        say Benchmark::timestr( Benchmark::countit( 10, $code )), " $name: $_"
            for @args;
    } else {
        say Benchmark::timestr( Benchmark::countit( 10, $code )), " $name";
    }
}
 
# Do it 100x for very fast test code (to increase weight compared to Benchmark overhead).
sub Benchmark(&@) {
    my( $code, @rest ) = @_;
    benchmark { for my $i (0..99) { &$code() }} @rest;
}
 
sub Config(@) {
    require Config;
    if( @_ ) {
        #{map { pairgrep { $a =~ /$_/ } %Config::Config } @_}; # needs 5.20
        my %config;
        while( my( $k, $v ) = each %Config::Config ) {
            $k =~ $_ and $config{$k} = $v, last
              for @_;
        }
        \%config;
    } else {
        \%Config::Config;
    }
}
 
sub Date(@) {
    my( $s, $us, $tz, $tzo );
    require Time::HiRes;
    for( @_ ) {
        if( ref ) {
            ($s, $us) = @$_;
        } elsif( /^([-+]?)(1[0-4]|0?[0-9])(?:\.([0-9])([0-9])?|:([0-5])?([0-9]))?$/i ) {
            $tz = $2 * 3600  +  ($3 ? ($3.($4//0)) * 36  :  $6 ? (($5//0).$6) * 60  :  0);
            $tzo = sprintf ' %s%02d:%02d', $1 || '+', $tz / 3600, $tz % 3600 / 60;
            $tz = -$tz if $1 eq '-';
        } elsif( /^([-+]?)(?=.)([0-9]*)(?:\.([0-9]*))?$/i ) {
            my $nus = substr +($3 // '') . '0'x6, 0, 6;
            if( $1 ) {
                ($s, $us) = Time::HiRes::gettimeofday() unless defined $s;
                if( $1 eq '+' ) {
                    $s += $2 // 0;
                    $us += $nus;
                    ++$s, $us -= 1e6 if $us >= 1e6;
                } else {
                    $s -= $2 // 0;
                    $us -= $nus;
                    --$s, $us += 1e6 if $us < 0;
                }
            } else {
                $s = $2 // 0;
                $us = $nus;
            }
        }
    }
    ($s, $us) = Time::HiRes::gettimeofday() unless defined $s;
    if( $pl::isodate ) {;
        my @time = reverse +(defined( $tz ) ? gmtime $s + $tz : localtime $s)[0..5];
        $s = sprintf '%04d-%02d-%02dT%02d:%02d:%02d.%06d%s', 1900 + shift( @time ), 1 + shift( @time ), @time, $us, $tzo // '';
    } else {
        $s = defined( $tz ) ? gmtime $s + $tz : localtime $s;
        substr $s, 19, 0, $tzo if $tzo;
        substr $s, 19, 0, sprintf '.%06d', $us // 0;
    }
    defined wantarray ? $s : say $s;
}
 
sub Isodate(@) {
    local $pl::isodate = 1;
    &Date;
}
 
# turns list of ipv4/6 addresses & hostnames or $_ into /etc/hosts format
# todo comment not found
sub hosts(@) {
    require Socket;
    package Socket;
    my( %res, %name );
    my $res = sub {
        my( $v4, $name, $aliases, undef, undef, @addr ) = @_;
        undef $name{$_} for $name, split ' ', $aliases;
        for( @addr ) {
            $_ = unpack 'H*', $_;
            if( $v4 ) { # make sortable by kind
                s/^(?=7f)/g/ or # loopback
                  s/^(?=a9fe)/i/ or # link local
                  s/^(?=0a|ac1|c0a8)/k/ or # private
                  substr $_, 0, 0, 'm';
            } else {
                s/^(?=0+1$)/h/ or # loopback
                  s/^(?=fe[89ab])/j/ or # link local
                  s/^(?=fd)/l/ or # private
                  substr $_, 0, 0, 'n';
            }
            @{$res{$_}}{keys %name} = (); # don't just store %name, different names might point to same IP but not vice versa
        }
    };
    my @unpack = (\&unpack_sockaddr_in6, \&unpack_sockaddr_in);
    for my $name ( @_ ? @_ : $_ ) {
        for( getaddrinfo( $name, undef, {socktype => SOCK_STREAM()} )) {
            next unless ref; # 1st is return code
            %name = ();
            undef $name{$_->{canonname}} if defined $_->{canonname};
            my $v4 = $_->{family} == AF_INET();
            my $addr = $unpack[$v4]->( $_->{addr} );
            my @get = gethostbyaddr( $addr, $_->{family} );
            &$res( $v4, @get ? @get : ($name, (undef)x3, $addr) );
        }
    }
    for( sort keys %res ) {
        next if 1 == length;    # IPv6 on old perl
        my $ip = pack 'H*', substr $_, 1;
        ::echo 4 == length $ip ? inet_ntoa( $ip ) : inet_ntop( AF_INET6(), $ip ),
          sort grep ! /^[0-9.]+$|^(?=.*:)[0-9a-f:]+$/i, keys %{$res{$_}};
    }
}
 
# Fill keydiff arrays
sub keydiff(;$$) {
    my $val;
    if( @_ > 1 ) {
        $val = $_[1];
    } else {
        chomp( $val = $_ );
    }
    $KEYDIFF{$_[0] // $1}[$ARGIND] = $val;
}
sub Keydiff(;$$) {
    my $key = $Static[$_[0] // 0];
    if( @_ > 1 ) {
        keydiff $key, $_[1];
    } else {
        keydiff $key;
    }
}
 
# trim small or big if negative values from hash (default %NUMBER) at any depth
sub Number(;$\[%@]) {
    my $n = $_[0] // 2;
    my $ref = $_[1] // \%NUMBER;
    if( 'HASH' eq ref $ref ) {
        for( keys %$ref ) {
        }
    } else {
        for( my $i = @$ref; $i >= 0; --$i ) {
        }
    }
}
 
# Pipe command to CODE
sub piped(&$@) {
    my $code = shift;
    open my $fd, "-|", @_ or die "$_[0]: $!\n";
    &$code() while <$fd>;
}
 
sub help(;$) {
    if( @_ && ! defined $_[0] ) {
        print <<\EOF;
usage: pl {-{BbeE} program} [-o] [-Vversion] [-perlopt...] [--] [main program] [arg ...]
  -Aprog            map prog over @A (or undef) returning new @A
  -bprog & -eprog   wrap begin/end program around each file in -n/-p...
  -Bprog & -Eprog   wrap begin/end program around program in same scope, my-vars work
  -o[number]        assume "for(@A) { ... }" loop around main program or array of number args
  -O[number]        like -o but use $A as loop var
  -p[number]        print on each loop (also -o/-O) iteration, at most number times
  -P[number]        like -p but print only if main program evaluates to true, like grep
  -r                reset "$." and -p/-P counter for each file
  -VVERSION         rerun with given version, which is just appended to "perl"
  --color[=when]    colorize the output; when can be 'never', 'always', or 'auto' (the default)
These options are handled by perl:
EOF
        piped { print if /^\s+-[0acCdDfFiIlmMntTuUvwWX]/ } $^X, '-h';
    }
    print <<\EOF;
Predefined functions:
  b { } name, arg... benchmark slow code for 10s, display name, looping over args.
  B { } name, arg... same but run code 100 times in benchmark, to reduce overhead.
  C re...            %Config, e.g. C->{sitelib}, optionally only part matching regexps
  D [arg...][, tz]   Date (from arg [s, us], s{.us}, offset [+-]s{.us}, tz ([+-]0-14{:mm|.ff})
  e arg...           echo prettified args or $_ with spaces and newline
  E arg...           same but no newline
  f fmt, arg...      format prettified args with newline, index can be "%1:" instead of "%1\$"
  F fmt, arg...      same but no newline
  h ip|name...       dns-lookup as hosts file
  I [arg...][, tz]   Isodate (from arg [s, us], s{.us}, offset [+-]s{.us}, tz ([+-]0-14{:mm|.ff})
  k [key, value]     store value or chomped $_ in $K{key or $1}[$I] for keydiff
  K [number, value]  same but key is $F[number] or $F[0]
  N [n, arg]         trim arg (default %N) values < n or > -n (default 2) e.g.; -EN or -E 'N -5'
  p { } cmd, arg...  open pipe from cmd and loop over it.
  t [drizzle, arg...]   fill args (key, value...|\%hash, default %T) into template or $_
  T [drizzle, arg...]   same but no newline
Predefined & magic variables:
  *A                 A, $A & @A are aliases to ARGV, $ARGV & @ARGV
  $I    0..n         index of ARG currently being processed in -o, -n or -p
  %K    ()[]         at end, sort by keys, print keydiff of $I array elements.  Filled by k.
  %N    ()           at end, sort numerically by values
  $q    '
  $Q    "
  *R    undef () {}  at end, print each, if defined or not empty, %RESULT sorted by keys
  %T    ()           Default template values, if npl given
EOF
}
 
 
# \todo help doc readline test:my
{
    package pl;
 
    our( %c, %template );
 
    # It's annoyingly hard to figure out if all are unique & lexically compatible numbers, or whether to sort textually.
    sub sort_keys(\%) {
        my $hash = $_[0];
        return () unless keys %$hash;
        goto TXT if $::TXT_SORT;
        my( $hex, $perl, $no_oct, %seen, @seen_oct ) = $::HEX_SORT;
        for( keys %$hash ) {
            goto TXT if /[^0-9a-fx._+-]/i;
            $hex = 0, last unless /^[0-9a-f](?:_?[0-9a-f]+)*$/i;
            goto TXT if exists $seen{hex $_};
            undef $seen{hex $_};
        }
        return sort { hex $a <=> hex $b } keys %$hash if $hex;
 
        %seen = ();
        for( keys %$hash ) {
            unless( $no_oct ||= /^[+-]?0(?=.*[89])/ ) {
                if( /^[+-]?0(?:b(?:_?[01]+)*|x(?:_?[0-9a-f]+)*)$/i ) {
                    goto TXT if exists $seen{eval $_};
                    undef $seen{eval $_};
                    $perl = 1, next;
                }
                if( /^[+-]?0(?:_?[0-7]+)*$/ ) {
                    push @seen_oct, $_; # can't decide yet
                    next;
                }
            }
 
            if( /^[+-]?(?=.)[0-9]*(?:\.[0-9]*)?(?:(?<=.)e[+-]?[0-9]+)?$/i ) {
                goto TXT if exists $seen{0 + $_};
                undef $seen{0 + $_};
                next;
            }
 
            goto TXT;
        }
        if( $perl ) {
            goto TXT if $no_oct;
            for( @seen_oct  ) {
                goto TXT if exists $seen{eval $_};
                undef $seen{eval $_};
            }
            return sort { eval $a <=> eval $b } keys %$hash;
        } else {
            for( @seen_oct  ) {
                goto TXT if exists $seen{0 + $_};
                undef $seen{0 + $_};
            }
            return sort { $a <=> $b } keys %$hash;
        }
 
      TXT:                              # clearest solution here
        sort keys %$hash;
    }
 
 
    sub keydiff() {
        goto &_keydiff if $c{E} && eval { require Algorithm::Diff };
        for my $key ( sort_keys %::KEYDIFF ) {
            $#{$::KEYDIFF{$key}} = $ARGIND - 1; # lengthen list if needed
            my $str;
            next if ::all { ($str ||= $_) eq $_ if defined } @{$::KEYDIFF{$key}};
            say "$c{B}$key$c{E}";
            my( $begin, $end );
            if( $c{E} ) {
                for( @{$::KEYDIFF{$key}} ) {
                    if( !defined ) {
                    } elsif( defined $end ) {
                        chop $begin while length $begin and !/\A\Q$begin/;
                        #substr( $begin, -1 ) = '' until !length $begin or /^\Q$begin/;
                        substr( $end, 0, 1 ) = '' while length $end and !/\Q$end\E\Z/;
                    } else {
                        $begin = $end = $_;
                    }
                }
                $begin = length $begin;
                $end = length $end;
                #$_ = length for $begin, $end;
            }
            say "\t",
              !defined() ? "$c{I}n/a$c{E}" :
              !defined( $end ) ? $_ :
              $begin == length() ? "$c{G}$_$c{E}" :
 
              ($begin ? $c{G} . substr( $_, 0, $begin ) : '') .
              $c{R} . substr( $_, $begin, length() - $begin - $end ) .
              ($end ? $c{G} . substr $_, length() - $end : '') .
              $c{E}
 
              for @{$::KEYDIFF{$key}};
        }
    }
    sub _keydiff() {
        for my $key ( sort_keys %::KEYDIFF ) {
            my( $max, $n, $ref ) = (0, 0);
            for( @{$::KEYDIFF{$key}} ) {
                next unless defined;
                $max = length if $max < length;
                ++$n;
                if( $ref ) {
                    $ref = Algorithm::Diff::LCS( $ref, [split //] );
                } else {
                    $ref = [split //];
                }
            }
            next if $n == $ARGIND && @$ref == $max;
            say "$c{B}$key$c{E}";
            $#{$::KEYDIFF{$key}} = $ARGIND - 1; # lengthen list if needed
            for( @{$::KEYDIFF{$key}} ) {
                if( defined ) {
                    if( ! @$ref ) {
                        substr $_, 0, 0, $c{R};
                    } elsif( @$ref == length ) {
                        substr $_, 0, 0, $c{G};
                    } else {
                        my( undef, $idx ) = Algorithm::Diff::LCSidx( $ref, [split //] );
                        for my $i ( reverse @$idx ) {
                            substr $_, $i + 1, 0, $c{R};
                            substr $_, $i, 0, $c{G};
                        }
                        substr $_, 0, 0, $c{R};
                        s/\e\[3.m(?=\e\[3.m|$)//g;
                        () while s/(\e\[3.m)[^\e]+\K\1//;
                    }
                } else {
                    $_ = "$c{I}n/a";
                }
                say "\t$_$c{E}";
            }
        }
    }
 
    sub getline {
        { local $\ = ''; print STDERR defined() ? '>> ' : '> ' }
        my $part = <>;
        if( defined $part ) {
            if( defined ) {
                $_ .= "\n" . $part;
            } else {
                $_ = $part;
            }
        } else {
            say '';
            exit;
        }
    }
    sub shell {
        # No my, as that would inject into eval.
        our $lp = eval { require Lexical::Persistence; Lexical::Persistence->new() };
        while( 1 ) {
            our $lines = undef;
            for( $lines ) {
                &getline;
                &getline while s/\\$//s;
                if( /\{$/s ) {
                    &getline until /\n\}$/s;
                }
            }
            {
                package main;
                $lp ? $lp->do( $lines ) : eval $lines;
            }
            warn $@ if $@;
        }
    }
}
 
# \todo help doc readline test:my
 
# Put this before compiling caller's main program, as these get run LIFO.
END {
    echo for $RESULT // (), @RESULT;
    form "%s:  %s", $_, $RESULT{$_}
        for pl::sort_keys %RESULT;
 
    # todo lenint.lenfloat %d/f
    printf "%8d: %s\n", $NUMBER{$_}, $_
        for sort { $NUMBER{$a} <=> $NUMBER{$b} } pl::sort_keys %NUMBER;
    &pl::keydiff if keys %KEYDIFF;
}
 
################################################################ 
 
BEGIN {
    $pl::Echo = \&Echo;
    *I = \$ARGIND;
    *Static = \@::F;
    eval '*' . substr( $_, 1, 1 ) . " = \\$_" for
      qw'*ARGV &benchmark &Benchmark &Config &Date &echo &Echo &form &Form &hosts $HEX_SORT &Isodate &keydiff &Keydiff %KEYDIFF
         &Number %NUMBER &piped $quote $Quote *RESULT &template &Template %TEMPLATE $TXT_SORT';
 
    ($ARGIND, $quote, $Quote, $H) = (0, "'", '"', 1);
    unless( @pl::opts_values || @ARGV ) {
        *pl::prog = \&pl::shell;
        return;
    }
 
    {
        # Assemble a program that works under perl -n, etc., while adding in pl's options.
        # This is wild stuff, as it has to blend in various options, while potentially accommodating an outer loop.
        my @prog = "use feature ':'.substr \$^V, 1;\n"; # Enable latest optional features.
        @prog[2, 4, 11, 13, 22] =
          ('sub pl::prog { $pl::last = 1;',
           'LINE: {',           # dummy loop
           '} continue {',              # program didn't do last
           '$pl::last = 0 }',
           '}');
        while( @pl::opts_values ) {    # no for, need to shift
            $_ = shift @pl::opts_values;
            if( /^--color(?:()|=(a)lways|=(n)ever|=auto)$/s ) {
                $pl::c = defined( $1 ) || $2 ? 1 : $3 ? 0 : undef;
                next;
            } elsif( /[?h]/ ) {
                help undef;
                exit;
            } elsif( /v/ ) {
                say 'This is ' . ($0 =~ s!.*[/\\]!!r) . " $pl::VERSION, with perl $^V
 
Copyright 1997-2023, Daniel Pfeiffer
 
Pl may be copied only under the terms of either the Artistic License or the
GNU General Public License, which may be found in the Perl 5 source kit.";
                exit;
            }
 
            if( /([ABbEe])/ ) {
                # put ; after \n as user may end with a comment
                my $bit = "\n#line 1 \"-$1 option\"\n$pl::opts_values[0]\n;";
                my $block = shift( @pl::opts_values ) =~ /\A\{.+\}\Z/s;
                if( /A/ ) {
                    chop $bit;
                    $prog[0] .= '@ARGV = ' . ($block ? "grep $bit" : "map { $bit }") . " \@ARGV ? \@ARGV : undef;\n";
                } elsif( /b/ ) {
                    # Do it each time ARGV changes
                    $prog[5] = 'if( ($pl::exARGV //= "") ne $ARGV ) {' . $bit . '$pl::exARGV = $ARGV; }';
                } elsif( /B/ ) {
                    $prog[0] .= $block ? "BEGIN$bit" : $bit;
                } elsif( /e/ ) {
                    # append 1 to not leak any wantarray into $bit
                    $prog[8] = "\nINIT { \$pl::_e = sub { $bit 1 }}";
                    $pl::_e = '&$pl::_e();';
                } else {
                    $prog[9] = "\n;END { $bit 1 }";
                }
            }
            if( /o([1-9][0-9]*)?/i ) {
                if( my $o = $1 ) {
                    @prog[3, 14] =
                      ('while( @ARGV > $ARGIND ) { sub {' . (/o/ ? '$_' : '$ARGV') . ' = \@_ }->( @ARGV[$ARGIND..$ARGIND+' . ($o - 1) . '] );', # hack to ref into @ARGV
                       '} continue { $ARGIND += ' . $o . '; last if $pl::last }')
                } else {
                    @prog[3, 14] =
                      (/O/ ? 'for $ARGV (@ARGV) {' : 'for(@ARGV) {',
                       '} continue { ++$ARGIND; last if $pl::last }');
                }
            }
            if( /p([1-9][0-9]*)?/i ) {
                my $p = $1;
                $prog[1] = '$pl::_pn = 0;';
                @prog[6, 10] =
                  ('$pl::_P = do {',
                   '}') if /P/;
                $prog[12] = '++$pl::_pn, print or die "-p destination: $!\n"' . (/P/ ? 'if $pl::_P;' : ';');
                $prog[13] =~ s/0/\$pl::_pn >= $p ? 2 : 0/ if $p;
            }
            $pl::_r = 1 if /r/;
        }
        $prog[7] = @ARGV ? "\n#line 1 \"main program\"\n" . shift() . "\n;" : ';';
        #$prog[13] =~ s/\$pl::last =( \$pl::_pn >= \d+)/ if($1 ) { &\$pl::_e() if \$pl::_e; exit }/ unless $pl::_r || $prog[14];
        $prog[14] //=           # ! -[oO]
          'if( $pl::last || eof ) { ++$ARGIND;' .
            ($pl::_r ? 'close ARGV; $pl::_pn = 0;' : 'if( $pl::last ) { my $d = $.; close ARGV; $. = $d }') .
              ($pl::_e // '') .
                ($pl::_r ? '}' : 'exit if $pl::last == 2 }');
        # Don't pollute eval with my-vars
        $_ = join '', grep defined, @prog;
    };
    #no warnings 'experimental';        # overridden by -W
    no strict;
    if( ($ENV{PLDUMP} // 0) == 1 ) { open STDOUT, '| perltidy -cb'; say; exit }
    eval;
    if( $@ ) {
        warn $@;
        exit 255;
    }
    undef $_;
    @pl::c{qw(B I G R E)} = $pl::c // (-t STDOUT && $^O !~ /^MSWin/) ?
      map "\e[${_}m", 1, 3, 32, 31, '' :
      ('')x5;
 
    package Data::Dumper;
    our( $Deparse, $Quotekeys, $Terse, $Sortkeys ) = (1, 0, 1, sub { [&pl::sort_keys] });
}
&pl::prog; # will be called repeatedly if -n
 
 

 
=head1 NAME
 
pl - Perl pl-Liner Magic Wand
 
=head1 SYNOPSIS
 
Some tasks are too menial for a dedicated script but still too cumbersome
even with the many neat pl-liner options of C<perl -E>.  This small script
fills the gap: various pl-letter commands & magic variables (with meaningful
aliases too) and more nifty loop options take Perl programming to the command
line.  Fully imports C<List::Util>.  With no program on the command line,
starts a pl Shell.
 
How to C<e(cho)> values, including from C<@A(RGV)>, with single C<$q(uote)> &
double C<$Q(uote)>:
 
    pl 'echo "${quote}Perl$quote", "$Quote@ARGV$Quote"' pl-liner
    pl 'e "${q}Perl$q", "$Q@A$Q"' pl-liner
 
    >   'Perl' "pl-liner"
 
Same for hard-to-print values:
 
    pl 'echo \"Perl", \@ARGV, undef' pl-liner
    pl 'e \"Perl", \@A, undef' pl-liner
 
    >   \'Perl' [
    >     'pl-liner'
    >   ] undef
 
Loop over args, printing each with line ending.  And same, SHOUTING:
 
    pl -opl '' Perl pl-liner
    pl -opl '$_ = uc' Perl pl-liner
 
    >   Perl
    >   pl-liner
    >   PERL
    >   pl-LINER
 
Print up to 3 matching lines, resetting count (and C<$.>) for each file:
 
    pl -rP3 '/Perl.*pl.*liner/' file*
 
Count hits in magic statistics hash C<%N(UMBER)>:
 
    pl -n '++$NUMBER{$1} while /(Perl|pl|liner)/g' file*
    pl -n '++$N{$1} while /(Perl|pl|liner)/g' file*
 
    >          2: pl
    >          7: liner
    >          9: Perl
 
Though they're sometimes slightly, sometimes quite a bit more complicated,
most Perl pl-liners from the internet work, just by omitting C<-e> or C<-E>.
There's only pl main program in pl, but you can just as well concatenate the
C<-e>s with C<;>.  See L<minor differences|/"Minor Differences with perl -E">
for exceptions.  Let's see L<many varied examples|https://metacpan.org/dist/App-pl/view/pod/examples.pod>.
 
=head1 DESCRIPTION
 
I<Don't believe everything you read on SourceForge^H^H^H the internet! --B< >Plato :-y>
 
Pl follows Perl's philosophy for pl-liners: the pl variable solely used in
pl-liners, C<@F>, is single-lettered.  Because not everypl may like that, pl
has it both ways.  Everything is aliased both as a word and as a single
letter, including Perl's own C<@F> & C<*ARGV>.
 
Perl pl-liners, and hence pl, are by nature bilingual.  You must run the
command with its options & arguments, typically from Shell.  By design, Perl
quotes mimic Shell quotes, so here they collide.  As Perl also uses Shell
meta-characters like C<$>, the best solution is to protect Perl-code from the
Shell with single quotes.  That means you can't use them inside.  (An ugly way
around that, is C<'\''>, which ends a string, backslashes a quote and starts
another.)  For literal quotes use C<$q(uote)>.  For quoting use double quotes
or C<q{}>.
 
I<Shell and Perl, unlike most other languages, don't make you stick your toe
up your nose to get newlines into strings.  Thus, you see long "pl-liners" as
legible many-liners.  You get more features on the L<pl
homepage|https://perl1liner.sourceforge.io/>, like in the veggie-burger menu,
you can toggle many-line display.  In normal text short and long name variants
are initial-bold as C<X(YZ)>.  All examples use the long names, if applicable.
On the homepage those are in the darker blue upper half.  They are repeated with the short variant.  Many examples are followed by their output, indented with C<< > >>.>
 
=head1 DOCUMENTATION
 
=head2 Options
 
Many of perl's options are also available in pl, sometimes enhanced with extra
functionality.  And the new options complement what perl offers, specifically
oriented towards pl-liners.
 
=over
 
=item C<-0I<[octal]>>
 
I<perl:> Specify record separator with B<-n>/B<-p> (C<\0>, if no argument).
 
=item C<-AI<prog>>
 
Map program over already available C<@A(RGV)> (from command line or previous
B<-A>) or undef.  If you wrap program in C<{}> uses C<grep> instead of C<map>.
The result becomes the new C<@A(RGV)>.  You can mix it with B<-B>.  The 1st
two are equivalent, except that the 1st pl isn't limited by Shell line length
limitations.  The third again greps by file size, reading only the Perl
modules less than 1E<thinsp>kB:
 
    pl -nA '<*.pm>' '...'
    pl -n '...' *.pm
    pl -nA '<*.pm>' -A '{ (stat)[7] < 1000 }' '...'
 
=item C<-a>
 
I<perl:> Autosplit mode with B<-n>/B<-p> (splits C<$_> into C<@F(IELD)>).
 
=item C<-bI<prog>>
 
Run program before reading a new file in B<-n>/B<-P>/B<-p>.
 
=item C<-BI<prog>>
 
Add program before main program in same scope.  You can use it to initialise
C<my> variables.  Whereas, if you define a C<my> variable in the main program
of a B<-n>, B<-p>, B<-P>, B<-o>, or B<-O> loop, that's a new variable on each
iteration.  This doesn't do a C<BEGIN> block unless you wrap program in C<{}>.
You may mix it with B<-A>.
 
=item C<-c>
 
I<perl:> Check syntax only (runs C<BEGIN> and C<CHECK> blocks).
 
=item C<-CI<[number/list]>>
 
I<perl:> Enables the listed Unicode features.
 
=item C<--colorI<[=when]>>
 
Colorize (people with impairment may adapt their system, terminal, or browser)
some of the output; when can be C<never>, C<always>, or C<auto> (the default).
 
=item C<-dI<[:debugger]>>
 
I<perl:> Run program under debugger.
 
=item C<-DI<[number/list]>>
 
I<perl:> Set debugging flags (argument is a bit mask or alphabets).
 
=item C<-eI<prog>>
 
Run program after finishing reading a file in B<-n>/B<-p>.
 
=item C<-EI<prog>>
 
Add an C<END> block after main-program in same scope.  So, C<my>-vars work as
follows: the C<END> block is a closure of the 1st C<$inner> variable.  Perl
warns "Variable "$inner" will not stay shared":
 
    pl -OB 'my $outer' -E 'echo $inner, $outer' 'my $inner = $outer = $ARGV' a b c
    pl -OB 'my $outer' -E 'e $inner, $outer' 'my $inner = $outer = $A' a b c
 
    >   a c
 
=item C<-f>
 
I<perl:> Don't do F<$sitelib/sitecustomize.pl> at startup.
 
=item C<-FI</pattern/>>
 
I<perl:> Provide C<split()> pattern for B<-a> switch (C<//>'s are optional).
 
=item C<-II<directory>>
 
I<perl:> Specify C<@INC>/C<#include> directory (several B<-I>'s allowed).
 
=item C<-iI<[extension]>>
 
I<perl:> Edit C<< <> >> files in place (makes backup if extension supplied).
 
=item C<-n>
 
I<As I said before, I never repeat myself. :-)>
 
I<perl:> Assume C<< while (<>) { ... } >> loop around program.  It's a little
richer than that: if you use C<last>, it closes the current file, leaving you
to continue the loop on the next file.
 
=item C<-oI<[number]>>
 
Assume C<for(@ARGV) { ... }> loop around main program, and C<$ARGIND> (or
C<$I>) is the current position.  In this case B<-p> doesn't imply B<-n>.  If
you give number, passes that many args at once as an array, referencing the
original values.  If there aren't enough on the last round, fills up
C<@A(RGV)> with C<undef>s.
 
    pl -opl '' I II III IV
    pl -o3 'echo $ARGIND, @$_' i ii iii iv v vi vii viii ix
    pl -opl '' I II III IV
    pl -o3 'e $I, @$_' i ii iii iv v vi vii viii ix
 
    >   I
    >   II
    >   III
    >   IV
    >   0 i ii iii
    >   3 iv v vi
    >   6 vii viii ix
 
=item C<-OI<[number]>>
 
like B<-o> but use C<@A(RGV)> as loop variable.
 
=item C<-pI<[number]>>
 
I<Does C<pl -penis> do pussy?  B< >It implements C<cat>. :-*>
 
I<perl+:> On each loop C<print> (also B<-o> and B<-O>, in which case you must
fill C<$_>) iteration.  If you give number, prints at most number times.
 
=item C<-PI<[number]>>
 
Like B<-p> but print only if main program evaluates to true, like C<grep>.
 
=item C<-r>
 
Reset C<$.> and B<-p>/B<-P> counter for each file.
 
=item C<-T>
 
I<perl:> Enable tainting checks.
 
=item C<-t>
 
I<perl:> Enable tainting warnings.
 
=item C<-U>
 
I<perl:> Allow unsafe operations.
 
=item C<-u>
 
I<perl:> Dump core after parsing program.
 
=item C<-v>
 
I<perl:> Print version, patchlevel and license.
 
=item C<-VI<version>>
 
Rerun with given perl version, which is just a string appended to F<perl>.
 
=item C<-W>
 
I<perl:> Enable all warnings.
 
=item C<-w>
 
I<perl:> Enable many useful warnings.
 
=item C<-X>
 
I<perl:> Disable all warnings.
 
=back
 
=head2 Functions
 
Various functions, always also with a pl letter alias, perform little tasks
that can be useful in pl-liners.
 
=over
 
=item C<benchmark { } I<[name[, arg...]]>> |
C<b { } I<[name[, arg...]]>>
 
Benchmark slow code for 10E<thinsp>s, display name, looping over args.
 
=item C<Benchmark { } I<[name[, arg...]]>> |
C<B { } I<[name[, arg...]]>>
 
Same but run code 100 times in benchmark, to reduce overhead.
 
=item C<Config I<[regexp...]>> |
C<C I<[regexp...]>>
 
Import and return C<%Config>, e.g., C<< Config->{sitelib} >>, optionally only part matching regexps.
 
=item C<Date I<[arg...][, tz]>> |
C<D I<[arg...][, tz]>>
 
I<Why is Halloween Christmas?  Because Oct 31 = Dec 25. (^)>
 
Date (from arg [s, us], s{.us}, offset [+-]s{.us}, tz ([+-]0-14{:mm|.ff}).
You should pass microseconds as strings because floats have
implementation-dependent rounding issues.  You must pass positive offsets as
strings because otherwise it loses the C<+>.  Returns the date, if called in
some context, else echoes it.
 
    pl 'Date;
        $_ = Date -86400, "+3600";
        echo $_, " -- ", Date "+8:45"'
    pl 'D;
        $_ = D -86400, "+3600";
        e $_, " -- ", D "+8:45"'
 
    >   Thu Jul 13 20:24:51.009836 2023
    >   Wed Jul 12 21:24:51.009956 2023  --  Fri Jul 14 03:09:51.009994 +08:45 2023
 
=item C<echo I<[arg...]>> |
C<e I<[arg...]>>
 
Echo prettified args or C<$_> with spaces and newline.  Prettified means,
C<undef> becomes that string, italic if B<--color> is active.  Anything that
can be stringified, is.  Any other reference goes through C<Data::Dumper>,
which pl loads only if needed.
 
If it's called in scalar context (e.g., C<$x = echo ...>) instead return the
same as it would echo, in pl string (inspired by Shell C<$(...)>).  If it's
called in list context (e.g., C<@l = echo ...>) return each arg prettified
individually, with a newline on the last pl.
 
=item C<Echo I<[arg...]>> |
C<E I<[arg...]>>
 
Same but no newline.
 
=item C<form I<format, [arg...]>> |
C<f I<format, [arg...]>>
 
Form(at) and echo prettified args or C<$_> with newline.  If it's called in
scalar or list context (e.g., C<$x = form ...>) instead return the same as it
would echo, in pl string.  Parameter index can be C<"%1:"> instead of
C<"%1\$">.
 
=item C<Form I<format, [arg...]>> |
C<F I<format, [arg...]>>
 
Same but no newline.
 
=item C<Isodate I<[arg...][, tz]>> |
C<I I<[arg...][, tz]>>
 
Same as C<D(ate)> but uses ISO format.
 
    pl 'Isodate;
        $_ = Isodate 7 * -86400;
        echo $_, " -- ", Isodate "+8.75"'
    pl 'I;
        $_ = I 7 * -86400;
        e $_, " -- ", I "+8.75"'
 
    >   2023-07-13T20:24:51.124180
    >   2023-07-06T20:24:51.124266  --  2023-07-14T03:09:51.124298 +08:45
 
=item C<keydiff I<[key[, value]]>> |
C<k I<[key[, value]]>>
 
Store value or chomped C<$_> in C<$KEYDIFF{key or $1}[$ARGIND]>.  At the
C<END> for each key (which pl sorts numerically if possible) pl diffs all
values.
 
=item C<Keydiff I<[number[, value]]>> |
C<K I<[number[, value]]>>
 
Same but key is C<$Static[number]> or C<$F[0]>.
 
=item C<Number I<[n[, hash_or_array]]>> |
C<N I<[n[, hash_or_array]]>>
 
Trim hash_or_array (default C<%N(UMBER)>) values less than C<n> (default 2),
or, if negative, more than C<-n> e.g., C<B<-E>N> or C<B<-E> 'Number -5,
@RESULT'>. This happens recursively at any depth in nested hashes or arrays.
 
The first argument can also be a function, where deletion happens for every
element where it returns a falsy value. It gets called for each scalar element
with 3 arguments: 0 - the current (nested) hash or array, 1 - its key or
index, 2 - its value.
 
    pl '%RESULT = (neg => [-9..-1], pos => [1..9]); Number sub { ! ($_[2] % 3) }, %RESULT'
    pl '%R = (neg => [-9..-1], pos => [1..9]); N sub { ! ($_[2] % 3) }, %R'
 
    >   neg:  [
    >     -9,
    >     -6,
    >     -3
    >   ]
    >   pos:  [
    >     3,
    >     6,
    >     9
    >   ]
 
=item C<piped { } I<cmd[, arg...]>> |
C<p { } I<cmd[, arg...]>>
 
Open pipe from cmd and loop over it.
 
=item C<template I<< [drizzle[, hash|key => value...]] >>> |
C<t I<< [drizzle[, hash|key => value...]] >>>
 
Replace values from hash in template (defaults to C<$_>), which may also be a
filehandle of ref to a filename.  Hash (defaults to C<%T(EMPLATE)>) may be
given as a reference or key-value pairs.  The template may use pl of three
markup styles: C<[% I<x> %]>, C<{{ I<x> }}>, or C<< <?pl I<x> ?> >>.  These
are totally equivalent.  The 1st pl found in the template will be used.  If
you put a C<~> just inside a delimiter (e.g., C<< <?pl~ I<x> ~?> >>), it will
gobble horizontal whitespace on that side, behind also pl newline.
 
Within the markup I<x> may be any valid syntax for a Perl hash key.  The 3
characters C<|>, C<:>, or C<!> mark the end of the key name and introduce 3
kinds of filter.  The 1st two, can be C<?|> or C<?:>, meaning this item
applies only if the key exists.  Otherwise the key is optional.  If you give a
key, its value is locally assigned to C<$_>.  If C<undef> it defaults to
C<''>.
 
You write the filter in Perl.  You can abort the filter with C<last>.  If the
filter starts with C<|> then you pipe its scalar value into the template.  If
there is no filter after C<|>, you recursively treat the value as a template.
If the filter starts with C<:> then you insert the value of C<$_>.  An easy
way to only do that is C<[%:%]>.  The whole template gets compiled to an
anonymous C<sub>, the 1st time.  As this happens inside the function, filters
have no access to your surrounding C<my> variables.
 
You insert the Perl code following C<!> into the code the template compiles to
verbatim.  This is useful for flow control.  E.g., if C<$TEMPLATE{x}> (or
C<$T{x}>) is an array, you can loop over its values as C<@$_>.  The loop logic
is entirely in Perl, which again localizes C<$_>.  If do-nothing C<[%!%]>
starts the document, it "declares" its markup syntax.  And with C<~> it
suppresses surrounding space including pl following newline.  (See
L<PLDUMP|/$ENV{PLDUMP}>)
 
    pl 'template q(A: {{ a }} B: {{ | "no" }}{{ b ?| "yes" }} C: {{ lc "C" }} C+1: {{ c|$_ + 1}}), qw(a 1 c 3)'
    pl 'template q([%x!for( @$_ ) { %] X: [% : %] [%~ ! } %][% y %]), { x => [1..5] }'
    pl 'template q(<ul><?pl l!for( @$_ ) { ?> <li><?pl:?></li><?pl ! } ?> </ul>), { l => [1..3] }'
    pl 't q(A: {{ a }} B: {{ | "no" }}{{ b ?| "yes" }} C: {{ lc "C" }} C+1: {{ c|$_ + 1}}), qw(a 1 c 3)'
    pl 't q([%x!for( @$_ ) { %] X: [% : %] [%~ ! } %][% y %]), { x => [1..5] }'
    pl 't q(<ul><?pl l!for( @$_ ) { ?> <li><?pl:?></li><?pl ! } ?> </ul>), { l => [1..3] }'
 
    >   A: 1 B: no C: 3 C+1: 4
    >    X: 1 X: 2 X: 3 X: 4 X: 5
    >    1 2 3 
 
=item C<Template I<< [drizzle[, hash|key => value...]] >> > |
C<T I<< [drizzle[, hash|key => value...]] >> >
 
Same but no newlines, also not on nested templates.
 
    pl '$_ = q( [ <?pl~inner|~?> ]);
        $TEMPLATE{inner} = q([ {{ who | $_ || "Jack" }} in the box ]);
        Template;
        $TEMPLATE{who} = "Sue";
        template'
    pl '$_ = q( [ <?pl~inner|~?> ]);
        $T{inner} = q([ {{ who | $_ || "Jack" }} in the box ]);
        T;
        $T{who} = "Sue";
        t'
 
    >    [[ Jack in the box ]] [[ Sue in the box ]
    >   ]
 
=back
 
=head2 Variables
 
Various variables, always also with a pl letter alias, often perform magic
tasks at the C<END>.
 
=over
 
=item C<*ARGV> |
C<*A>
 
I<perl:> C<ARGV>, C<$ARGV> & C<@ARGV> are all aliased to C<A>, C<$A> & C<@A>.
 
=item C<$ARGIND> |
C<$I>
 
Index of ARG which B<-o>, B<-n>, or B<-p> loop is currently processing.
 
=item C<@Static> |
C<@F>
 
I<perl:> This is an alias to loop autosplit variable C<@F>.
 
=item C<$quote> |
C<$q>
 
Predefined to a single quote C<'> without any magic.  Perl's C<q()> makes it
easy to integrate functional quotes under all circumstances.  This does the
same for literal quotes.
 
=item C<$Quote> |
C<$Q>
 
Predefined to a double quote C<"> without any magic.  Perl's C<qq()> makes it
easy to integrate functional quotes under all circumstances.  This does the
same for literal quotes.
 
=item C<%KEYDIFF> |
C<%K>
 
At END, sort by keys, print keydiff of C<$ARGIND> array elements.  Filled by
C<k(eydiff)>.
 
=item C<%NUMBER> |
C<%N>
 
At END, sort numerically by values.
 
=item C<*RESULT> |
C<*R>
 
At END, echo C<$RESULT> if defined, then C<@RESULT> pl per line if not empty,
then C<%RESULT> sorted by keys.
 
=item C<$ENV{PLDUMP}>
 
Since C<pl -MO=Deparse> won't show your parts of the program, it can be quite
baffling when things go wrong.  If you export this with value 1 before
starting pl, you see how your parts get embedded in various bits of generated
stuff.  If you install C<perltidy>, pl will use it.  Some options get handled
by perl, so they won't show up here:
 
    PLDUMP=1 \
        pl 'say "Hello Perld!"'
 
    >   use feature ':' . substr $^V, 1;
    >   
    >   sub pl::prog {
    >       $pl::last = 1;
    >     LINE: {
    >   #line 1 "main program"
    >           say "Hello Perld!";
    >       } continue {
    >           $pl::last = 0;
    >       }
    >       if ( $pl::last || eof ) {
    >           ++$ARGIND;
    >           if ($pl::last) { my $d = $.; close ARGV; $. = $d }
    >           exit if $pl::last == 2;
    >       }
    >   }
 
If you export this with value 2, it will instead show what a template would
compile to:
 
    PLDUMP=2 \
        pl 'template q([%x!for( @$_ ) {%] X: [% : %] [%~!}%]), x => [1..5]'
    PLDUMP=2 \
        pl 't q([%x!for( @$_ ) {%] X: [% : %] [%~!}%]), x => [1..5]'
 
    >   my @_template = [
    >     '',
    >     ' X: ',
    >     ''
    >   ] ;
    >   #line 1 "template"
    >   sub { my $_template = $_template[0]
    >   ; for((local $_ = $TEMPLATE{x} // '')?():(), @$_ ) {
    >   ;$_template .= $_template[1] . (do {{ 
    >   ; $_ }} // '');$_template .= $_template[2]
    >   ; }
    >   ;
    >   ; $_template }
 
=back
 
=head1 COMPATIBILITY
 
Even if it's rare nowadays, you can still find Perl 5.16 out in the wild
(e.g., in RHEL 7).  Pl accommodates it gracefully, falling back to what works.
It has shims for C<any>, C<all>, C<npl>, C<notall>, C<product> & C<sum0>.
(Some Unices maintain even older Perl versions, e.g., AIX or Solaris: you can
go back till Perl 5.10 with L<pl
0.63.2|https://sourceforge.net/projects/perl1liner/files/App-pl-v0.63.2.tgz/download>.)
 
=head2 Minor Differences with perl -E
 
Known minor differences are:
 
=over
 
=item *
 
by design in an B<-n> loop C<last> is per file instead of behaving like C<exit>
 
=item *
 
don't C<goto LINE>, but C<next LINE> is fine
 
=item *
 
using C<pop>, etc. to implicitly modify C<@A(RGV)> works in B<-B> begin code
but not in your main program (which gets compiled to a function)
 
=item *
 
shenanigans with unbalanced braces won't work
 
=back
 
=head2 Windows Notes
 
I<B<W>ork B<I>s B<N>ever B<D>pl B<O>n B<W>indows B<S>ystems ;-)>
 
Do yourself a favour and get a real Shell, e.g., from
L<Cygwin|https://cygwin.com/>, L<git|https://gitforwindows.org>,
L<MinGW|https://mingw.osdn.io/>,
L<MSYS|https://sourceforge.net/p/mingw-w64/wiki2/MSYS/>, or
L<WSL|https://docs.microsoft.com/en-us/windows/wsl/about>!  If you can't avoid
F<command.com> or F<cmd.exe>, you will have to first convert all inner quotes
to C<qq> or C<\">.  Then convert the outer single quotes to double quotes:
 
    pl "echo qq{${quote}Perl$quote}, \"$Quote@ARGV$Quote\"" pl-liner
    pl "e qq{${q}Perl$q}, \"$Q@A$Q\"" pl-liner
 
    >   Perl pl-liner
 
PowerShell is weirder.  (Did I mention you'd be better off with a real Shell?)
You must use outer single quotes, but you still need to protect double quotes:
 
    pl 'echo qq{${quote}Perl$quote}, \"$Quote@ARGV$Quote\"' pl-liner
    pl 'e qq{${q}Perl$q}, \"$Q@A$Q\"' pl-liner
 
    >   Perl pl-liner
 
While the old Windows 10 terminal understands ANSI escape sequences, it makes
it horribly hard to activate them.  Therefore, they're off by default, requiring
B<--color> to override that choice.
 
=for html <hr>
 
Pl is on L<SourceForge|https://perl1liner.sourceforge.io/> and also available
on L<meta::cpan|https://metacpan.org/dist/App-pl>.