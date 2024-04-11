#!/bin/sh
 
#############################################################################
# configuration to fill in (or to replace in your .staticperlrc)
 
STATICPERL=~/.staticperl
CPAN=http://mirror.netcologne.de/cpan # which mirror to use
EMAIL="read the documentation <rtfm@example.org>"
DLCACHE=
 
# perl build variables
MAKE=make
PERL_VERSION=http://stableperl.schmorp.de/dist/latest.tar.gz # 5.12.5 and 5.8.9 are good choices for small builds
PERL_CC=cc
PERL_CONFIGURE="" # additional Configure arguments
PERL_CCFLAGS="-g -DPERL_DISABLE_PMC -DPERL_ARENA_SIZE=16376 -DNO_PERL_MALLOC_ENV -D_GNU_SOURCE -DNDEBUG"
PERL_OPTIMIZE="-Os" # -Os -ffunction-sections -fdata-sections -finline-limit=8 -ffast-math"
 
ARCH="$(uname -m)"
 
#case "$ARCH" in
#   i*86 | x86_64 | amd64 )
#      PERL_OPTIMIZE="$PERL_OPTIMIZE -mpush-args -mno-inline-stringops-dynamically -mno-align-stringops -mno-ieee-fp" # x86/amd64
#      case "$ARCH" in
#         i*86 )
#            PERL_OPTIMIZE="$PERL_OPTIMIZE -fomit-frame-pointer -march=pentium3 -mtune=i386" # x86 only
#            ;;
#      esac
#      ;;
#esac
 
# -Wl,--gc-sections makes it impossible to check for undefined references
# for some reason so we need to patch away the "-no" after Configure and before make :/
# --allow-multiple-definition exists to work around uclibc's pthread static linking bug
#PERL_LDFLAGS="-Wl,--no-gc-sections -Wl,--allow-multiple-definition"
PERL_LDFLAGS=
PERL_LIBS="-lm -lcrypt" # perl loves to add lotsa crap itself
 
# some configuration options for modules
PERL_MM_USE_DEFAULT=1
PERL_MM_OPT="MAN1PODS= MAN3PODS="
#CORO_INTERFACE=p # needed without nptl on x86, due to bugs in linuxthreads - very slow
#EV_EXTRA_DEFS='-DEV_FEATURES=4+8+16+64 -DEV_USE_SELECT=0 -DEV_USE_POLL=1 -DEV_USE_EPOLL=1 -DEV_NO_LOOPS -DEV_COMPAT3=0'
export PERL_MM_USE_DEFAULT PERL_MM_OPT
 
# which extra modules to install by default from CPAN that are
# required by mkbundle
STATICPERL_MODULES="ExtUtils::MakeMaker ExtUtils::CBuilder common::sense Pod::Strip PPI PPI::XS Pod::Usage"
 
# which extra modules you might want to install
EXTRA_MODULES=""
 
# overridable functions
preconfigure()  { : ; }
patchconfig()   { : ; }
postconfigure() { : ; }
postbuild()     { : ; }
postinstall()   { : ; }
 
# now source user config, if any
if [ "$STATICPERLRC" ]; then
   . "$STATICPERLRC"
else
   [ -r /etc/staticperlrc ] && . /etc/staticperlrc
   [ -r ~/.staticperlrc   ] && . ~/.staticperlrc
   [ -r "$STATICPERL/rc"  ] && . "$STATICPERL/rc"
fi
 
#############################################################################
# support
 
# work around ExtUtils::CBuilder and others
export CC="$PERL_CC"
export CFLAGS="$PERL_CFLAGS"
export LD="$PERL_CC"
export LDFLAGS="$PERL_LDFLAGS"
unset LIBS
 
PERL_PREFIX="${PERL_PREFIX:=$STATICPERL/perl}" # where the perl gets installed
 
unset PERL5OPT PERL5LIB PERLLIB PERL_UNICODE PERLIO_DEBUG
unset PERL_MB_OPT
LC_ALL=C; export LC_ALL # just to be on the safe side
 
# prepend PATH - not required by staticperl itself, but might make
# life easier when working in e.g. "staticperl cpan / look"
PATH="$PERL_PREFIX/perl/bin:$PATH"
 
# set version in a way that Makefile.PL can extract
VERSION=VERSION; eval \
$VERSION="1.46"
 
fatal() {
   printf -- "\nFATAL: %s\n\n" "$*" >&2
   exit 1
}
 
verbose() {
   printf -- "%s\n" "$*"
}
 
verblock() {
   verbose
   verbose "***"
   while read line; do
      verbose "*** $line"
   done
   verbose "***"
   verbose
}
 
rcd() {
   cd "$1" || fatal "$1: cannot enter"
}
 
trace() {
   prefix="$1"; shift
#   "$@" 2>&1 | while read line; do
#      echo "$prefix: $line"
#   done
   "$@"
}
 
trap wait 0
 
#############################################################################
# clean
 
distclean() {
   verblock <<EOF
   deleting everything installed by this script (rm -rf $STATICPERL)
EOF
 
   rm -rf "$STATICPERL"
}
 
#############################################################################
# download/configure/compile/install perl
 
clean() {
   rm -rf "$STATICPERL/src"
}
 
realclean() {
   rm -f "$PERL_PREFIX/staticstamp.postinstall"
   rm -f "$PERL_PREFIX/staticstamp.install"
   rm -f "$STATICPERL/src/perl/staticstamp.configure"
}
 
fetch() {
(
   rcd "$STATICPERL"
 
   mkdir -p src
   rcd src
 
   if ! [ -d "perl" ]; then
      rm -rf unpack
      mkdir -p unpack
 
      case "$PERL_VERSION" in
         *:* )
            # url
            PERLURL="$PERL_VERSION"
            PERLTAR="$(basename "$PERL_VERSION")"
            ;;
         /* )
            # directory
            verbose "copying $PERL_VERSION"
            cp -Rp "$PERL_VERSION/." unpack/.
            chmod -R u+w unpack
            mv unpack perl
            return
            ;;
         * )
            PERLURL="$CPAN/src/5.0/perl-$PERL_VERSION.tar.bz2"
            PERLTAR=perl-$PERL_VERSION.tar.bz2
            ;;
      esac
 
      if ! [ -e "$PERLTAR" ]; then
         verblock <<EOF
downloading perl
 
trying to download from $PERLURL
 
you can configure a download cache directory via DLCACHE
in your .staticperlrc to avoid repeated downloads.
 
to manually download perl yourself, place a suitable tarball in
$DLCACHE/$PERLTAR
 
either curl or wget is required for automatic download.
curl is tried first, then wget.
EOF
 
         rm -f $PERLTAR~ # just to be on the safe side
         { [ "$DLCACHE" ] && cp "$DLCACHE"/$PERLTAR $PERLTAR~ >/dev/null 2>&1; } \
            || wget -O $PERLTAR~ "$PERLURL" \
            || curl -f >$PERLTAR~ "$PERLURL" \
            || fatal "$URL: unable to download"
         rm -f $PERLTAR
         mv $PERLTAR~ $PERLTAR
         if [ "$DLCACHE" ]; then
            mkdir -p "$DLCACHE"
            cp $PERLTAR "$DLCACHE"/$PERLTAR~$$~ && \
               mv "$DLCACHE"/$PERLTAR~$$~ "$DLCACHE"/$PERLTAR
         fi
      fi
 
      verblock <<EOF
unpacking perl
EOF
 
      case "$PERLTAR" in
         *.xz   ) UNCOMPRESS="xz -d"    ;;
         *.lzma ) UNCOMPRESS="lzma -d"  ;;
         *.bz2  ) UNCOMPRESS="bzip2 -d" ;;
         *.gz   ) UNCOMPRESS="gzip -d"  ;;
         *.tar  ) UNCOMPRESS="cat"      ;;
         * )
            fatal "don't know hot to uncompress $PERLTAR,\nonly tar, tar.gz, tar.bz2, tar.lzma and tar.xz are supported."
            exit 1
            ;;
      esac
 
      <"$PERLTAR" $UNCOMPRESS -d | ( cd unpack && tar xf - ) \
         || fatal "$PERLTAR: error during unpacking"
 
      if [ -d unpack/*/ ]; then
         chmod -R u+w unpack/*/
         mv unpack/*/ perl
         rmdir -p unpack
      else
         fatal "unpacking $PERLTAR did not result in a single directory, don't know how to handle this"
      fi
 
      rm "$PERLTAR"
   fi
) || exit
}
 
# similar to GNU-sed -i or perl -pi
sedreplace() {
   sed -e "$1" <"$2" > "$2~" || fatal "error while running sed"
   rm -f "$2"
   mv "$2~" "$2"
}
 
configure_failure() {
   cat <<EOF
 
 
*** 
*** Configure failed - see above for the exact error message(s).
*** 
*** Most commonly, this is because the default PERL_CCFLAGS or PERL_OPTIMIZE
*** flags are not supported by your compiler. Less often, this is because
*** PERL_LIBS either contains a library not available on your system (such as
*** -lcrypt), or because it lacks a required library (e.g. -lsocket or -lnsl).
*** 
*** You can provide your own flags by creating a ~/.staticperlrc file with
*** variable assignments. For example (these are the actual values used):
***
 
PERL_CC="$PERL_CC"
PERL_CCFLAGS="$PERL_CCFLAGS"
PERL_OPTIMIZE="$PERL_OPTIMIZE"
PERL_LDFLAGS="$PERL_LDFLAGS"
PERL_LIBS="$PERL_LIBS"
 
EOF
   exit 1
}
 
configure() {
(
   fetch
 
   rcd "$STATICPERL/src/perl"
 
   [ -e staticstamp.configure ] && return
 
   verblock <<EOF
configuring $STATICPERL/src/perl
EOF
 
   rm -f "$PERL_PREFIX/staticstamp.install"
 
   "$MAKE" distclean >/dev/null 2>&1
 
   sedreplace '/^#define SITELIB/d' config_h.SH
 
   # I hate them for this
   grep -q -- -fstack-protector Configure && \
      sedreplace 's/-fstack-protector/-fno-stack-protector/g' Configure
 
   # what did that bloke think
   grep -q -- usedl=.define hints/darwin.sh && \
      sedreplace '/^usedl=.define.;$/d' hints/darwin.sh
 
   preconfigure || fatal "preconfigure hook failed"
 
#   trace configure \
   sh Configure -Duselargefiles \
                -Uuse64bitint \
                -Dusemymalloc=n \
                -Uusedl \
                -Uusethreads \
                -Uuseithreads \
                -Uusemultiplicity \
                -Uusesfio \
                -Uuseshrplib \
                -Uinstallusrbinperl \
                -A ccflags=" $PERL_CCFLAGS" \
                -Dcc="$PERL_CC" \
                -Doptimize="$PERL_OPTIMIZE" \
                -Dldflags="$PERL_LDFLAGS" \
                -Dlibs="$PERL_LIBS" \
                -Dprefix="$PERL_PREFIX" \
                -Dbin="$PERL_PREFIX/bin" \
                -Dprivlib="$PERL_PREFIX/lib" \
                -Darchlib="$PERL_PREFIX/lib" \
                -Uusevendorprefix \
                -Dsitelib="$PERL_PREFIX/lib" \
                -Dsitearch="$PERL_PREFIX/lib" \
                -Uman1dir \
                -Uman3dir \
                -Usiteman1dir \
                -Usiteman3dir \
                -Dpager=/usr/bin/less \
                -Demail="$EMAIL" \
                -Dcf_email="$EMAIL" \
                -Dcf_by="$EMAIL" \
                $PERL_CONFIGURE \
                -Duseperlio \
                -Uversiononly \
                -dE || configure_failure
 
   sedreplace '
      s/-Wl,--no-gc-sections/-Wl,--gc-sections/g
      s/ *-fno-stack-protector */ /g
   ' config.sh
 
   patchconfig || fatal "patchconfig hook failed"
 
   sh Configure -S || fatal "Configure -S failed"
 
   postconfigure || fatal "postconfigure hook failed"
 
   : > staticstamp.configure
) || exit
}
 
write_shellscript() {
   {
      echo "#!/bin/sh"
      echo "STATICPERL=\"$STATICPERL\""
      echo "PERL_PREFIX=\"$PERL_PREFIX\""
      echo "MAKE=\"$MAKE\""
      cat
   } >"$PERL_PREFIX/bin/$1"
   chmod 755 "$PERL_PREFIX/bin/$1"
}
 
build() {
(
   configure
 
   rcd "$STATICPERL/src/perl"
 
   verblock <<EOF
building $STATICPERL/src/perl
EOF
 
   rm -f "$PERL_PREFIX/staticstamp.install"
 
   "$MAKE" || fatal "make: error while building perl"
 
   postbuild || fatal "postbuild hook failed"
) || exit
}
 
_postinstall() {
   if ! [ -e "$PERL_PREFIX/staticstamp.postinstall" ]; then
      NOCHECK_INSTALL=+
      instcpan $STATICPERL_MODULES
      [ "$EXTRA_MODULES" ] && instcpan $EXTRA_MODULES
 
      postinstall || fatal "postinstall hook failed"
 
      : > "$PERL_PREFIX/staticstamp.postinstall"
   fi
}
 
install() {
(
   if ! [ -e "$PERL_PREFIX/staticstamp.install" ]; then
      build
 
      verblock <<EOF
installing $STATICPERL/src/perl
to $PERL_PREFIX
EOF
 
      ln -sf "perl/bin/" "$STATICPERL/bin"
      ln -sf "perl/lib/" "$STATICPERL/lib"
 
      mkdir "$STATICPERL/patched"
 
      ln -sf "$PERL_PREFIX" "$STATICPERL/perl" # might get overwritten
      rm -rf "$PERL_PREFIX"                    # by this rm -rf
 
      rcd "$STATICPERL/src/perl"
 
      "$MAKE" install || fatal "make install: error while installing"
 
      rcd "$PERL_PREFIX"
 
      # create a "make install" replacement for CPAN
      write_shellscript SP-make-make <<'end_of_make'
#! sh
 
# newer Storable versions have some weird hack to try to measure the
# stack size at build time, for reasons not well understood. it seems
# perl5-porters think that stack sizes cannot be configured so compile time
# stack size always equals runtime stack size. very weird, potential security
# bug and doesn't even work, so work around it.
if [ -e Storable.pm.PL ] && [ -e stacksize ]; then
   echo patching stacksize bug in Storable
   cat >stacksize <<'EOSS'
#! perl
mkdir "lib", 0777;
mkdir "lib/Storable", 0777;
open my $fh, ">lib/Storable/Limit.pm" or die;
syswrite $fh, <<EOPM;
# patched by staticperl
\$Storable::recursion_limit = 512
  unless defined \$Storable::recursion_limit;
\$Storable::recursion_limit_hash = 512
  unless defined \$Storable::recursion_limit_hash;
1;
EOPM
EOSS
fi
 
"$MAKE" "$@"
 
end_of_make
 
      # create a "make install" replacement for CPAN
      write_shellscript SP-make-install-make <<'end_of_make_install_make'
#! sh
 
"$MAKE" || exit
 
if find blib/arch/auto -type f \( -name "*.a" -o -name "*.obj" -o -name "*.lib" \) | grep -q .; then
   echo Probably a static XS module, rebuilding perl
   if "$MAKE" all perl; then
      mv perl "$PERL_PREFIX"/bin/perl~ \
         && rm -f "$PERL_PREFIX"/bin/perl \
         && mv "$PERL_PREFIX"/bin/perl~ "$PERL_PREFIX"/bin/perl
      "$MAKE" -f Makefile.aperl map_clean
   else
      "$MAKE" -f Makefile.aperl map_clean
      exit 1
   fi
fi
 
"$MAKE" install UNINST=1 || exit
 
"$PERL_PREFIX"/bin/SP-patch-postinstall
 
end_of_make_install_make
 
      # create a "patch modules" helper
      write_shellscript SP-patch-postinstall <<'end_of_patch_postinstall'
#! sh
 
# helper to apply patches after installation
 
patch() {
   path="$PERL_PREFIX/lib/$1"
   cache="$STATICPERL/patched/$2"
   sed="$3"
 
   if "$PERL_PREFIX/bin/perl" -e 'exit 0+((stat shift)[7] == (stat shift)[7])' "$path" "$cache" ||
      "$PERL_PREFIX/bin/perl" -e 'exit 0+((stat shift)[9] <= (stat shift)[9])' "$path" "$cache"
   then
      if  [ -e "$path" ]; then
         echo "patching $path for a better tomorrrow"
 
         umask 022
         if ! sed -e "$sed" <"$path" > "$cache~"; then
            echo
            echo "*** FATAL: error while patching $path"
            echo
         else
            rm -f "$path"
            mv "$cache~" "$path"
            cp "$path" "$cache"
         fi
      fi
   fi
}
 
# patch CPAN::HandleConfig.pm to always include _our_ MyConfig.pm,
# not the one in the users homedirectory, to avoid clobbering his.
patch CPAN/HandleConfig.pm cpan_handleconfig_pm '
1i\
use CPAN::MyConfig; # patched by staticperl
'
 
# patch ExtUtils::MM_Unix to always search blib for modules
# when building a perl - this works around Pango/Gtk2 being misdetected
# as not being an XS module.
patch ExtUtils/MM_Unix.pm mm_unix_pm '
/^sub staticmake/,/^}/ s/if (@{$self->{C}}) {/if (@{$self->{C}} or $self->{NAME} =~ m%^(Pango|Gtk2)$%) { # patched by staticperl/
'
 
# patch ExtUtils::Miniperl to always add DynaLoader
# this is required for dynamic loading in static perls,
# and static loading in dynamic perls, when rebuilding a new perl.
# Why this patch is necessray I don't understand. Yup.
patch ExtUtils/Miniperl.pm extutils_miniperl.pm '
/^sub writemain/ a\
    push @_, "DynaLoader"; # patched by staticperl
'
 
# ExtUtils::CBuilder always tries to link shared libraries
# even on systems without shared library support. From the same
# source as Module::Build, so no wonder it's broken beyond fixing.
# and since so many dependent modules are even worse,
# we hardwire to 0 to get their pure-perl versions.
patch ExtUtils/CBuilder/Base.pm extutils_cbuilder_base.pm '
/^sub have_compiler/ a\
   return 0; # patched by staticperl
'
 
end_of_patch_postinstall
 
      # immediately use it
      "$PERL_PREFIX/bin/SP-patch-postinstall"
 
      # help to trick CPAN into avoiding ~/.cpan completely
      echo 1 >"$PERL_PREFIX/lib/CPAN/MyConfig.pm"
 
      # we call cpan with -MCPAN::MyConfig in this script, which
      # is strictly unnecssary as we have to patch CPAN anyway,
      # so consider it "for good measure".
      "$PERL_PREFIX"/bin/perl -MCPAN::MyConfig -MCPAN -e '
         CPAN::Shell->o (conf => urllist => push => "'"$CPAN"'");
         CPAN::Shell->o (conf => q<cpan_home>, "'"$STATICPERL"'/cpan");
         CPAN::Shell->o (conf => q<init>);
         CPAN::Shell->o (conf => q<cpan_home>, "'"$STATICPERL"'/cpan");
         CPAN::Shell->o (conf => q<build_dir>, "'"$STATICPERL"'/cpan/build");
         CPAN::Shell->o (conf => q<prefs_dir>, "'"$STATICPERL"'/cpan/prefs");
         CPAN::Shell->o (conf => q<histfile> , "'"$STATICPERL"'/cpan/histfile");
         CPAN::Shell->o (conf => q<keep_source_where>, "'"$STATICPERL"'/cpan/sources");
         CPAN::Shell->o (conf => q<makepl_arg>, "MAP_TARGET=perl");
         CPAN::Shell->o (conf => q<make>, "'"$PERL_PREFIX"'/bin/SP-make-make");
         CPAN::Shell->o (conf => q<make_install_make_command>, "'"$PERL_PREFIX"'/bin/SP-make-install-make");
         CPAN::Shell->o (conf => q<prerequisites_policy>, q<follow>);
         CPAN::Shell->o (conf => q<build_requires_install_policy>, q<yes>);
         CPAN::Shell->o (conf => q<recommends_policy>, q<0>);
         CPAN::Shell->o (conf => q<suggests_policy>, q<0>);
         CPAN::Shell->o (conf => q<prefer_installer>, q<EUMM>);
         CPAN::Shell->o (conf => q<commit>);
      ' || fatal "error while initialising CPAN"
 
      : > "$PERL_PREFIX/staticstamp.install"
   fi
 
   _postinstall
) || exit
}
 
import() {
(
   IMPORT="$1"
 
   rcd "$STATICPERL"
 
   if ! [ -e "$PERL_PREFIX/staticstamp.install" ]; then
      verblock <<EOF
import perl from $IMPORT to $STATICPERL
EOF
 
      rm -rf bin cpan lib patched perl src
      mkdir -m 755 perl perl/bin
      ln -s perl/bin/ bin
      ln -s "$IMPORT" perl/bin/
 
      echo "$IMPORT" > "$PERL_PREFIX/.import"
 
      : > "$PERL_PREFIX/staticstamp.install"
   fi
 
   _postinstall
) || exit
}
 
#############################################################################
# install a module from CPAN
 
instcpan() {
   [ $NOCHECK_INSTALL ] || install
 
   verblock <<EOF
installing modules from CPAN
$*
EOF
 
   MYCONFIG=
   [ -e "$PERL_PREFIX/.import" ] || MYCONFIG=-MCPAN::MyConfig
 
   "$PERL_PREFIX"/bin/perl $MYCONFIG -MCPAN -e 'notest (install => $_) for @ARGV' -- "$@" | tee "$STATICPERL/instcpan.log"
 
   if grep -q " -- NOT OK\$" "$STATICPERL/instcpan.log"; then
      fatal "failure while installing modules from CPAN ($*)"
   fi
   rm -f "$STATICPERL/instcpan.log"
}
 
#############################################################################
# install a module from unpacked sources
 
instsrc() {
   [ $NOCHECK_INSTALL ] || install
 
   verblock <<EOF
installing modules from source
$*
EOF
 
   for mod in "$@"; do
      echo
      echo $mod
      (
         rcd $mod
         "$MAKE" -f Makefile.aperl map_clean >/dev/null 2>&1
         "$MAKE" distclean >/dev/null 2>&1
         "$PERL_PREFIX"/bin/perl Makefile.PL || fatal "$mod: error running Makefile.PL"
         "$MAKE" || fatal "$mod: error building module"
         "$PERL_PREFIX"/bin/SP-make-install-make install || fatal "$mod: error installing module"
         "$MAKE" distclean >/dev/null 2>&1
         exit 0
      ) || exit $?
   done
}
 
#############################################################################
# main
 
podusage() {
   echo
 
   if [ -e "$PERL_PREFIX/bin/perl" ]; then
      "$PERL_PREFIX/bin/perl" -MPod::Usage -e \
         'pod2usage -input => *STDIN, -output => *STDOUT, -verbose => '$1', -exitval => 0, -noperldoc => 1' <"$0" \
         2>/dev/null && exit
   fi
 
   # try whatever perl we can find
   perl -MPod::Usage -e \
      'pod2usage -input => *STDIN, -output => *STDOUT, -verbose => '$1', -exitval => 0, -noperldoc => 1' <"$0" \
      2>/dev/null && exit
 
   fatal "displaying documentation requires a working perl - try '$0 install' to build one in a safe location"
}
 
usage() {
   podusage 0
}
 
catmkbundle() {
   {
      read dummy
      echo "#!$PERL_PREFIX/bin/perl"
      cat
   } <<'end_of_mkbundle'
#!/opt/bin/perl
 
#############################################################################
# cannot load modules till after the tracer BEGIN block
 
our $VERBOSE       = 1;
our $STRIP         = "pod"; # none, pod or ppi
our $UNISTRIP      = 1; # always on, try to strip unicore swash data
our $PERL          = 0;
our $APP;
our $VERIFY        = 0;
our $STATIC        = 0;
our $PACKLIST      = 0;
our $IGNORE_ENV    = 0;
our $ALLOW_DYNAMIC = 0;
our $HAVE_DYNAMIC; # maybe useful?
our $EXTRA_CFLAGS  = "";
our $EXTRA_LDFLAGS = "";
our $EXTRA_LIBS    = "";
 
our $OPTIMISE_SIZE = 0; # optimise for raw file size instead of for compression?
 
our $CACHE;
our $CACHEVER = 1; # do not change unless you know what you are doing
 
my $PREFIX  = "bundle";
my $PACKAGE = "static";
 
my %pm;
my %pmbin;
my @libs;
my @static_ext;
my $extralibs;
my @staticlibs;
my @incext;
 
@ARGV
   or die "$0: use 'staticperl help' (or read the sources of staticperl)\n";
 
# remove "." from @INC - staticperl.sh does it for us, but be on the safe side
BEGIN { @INC = grep !/^\.$/, @INC }
 
$|=1;
 
our ($TRACER_W, $TRACER_R);
 
sub find_incdir($) {
   for (@INC) {
      next if ref;
      return $_ if -e "$_/$_[0]";
   }
 
   undef
}
 
sub find_inc($) {
   my $dir = find_incdir $_[0];
 
   return "$dir/$_[0]"
      if defined $dir;
 
   undef
}
 
BEGIN {
   # create a loader process to detect @INC requests before we load any modules
   my ($W_TRACER, $R_TRACER); # used by tracer
 
   pipe $R_TRACER, $TRACER_W or die "pipe: $!";
   pipe $TRACER_R, $W_TRACER or die "pipe: $!";
 
   unless (fork) {
      close $TRACER_R;
      close $TRACER_W;
 
      my $pkg = "pkg000000";
 
      unshift @INC, sub {
         my $dir = find_incdir $_[1]
            or return;
 
         syswrite $W_TRACER, "-\n$dir\n$_[1]\n";
 
         open my $fh, "<:raw:perlio", "$dir/$_[1]"
            or warn "ERROR: $dir/$_[1]: $!\n";
 
         $fh
      };
 
      while (<$R_TRACER>) {
         if (/use (.*)$/) {
            my $mod = $1;
            my $eval;
 
            if ($mod =~ /^'.*'$/ or $mod =~ /^".*"$/) {
               $eval = "require $mod";
            } elsif ($mod =~ y%/.%%) {
               $eval = "require q\x00$mod\x00";
            } else {
               my $pkg = ++$pkg;
               $eval = "{ package $pkg; use $mod; }";
            }
 
            eval $eval;
            warn "ERROR: $@ (while loading '$mod')\n"
               if $@;
         } elsif (/eval (.*)$/) {
            my $eval = $1;
            eval $eval;
            warn "ERROR: $@ (in '$eval')\n"
               if $@;
         }
 
         syswrite $W_TRACER, "\n";
      }
 
      exit 0;
   }
}
 
# module loading is now safe
 
sub trace_parse {
   for (;;) {
      <$TRACER_R> =~ /^-$/ or last;
      my $dir  = <$TRACER_R>; chomp $dir;
      my $name = <$TRACER_R>; chomp $name;
 
      $pm{$name} = "$dir/$name";
 
      print "+ found potential dependency $name\n"
         if $VERBOSE >= 3;
   }
}
 
sub trace_module {
   print "tracing module $_[0]\n"
      if $VERBOSE >= 2;
 
   syswrite $TRACER_W, "use $_[0]\n";
   trace_parse;
}
 
sub trace_eval {
   print "tracing eval $_[0]\n"
      if $VERBOSE >= 2;
 
   syswrite $TRACER_W, "eval $_[0]\n";
   trace_parse;
}
 
sub trace_finish {
   close $TRACER_W;
   close $TRACER_R;
}
 
#############################################################################
# now we can use modules
 
use common::sense;
use Config;
use Digest::MD5;
 
sub cache($$$) {
   my ($variant, $src, $filter) = @_;
 
   if (length $CACHE and 2048 <= length $src and defined $variant) {
      my $file = "$CACHE/" . Digest::MD5::md5_hex "$CACHEVER\x00$variant\x00$src";
 
      if (open my $fh, "<:raw:perlio", $file) {
         print "using cache for $file\n"
            if $VERBOSE >= 7;
 
         local $/;
         return <$fh>;
      }
 
      $src = $filter->($src);
 
      print "creating cache entry $file\n"
         if $VERBOSE >= 8;
 
      if (open my $fh, ">:raw:perlio", "$file~") {
         if ((syswrite $fh, $src) == length $src) {
            close $fh;
            rename "$file~", $file;
         }
      }
 
      return $src;
   }
 
   $filter->($src)
}
 
sub dump_string {
   my ($fh, $data) = @_;
 
   if (length $data) {
      if ($^O eq "MSWin32") {
         # 16 bit system, strings can't be longer than 64k. seriously.
         print $fh "{\n";
         for (
            my $ofs = 0;
            length (my $substr = substr $data, $ofs, 20);
            $ofs += 20
         )  {
            $substr = join ",", map ord, split //, $substr;
            print $fh "  $substr,\n";
         }
         print $fh "   0 }\n";
      } else {
         for (
            my $ofs = 0;
            length (my $substr = substr $data, $ofs, 80);
            $ofs += 80
         )  {
            $substr =~ s/([^\x20-\x21\x23-\x5b\x5d-\x7e])/sprintf "\\%03o", ord $1/ge;
            $substr =~ s/\?/\\?/g; # trigraphs...
            print $fh "  \"$substr\"\n";
         }
      }
   } else {
      print $fh "  \"\"\n";
   }
}
 
#############################################################################
 
sub glob2re {
   for (quotemeta $_[0]) {
      s/\\\*/\x00/g;
      s/\x00\x00/.*/g;
      s/\x00/[^\/]*/g;
      s/\\\?/[^\/]/g;
 
      $_ = s/^\\\/// ? "^$_\$" : "(?:^|/)$_\$";
 
      s/(?: \[\^\/\] | \. ) \*\$$//x;
 
      return qr<$_>s
   }
}
 
our %INCSKIP = (
   "unicore/TestProp.pl" => undef, # 3.5MB of insanity, apparently just some testcase
);
 
sub get_dirtree {
   my $root = shift;
 
   my @tree;
   my $skip;
 
   my $scan; $scan = sub {
      for (sort do {
         opendir my $fh, $_[0]
            or return;
         readdir $fh
      }) {
         next if /^\./;
 
         my $path = "$_[0]/$_";
 
         if (-d "$path/.") {
            $scan->($path);
         } else {
            $path = substr $path, $skip;
            push @tree, $path
               unless exists $INCSKIP{$path};
         }
      }
   };
 
   $root =~ s/\/$//;
   $skip = 1 + length $root;
   $scan->($root);
 
   \@tree
}
 
my $inctrees;
 
sub get_inctrees {
   unless ($inctrees) {
      my %inctree;
      $inctree{$_} ||= [$_, get_dirtree $_] # entries in @INC are often duplicates
         for @INC;
      $inctrees = [values %inctree];
   }
 
   @$inctrees
}
 
#############################################################################
 
sub cmd_boot {
   $pm{"!boot"} = $_[0];
}
 
sub cmd_add {
   $_[0] =~ /^(.*?)(?:\s+(\S+))?$/
      or die "$_[0]: cannot parse";
 
   my $file = $1;
   my $as   = defined $2 ? $2 : $1;
 
   $pm{$as} = $file;
   $pmbin{$as} = 1 if $_[1];
}
 
sub cmd_staticlib {
   push @staticlibs, $_
      for split /\s+/, $_[0];
}
 
sub cmd_include {
   push @incext, [$_[1], glob2re $_[0]];
}
 
sub cmd_incglob {
   my ($pattern) = @_;
 
   $pattern = glob2re $pattern;
 
   for (get_inctrees) {
      my ($dir, $files) = @$_;
 
      $pm{$_} = "$dir/$_"
         for grep /$pattern/ && /\.(pl|pm)$/, @$files;
   }
}
 
sub parse_argv;
 
sub cmd_file {
   open my $fh, "<", $_[0]
      or die "$_[0]: $!\n";
 
   local @ARGV;
 
   while (<$fh>) {
      chomp;
      next unless /\S/;
      next if /^\s*#/;
 
      s/^\s*-*/--/;
      my ($cmd, $args) = split / /, $_, 2;
 
      push @ARGV, $cmd;
      push @ARGV, $args if defined $args;
   }
 
   parse_argv;
}
 
use Getopt::Long;
 
sub parse_argv {
   GetOptions
      "perl"            => \$PERL,
      "app=s"           => \$APP,
 
      "verbose|v"       => sub { ++$VERBOSE },
      "quiet|q"         => sub { --$VERBOSE },
 
      "strip=s"         => \$STRIP,
      "cache=s"         => \$CACHE, # internal option
      "eval|e=s"        => sub { trace_eval    $_[1] },
      "use|M=s"         => sub { trace_module  $_[1] },
      "boot=s"          => sub { cmd_boot      $_[1] },
      "add=s"           => sub { cmd_add       $_[1], 0 },
      "addbin=s"        => sub { cmd_add       $_[1], 1 },
      "incglob=s"       => sub { cmd_incglob   $_[1] },
      "include|i=s"     => sub { cmd_include   $_[1], 1 },
      "exclude|x=s"     => sub { cmd_include   $_[1], 0 },
      "usepacklists!"   => \$PACKLIST,
 
      "static!"         => \$STATIC,
      "staticlib=s"     => sub { cmd_staticlib $_[1] },
      "allow-dynamic!"  => \$ALLOW_DYNAMIC,
      "ignore-env"      => \$IGNORE_ENV,
 
      "extra-cflags=s"  => \$EXTRA_CFLAGS,
      "extra-ldflags=s" => \$EXTRA_LDFLAGS,
      "extra-libs=s"    => \$EXTRA_LIBS,
 
      "<>"              => sub { cmd_file      $_[0] },
      or exit 1;
}
 
Getopt::Long::Configure ("bundling", "no_auto_abbrev", "no_ignore_case");
 
parse_argv;
 
die "cannot specify both --app and --perl\n"
   if $PERL and defined $APP;
 
# required for @INC loading, unfortunately
trace_module "PerlIO::scalar";
 
#############################################################################
# apply include/exclude
 
{
   my %pmi;
 
   for (@incext) {
      my ($inc, $glob) = @$_;
 
      my @match = grep /$glob/, keys %pm;
 
      if ($inc) {
         # include
         @pmi{@match} = delete @pm{@match};
 
         print "applying include $glob - protected ", (scalar @match), " files.\n"
            if $VERBOSE >= 5;
      } else {
         # exclude
         delete @pm{@match};
 
         print "applying exclude $glob - removed ", (scalar @match), " files.\n"
            if $VERBOSE >= 5;
      }
   }
 
   my @pmi = keys %pmi;
   @pm{@pmi} = delete @pmi{@pmi};
}
 
#############################################################################
# scan for AutoLoader, static archives and other dependencies
 
sub scan_al {
   my ($auto, $autodir) = @_;
 
   my $ix = "$autodir/autosplit.ix";
 
   print "processing autoload index for '$auto'\n"
      if $VERBOSE >= 6;
 
   $pm{"$auto/autosplit.ix"} = $ix;
 
   open my $fh, "<:perlio", $ix
      or die "$ix: $!";
 
   my $package;
 
   while (<$fh>) {
      if (/^\s*sub\s+ ([^[:space:];]+) \s* (?:\([^)]*\))? \s*;?\s*$/x) {
         my $al = "auto/$package/$1.al";
         my $inc = find_inc $al;
 
         defined $inc or die "$al: autoload file not found, but should be there.\n";
 
         $pm{$al} = $inc;
         print "found autoload function '$al'\n"
            if $VERBOSE >= 6;
 
      } elsif (/^\s*package\s+([^[:space:];]+)\s*;?\s*$/) {
         ($package = $1) =~ s/::/\//g;
      } elsif (/^\s*(?:#|1?\s*;?\s*$)/) {
         # nop
      } else {
         warn "WARNING: $ix: unparsable line, please report: $_";
      }
   }
}
 
for my $pm (keys %pm) {
   if ($pm =~ /^(.*)\.pm$/) {
      my $auto    = "auto/$1";
      my $autodir = find_inc $auto;
 
      if (defined $autodir && -d $autodir) {
         # AutoLoader
         scan_al $auto, $autodir
            if -f "$autodir/autosplit.ix";
 
         # extralibs.ld
         if (open my $fh, "<:perlio", "$autodir/extralibs.ld") {
            print "found extralibs for $pm\n"
               if $VERBOSE >= 6;
 
            local $/;
            $extralibs .= " " . <$fh>;
         }
 
         $pm =~ /([^\/]+).pm$/ or die "$pm: unable to match last component";
 
         my $base = $1;
 
         # static ext
         if (-f "$autodir/$base$Config{_a}") {
            print "found static archive for $pm\n"
               if $VERBOSE >= 3;
 
            push @libs, "$autodir/$base$Config{_a}";
            push @static_ext, $pm;
         }
 
         # dynamic object
         if (-f "$autodir/$base.$Config{dlext}") {
            if ($ALLOW_DYNAMIC) {
               my $as = "!$auto/$base.$Config{dlext}";
               $pm{$as} = "$autodir/$base.$Config{dlext}";
               $pmbin{$as} = 1;
 
               $HAVE_DYNAMIC = 1;
 
               print "+ added dynamic object $as\n"
                  if $VERBOSE >= 3;
            } else {
               die "ERROR: found shared object '$autodir/$base.$Config{dlext}' but --allow-dynamic not given, aborting.\n"
            }
         }
 
         if ($PACKLIST && open my $fh, "<:perlio", "$autodir/.packlist") {
            print "found .packlist for $pm\n"
               if $VERBOSE >= 3;
 
            while (<$fh>) {
               chomp;
               s/ .*$//; # newer-style .packlists might contain key=value pairs
 
               # only include certain files (.al, .ix, .pm, .pl)
               if (/\.(pm|pl|al|ix)$/) {
                  for my $inc (@INC) {
                     # in addition, we only add files that are below some @INC path
                     $inc =~ s/\/*$/\//;
 
                     if ($inc eq substr $_, 0, length $inc) {
                        my $base = substr $_, length $inc;
                        $pm{$base} = $_;
 
                        print "+ added .packlist dependency $base\n"
                           if $VERBOSE >= 3;
                     }
 
                     last;
                  }
               }
            }
         }
      }
   }
}
 
#############################################################################
 
print "processing bundle files (try more -v power if you get bored waiting here)...\n"
   if $VERBOSE >= 1;
 
my $data;
my @index;
my @order = sort {
   length $a <=> length $b
      or $a cmp $b
} keys %pm;
 
# sorting by name - better compression, but needs more metadata
# sorting by length - faster lookup
# usually, the metadata overhead beats the loss through compression
 
for my $pm (@order) {
   my $path = $pm{$pm};
 
   128 > length $pm
      or die "ERROR: $pm: path too long (only 128 octets supported)\n";
 
   my $src = ref $path
           ? $$path
           : do {
              open my $pm, "<:raw:perlio", $path
                 or die "$path: $!";
 
              local $/;
               
              <$pm>
           };
 
   my $size = length $src;
 
   unless ($pmbin{$pm}) { # only do this unless the file is binary
      if ($pm =~ /^auto\/POSIX\/[^\/]+\.al$/) {
         if ($src =~ /^    unimpl \"/m) {
            print "$pm: skipping (raises runtime error only).\n"
               if $VERBOSE >= 3;
            next;
         }
      }
 
      $src = cache +($STRIP eq "ppi" ? "$UNISTRIP,$OPTIMISE_SIZE" : undef), $src, sub {
         if ($UNISTRIP && $pm =~ /^unicore\/.*\.pl$/) {
            print "applying unicore stripping $pm\n"
               if $VERBOSE >= 6;
 
            # special stripping for unicore swashes and properties
            # much more could be done by going binary
            $src =~ s{
               (^return\ <<'END';\n) (.*?\n) (END(?:\n|\Z))
            }{
               my ($pre, $data, $post) = ($1, $2, $3);
 
               for ($data) {
                  s/^([0-9a-fA-F]+)\t([0-9a-fA-F]+)\t/sprintf "%X\t%X", hex $1, hex $2/gem
                     if $OPTIMISE_SIZE;
 
#                  s{
#                     ^([0-9a-fA-F]+)\t([0-9a-fA-F]*)\t
#                  }{
#                     # ww - smaller filesize, UU - compress better
#                     pack "C0UU",
#                          hex $1,
#                          length $2 ? (hex $2) - (hex $1) : 0
#                  }gemx;
 
                  s/#.*\n/\n/mg;
                  s/\s+\n/\n/mg;
               }
 
               "$pre$data$post"
            }smex;
         }
 
         if ($STRIP =~ /ppi/i) {
            require PPI;
 
            if (my $ppi = PPI::Document->new (\$src)) {
               $ppi->prune ("PPI::Token::Comment");
               $ppi->prune ("PPI::Token::Pod");
 
               # prune END stuff
               for (my $last = $ppi->last_element; $last; ) {
                  my $prev = $last->previous_token;
 
                  if ($last->isa (PPI::Token::Whitespace::)) {
                     $last->delete;
                  } elsif ($last->isa (PPI::Statement::End::)) {
                     $last->delete;
                     last;
                  } elsif ($last->isa (PPI::Token::Pod::)) {
                     $last->delete;
                  } else {
                     last;
                  }
 
                  $last = $prev;
               }
 
               # prune some but not all insignificant whitespace
               for my $ws (@{ $ppi->find (PPI::Token::Whitespace::) }) {
                  my $prev = $ws->previous_token;
                  my $next = $ws->next_token;
 
                  if (!$prev || !$next) {
                     $ws->delete;
                  } else {
                     if (
                        $next->isa (PPI::Token::Operator::) && $next->{content} =~ /^(?:,|=|!|!=|==|=>)$/ # no ., because of digits. == float
                        or $prev->isa (PPI::Token::Operator::) && $prev->{content} =~ /^(?:,|=|\.|!|!=|==|=>)$/
                        or $prev->isa (PPI::Token::Structure::)
                        or ($OPTIMISE_SIZE &&
                            ($prev->isa (PPI::Token::Word::)
                               && (PPI::Token::Symbol:: eq ref $next
                                   || $next->isa (PPI::Structure::Block::)
                                   || $next->isa (PPI::Structure::List::)
                                   || $next->isa (PPI::Structure::Condition::)))
                           )
                     ) {
                        $ws->delete;
                     } elsif ($prev->isa (PPI::Token::Whitespace::)) {
                        $ws->{content} = ' ';
                        $prev->delete;
                     } else {
                        $ws->{content} = ' ';
                     }
                  }
               }
 
               # prune whitespace around blocks
               if ($OPTIMISE_SIZE) {
                  # these usually decrease size, but decrease compressability more
                  for my $struct (PPI::Structure::Block::, PPI::Structure::Condition::) {
                     for my $node (@{ $ppi->find ($struct) }) {
                        my $n1 = $node->first_token;
                        my $n2 = $n1->previous_token;
                        $n1->delete if $n1->isa (PPI::Token::Whitespace::);
                        $n2->delete if $n2 && $n2->isa (PPI::Token::Whitespace::);
                        my $n1 = $node->last_token;
                        my $n2 = $n1->next_token;
                        $n1->delete if $n1->isa (PPI::Token::Whitespace::);
                        $n2->delete if $n2 && $n2->isa (PPI::Token::Whitespace::);
                     }
                  }
 
                  for my $node (@{ $ppi->find (PPI::Structure::List::) }) {
                     my $n1 = $node->first_token;
                     $n1->delete if $n1->isa (PPI::Token::Whitespace::);
                     my $n1 = $node->last_token;
                     $n1->delete if $n1->isa (PPI::Token::Whitespace::);
                  }
               }
 
               # reformat qw() lists which often have lots of whitespace
               for my $node (@{ $ppi->find (PPI::Token::QuoteLike::Words::) }) {
                  if ($node->{content} =~ /^qw(.)(.*)(.)$/s) {
                     my ($a, $qw, $b) = ($1, $2, $3);
                     $qw =~ s/^\s+//;
                     $qw =~ s/\s+$//;
                     $qw =~ s/\s+/ /g;
                     $node->{content} = "qw$a$qw$b";
                  }
               }
 
               $src = $ppi->serialize;
            } else {
               warn "WARNING: $pm{$pm}: PPI failed to parse this file\n";
            }
         } elsif ($STRIP =~ /pod/i && $pm ne "Opcode.pm") { # opcode parses its own pod
            require Pod::Strip;
 
            my $stripper = Pod::Strip->new;
 
            my $out;
            $stripper->output_string (\$out);
            $stripper->parse_string_document ($src)
               or die;
            $src = $out;
         }
 
         if ($VERIFY && $pm =~ /\.pm$/ && $pm ne "Opcode.pm") {
            if (open my $fh, "-|") {
               <$fh>;
            } else {
               eval "#line 1 \"$pm\"\n$src" or warn "\n\n\n$pm\n\n$src\n$@\n\n\n";
               exit 0;
            }
         }
 
         $src
      };
 
#      if ($pm eq "Opcode.pm") {
#         open my $fh, ">x" or die; print $fh $src;#d#
#         exit 1;
#      }
   }
 
   print "adding $pm (original size $size, stored size ", length $src, ")\n"
      if $VERBOSE >= 2;
 
   push @index, ((length $pm) << 25) | length $data;
   $data .= $pm . $src;
}
 
length $data < 2**25
   or die "ERROR: bundle too large (only 32MB supported)\n";
 
my $varpfx = "bundle";
 
#############################################################################
# output
 
print "generating $PREFIX.h... "
   if $VERBOSE >= 1;
 
{
   open my $fh, ">", "$PREFIX.h"
      or die "$PREFIX.h: $!\n";
 
   print $fh <<EOF;
/* do not edit, automatically created by staticperl */
 
#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>
 
/* public API */
EXTERN_C PerlInterpreter *staticperl;
EXTERN_C void staticperl_xs_init (pTHX);
EXTERN_C void staticperl_init (XSINIT_t xs_init); /* argument can be 0 */
EXTERN_C void staticperl_cleanup (void);
 
EOF
}
 
print "\n"
   if $VERBOSE >= 1;
 
#############################################################################
# output
 
print "generating $PREFIX.c... "
   if $VERBOSE >= 1;
 
open my $fh, ">", "$PREFIX.c"
   or die "$PREFIX.c: $!\n";
 
print $fh <<EOF;
/* do not edit, automatically created by staticperl */
 
#include "bundle.h"
 
/* public API */
PerlInterpreter *staticperl;
 
EOF
 
#############################################################################
# bundle data
 
my $count = @index;
 
print $fh <<EOF;
#include "bundle.h"
 
/* bundle data */
 
static const U32 $varpfx\_count = $count;
static const U32 $varpfx\_index [$count + 1] = {
EOF
 
my $col;
for (@index) {
   printf $fh "0x%08x,", $_;
   print $fh "\n" unless ++$col % 10;
 
}
printf $fh "0x%08x\n};\n", (length $data);
 
print $fh "static const char $varpfx\_data [] =\n";
dump_string $fh, $data;
 
print $fh ";\n\n";
 
#############################################################################
# bootstrap
 
# boot file for staticperl
# this file will be eval'ed at initialisation time
 
# lines marked with "^D" are only used when $HAVE_DYNAMIC
my $bootstrap = '
BEGIN {
   package ' . $PACKAGE . ';
 
   # the path prefix to use when putting files into %INC
   our $inc_prefix;
 
   # the @INC hook to use when we have PerlIO::scalar available
   my $perlio_inc =  sub {
      my $data = find "$_[1]"
         or return;
 
      $INC{$_[1]} = "$inc_prefix$_[1]";
 
      open my $fh, "<", \$data;
      $fh
   };
 
D  if (defined &PerlIO::scalar::bootstrap) {
      # PerlIO::scalar statically compiled in
      PerlIO::scalar->bootstrap;
      @INC = $perlio_inc;
D  } else {
D     # PerlIO::scalar not available, use slower method
D     @INC = sub {
D        # always check if PerlIO::scalar might now be available
D        if (defined &PerlIO::scalar::bootstrap) {
D           # switch to the faster perlio_inc hook
D           @INC = map { $_ == $_[0] ? $perlio_inc : $_ } @INC;
D           goto &$perlio_inc;
D        }
D
D        my $data = find "$_[1]"
D           or return;
D
D        $INC{$_[1]} = "$inc_prefix$_[1]";
D
D        sub {
D           $data =~ /\G([^\n]*\n?)/g
D              or return;
D
D           $_ = $1;
D           1
D        }
D     };
D  }
}
';
 
$bootstrap .= "require '!boot';"
   if exists $pm{"!boot"};
 
if ($HAVE_DYNAMIC) {
   $bootstrap =~ s/^D/ /mg;
} else {
   $bootstrap =~ s/^D.*$//mg;
}
 
$bootstrap =~ s/#.*$//mg;
$bootstrap =~ s/\s+/ /g;
$bootstrap =~ s/(\W) /$1/g;
$bootstrap =~ s/ (\W)/$1/g;
 
print $fh "const char bootstrap [] = ";
dump_string $fh, $bootstrap;
print $fh ";\n\n";
 
print $fh <<EOF;
/* search all bundles for the given file, using binary search */
XS(find)
{
  dXSARGS;
 
  if (items != 1)
    Perl_croak (aTHX_ "Usage: $PACKAGE\::find (\$path)");
 
  {
    STRLEN namelen;
    char *name = SvPV (ST (0), namelen);
    SV *res = 0;
 
    int l = 0, r = $varpfx\_count;
 
    while (l <= r)
      {
        int m = (l + r) >> 1;
        U32 idx = $varpfx\_index [m];
        int comp = namelen - (idx >> 25);
 
        if (!comp)
          {
            int ofs = idx & 0x1FFFFFFU;
            comp = memcmp (name, $varpfx\_data + ofs, namelen);
 
            if (!comp)
              {
                /* found */
                int ofs2 =  $varpfx\_index [m + 1] & 0x1FFFFFFU;
 
                ofs += namelen;
                res = newSVpvn ($varpfx\_data + ofs, ofs2 - ofs);
                goto found;
              }
          }
 
        if (comp < 0)
          r = m - 1;
        else
          l = m + 1;
      }
 
    XSRETURN (0);
 
  found:
    ST (0) = sv_2mortal (res);
  }
 
  XSRETURN (1);
}
 
/* list all files in the bundle */
XS(list)
{
  dXSARGS;
 
  if (items != 0)
    Perl_croak (aTHX_ "Usage: $PACKAGE\::list");
 
  {
    int i;
 
    EXTEND (SP, $varpfx\_count);
 
    for (i = 0; i < $varpfx\_count; ++i)
      {
        U32 idx = $varpfx\_index [i];
 
        PUSHs (sv_2mortal (newSVpvn ($varpfx\_data + (idx & 0x1FFFFFFU), idx >> 25)));
      }
  }
 
  XSRETURN ($varpfx\_count);
}
 
#ifdef STATICPERL_BUNDLE_INCLUDE
#include STATICPERL_BUNDLE_INCLUDE
#endif
 
EOF
 
#############################################################################
# xs_init
 
print $fh <<EOF;
void
staticperl_xs_init (pTHX)
{
EOF
 
@static_ext = sort @static_ext;
 
# prototypes
for (@static_ext) {
   s/\.pm$//;
   (my $cname = $_) =~ s/\//__/g;
   print $fh "  EXTERN_C void boot_$cname (pTHX_ CV* cv);\n";
}
 
print $fh <<EOF;
  char *file = __FILE__;
  dXSUB_SYS;
 
  newXSproto ("$PACKAGE\::find", find, file, "\$");
  newXSproto ("$PACKAGE\::list", list, file, "");
 
  #ifdef STATICPERL_BUNDLE_XS_INIT
  STATICPERL_BUNDLE_XS_INIT;
  #endif
EOF
 
# calls
for (@static_ext) {
   s/\.pm$//;
 
   (my $cname = $_) =~ s/\//__/g;
   (my $pname = $_) =~ s/\//::/g;
 
   my $bootstrap = $pname eq "DynaLoader" ? "boot_DynaLoader" : "bootstrap";
 
   print $fh "  newXS (\"$pname\::$bootstrap\", boot_$cname, file);\n";
}
 
print $fh <<EOF;
  Safefree (PL_origfilename);
  PL_origfilename = savepv (PL_origargv [0]);
  sv_setpv (GvSV (gv_fetchpvs ("0", GV_ADD|GV_NOTQUAL, SVt_PV)), PL_origfilename);
 
  #ifdef _WIN32
    /* windows perls usually trail behind unix perls 8-10 years in exporting symbols */
 
    if (!PL_preambleav)
      PL_preambleav = newAV ();
 
    av_unshift (PL_preambleav, 1);
    av_store (PL_preambleav, 0, newSVpv (bootstrap, sizeof (bootstrap) - 1));
  #else
    Perl_av_create_and_unshift_one (&PL_preambleav, newSVpv (bootstrap, sizeof (bootstrap) - 1));
  #endif
 
  if (PL_oldname)
    ((XSINIT_t)PL_oldname)(aTHX);
}
EOF
 
#############################################################################
# optional perl_init/perl_destroy
 
if ($IGNORE_ENV) {
   $IGNORE_ENV = <<EOF;
  unsetenv ("PERL_UNICODE");
  unsetenv ("PERL_HASH_SEED_DEBUG");
  unsetenv ("PERL_DESTRUCT_LEVEL");
  unsetenv ("PERL_SIGNALS");
  unsetenv ("PERL_DEBUG_MSTATS");
  unsetenv ("PERL5OPT");
  unsetenv ("PERLIO_DEBUG");
  unsetenv ("PERLIO");
  unsetenv ("PERL_HASH_SEED");
EOF
} else {
   $IGNORE_ENV = "";
}
 
if ($APP) {
   print $fh <<EOF;
 
int
main (int argc, char *argv [])
{
  extern char **environ;
  int i, exitstatus;
  char **args = malloc ((argc + 3) * sizeof (const char *));
 
  args [0] = argv [0];
  args [1] = "-e";
  args [2] = "0";
  args [3] = "--";
 
  for (i = 1; i < argc; ++i)
    args [i + 3] = argv [i];
 
$IGNORE_ENV
  PERL_SYS_INIT3 (&argc, &argv, &environ);
  staticperl = perl_alloc ();
  perl_construct (staticperl);
 
  PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
 
  exitstatus = perl_parse (staticperl, staticperl_xs_init, argc + 3, args, environ);
  if (!exitstatus)
    perl_run (staticperl);
 
  exitstatus = perl_destruct (staticperl);
  perl_free (staticperl);
  PERL_SYS_TERM ();
  /*free (args); no point doing it this late */
 
  return exitstatus;
}
EOF
} elsif ($PERL) {
   print $fh <<EOF;
 
int
main (int argc, char *argv [])
{
  extern char **environ;
  int exitstatus;
 
$IGNORE_ENV
  PERL_SYS_INIT3 (&argc, &argv, &environ);
  staticperl = perl_alloc ();
  perl_construct (staticperl);
 
  PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
 
  exitstatus = perl_parse (staticperl, staticperl_xs_init, argc, argv, environ);
  if (!exitstatus)
    perl_run (staticperl);
 
  exitstatus = perl_destruct (staticperl);
  perl_free (staticperl);
  PERL_SYS_TERM ();
 
  return exitstatus;
}
EOF
} else {
   print $fh <<EOF;
 
EXTERN_C void
staticperl_init (XSINIT_t xs_init)
{
  static char *args[] = {
    "staticperl",
    "-e",
    "0"
  };
 
  extern char **environ;
  int argc = sizeof (args) / sizeof (args [0]);
  char **argv = args;
 
$IGNORE_ENV
  PERL_SYS_INIT3 (&argc, &argv, &environ);
  staticperl = perl_alloc ();
  perl_construct (staticperl);
  PL_origalen = 1;
  PL_exit_flags |= PERL_EXIT_DESTRUCT_END;
  PL_oldname = (char *)xs_init;
  perl_parse (staticperl, staticperl_xs_init, argc, argv, environ);
 
  perl_run (staticperl);
}
 
EXTERN_C void
staticperl_cleanup (void)
{
  perl_destruct (staticperl);
  perl_free (staticperl);
  staticperl = 0;
  PERL_SYS_TERM ();
}
EOF
}
 
close $fh;
 
print -s "$PREFIX.c", " octets (", (length $data) , " data octets).\n\n"
   if $VERBOSE >= 1;
 
#############################################################################
# libs, cflags
 
my $ccopts;
 
{
   print "generating $PREFIX.ccopts... "
      if $VERBOSE >= 1;
 
   $ccopts = "$Config{ccflags} $Config{optimize} $Config{cppflags} -I$Config{archlibexp}/CORE $EXTRA_CFLAGS";
   $ccopts =~ s/([\(\)])/\\$1/g;
 
   open my $fh, ">$PREFIX.ccopts"
      or die "$PREFIX.ccopts: $!";
   print $fh $ccopts;
 
   print "$ccopts\n\n"
      if $VERBOSE >= 1;
}
 
my $ldopts;
 
{
   print "generating $PREFIX.ldopts... ";
 
   $ldopts = $STATIC ? "-static " : "";
 
   $ldopts .= "$Config{ccdlflags} $Config{ldflags} $EXTRA_LDFLAGS @libs $Config{archlibexp}/CORE/$Config{libperl} $Config{perllibs} $EXTRA_LIBS";
 
   my %seen;
   $ldopts .= " $_" for reverse grep !$seen{$_}++, reverse +($extralibs =~ /(\S+)/g);
 
   for (@staticlibs) {
      $ldopts =~ s/(^|\s) (-l\Q$_\E) ($|\s)/$1-Wl,-Bstatic $2 -Wl,-Bdynamic$3/gx;
   }
 
   $ldopts =~ s/([\(\)])/\\$1/g;
 
   open my $fh, ">$PREFIX.ldopts"
      or die "$PREFIX.ldopts: $!";
   print $fh $ldopts;
 
   print "$ldopts\n\n"
      if $VERBOSE >= 1;
}
 
if ($PERL or defined $APP) {
   $APP = "perl" unless defined $APP;
 
   my $build = "$Config{cc} $ccopts -o \Q$APP\E$Config{_exe} bundle.c $ldopts";
 
   print "build $APP...\n"
      if $VERBOSE >= 1;
 
   print "$build\n"
      if $VERBOSE >= 2;
 
   system $build;
 
   unlink "$PREFIX.$_"
      for qw(ccopts ldopts c h);
 
   print "\n"
      if $VERBOSE >= 1;
}
 
end_of_mkbundle
}
 
bundle() {
   MKBUNDLE="${MKBUNDLE:=$PERL_PREFIX/bin/SP-mkbundle}"
   catmkbundle >"$MKBUNDLE~" || fatal "$MKBUNDLE~: cannot create"
   chmod 755 "$MKBUNDLE~" && mv "$MKBUNDLE~" "$MKBUNDLE"
   CACHE="$STATICPERL/cache"
   mkdir -p "$CACHE"
   "$PERL_PREFIX/bin/perl" -- "$MKBUNDLE" --cache "$CACHE" "$@"
}
 
if [ $# -gt 0 ]; then
   while [ $# -gt 0 ]; do
      mkdir -p "$STATICPERL" || fatal "$STATICPERL: cannot create"
      mkdir -p "$PERL_PREFIX" || fatal "$PERL_PREFIX: cannot create"
 
      command="${1#--}"; shift
      case "$command" in
         version )
            echo "staticperl version $VERSION"
            ;;
         fetch | configure | build | install | clean | realclean | distclean )
            ( "$command" ) || exit
            ;;
         import )
            ( import "$1" ) || exit
            shift
            ;;
         instsrc )
            ( instsrc "$@" ) || exit
            exit
            ;;
         instcpan )
            ( instcpan "$@" ) || exit
            exit
            ;;
         perl )
            ( install ) || exit
            exec "$PERL_PREFIX/bin/perl" "$@"
            exit
            ;;
         cpan )
            ( install ) || exit
            PERL="$PERL_PREFIX/bin/perl"
            export PERL
            exec "$PERL_PREFIX/bin/cpan" "$@"
            exit
            ;;
         mkbundle )
            ( install ) || exit
            bundle "$@"
            exit
            ;;
         mkperl )
            ( install ) || exit
            bundle --perl "$@"
            exit
            ;;
         mkapp )
            ( install ) || exit
            bundle --app "$@"
            exit
            ;;
         help )
            podusage 2
            ;;
         * )
            exec 1>&2
            echo
            echo "Unknown command: $command"
            podusage 0
            ;;
      esac
   done
else
   usage
fi
 
exit 0

 
=head1 NAME
 
staticperl - perl, libc, 100 modules, all in one standalone 500kb file
 
=head1 SYNOPSIS
 
   staticperl help      # print the embedded documentation
   staticperl fetch     # fetch and unpack perl sources
   staticperl configure # fetch and then configure perl
   staticperl build     # configure and then build perl
   staticperl install   # build and then install perl
   staticperl clean     # clean most intermediate files (restart at configure)
   staticperl distclean # delete everything installed by this script
   staticperl perl ...  # invoke the perlinterpreter
   staticperl cpan      # invoke CPAN shell
   staticperl instsrc path...        # install unpacked modules
   staticperl instcpan modulename... # install modules from CPAN
   staticperl mkbundle <bundle-args...> # see documentation
   staticperl mkperl <bundle-args...>   # see documentation
   staticperl mkapp appname <bundle-args...> # see documentation
 
Typical Examples:
 
   staticperl install   # fetch, configure, build and install perl
   staticperl cpan      # run interactive cpan shell
   staticperl mkperl -MConfig_heavy.pl # build a perl that supports -V
   staticperl mkperl -MAnyEvent::Impl::Perl -MAnyEvent::HTTPD -MURI -MURI::http
                        # build a perl with the above modules linked in
   staticperl mkapp myapp --boot mainprog mymodules
                        # build a binary "myapp" from mainprog and mymodules
 
=head1 DESCRIPTION
 
This script helps you to create single-file perl interpreters
or applications, or embedding a perl interpreter in your
applications. Single-file means that it is fully self-contained - no
separate shared objects, no autoload fragments, no .pm or .pl files are
needed. And when linking statically, you can create (or embed) a single
file that contains perl interpreter, libc, all the modules you need, all
the libraries you need and of course your actual program.
 
With F<uClibc> and F<upx> on x86, you can create a single 500kb binary
that contains perl and 100 modules such as POSIX, AnyEvent, EV, IO::AIO,
Coro and so on. Or any other choice of modules (and some other size :).
 
To see how this turns out, you can try out smallperl and bigperl, two
pre-built static and compressed perl binaries with many and even more
modules: just follow the links at L<http://staticperl.schmorp.de/>.
 
The created files do not need write access to the file system (like PAR
does). In fact, since this script is in many ways similar to PAR::Packer,
here are the differences:
 
=over 4
 
=item * The generated executables are much smaller than PAR created ones.
 
Shared objects and the perl binary contain a lot of extra info, while
the static nature of F<staticperl> allows the linker to remove all
functionality and meta-info not required by the final executable. Even
extensions statically compiled into perl at build time will only be
present in the final executable when needed.
 
In addition, F<staticperl> can strip perl sources much more effectively
than PAR.
 
=item * The generated executables start much faster.
 
There is no need to unpack files, or even to parse Zip archives (which is
slow and memory-consuming business).
 
=item * The generated executables don't need a writable filesystem.
 
F<staticperl> loads all required files directly from memory. There is no
need to unpack files into a temporary directory.
 
=item * More control over included files, more burden.
 
PAR tries to be maintenance and hassle-free - it tries to include more
files than necessary to make sure everything works out of the box. It
mostly succeeds at this, but he extra files (such as the unicode database)
can take substantial amounts of memory and file size.
 
With F<staticperl>, the burden is mostly with the developer - only direct
compile-time dependencies and L<AutoLoader> are handled automatically.
This means the modules to include often need to be tweaked manually.
 
All this does not preclude more permissive modes to be implemented in
the future, but right now, you have to resolve hidden dependencies
manually.
 
=item * PAR works out of the box, F<staticperl> does not.
 
Maintaining your own custom perl build can be a pain in the ass, and while
F<staticperl> tries to make this easy, it still requires a custom perl
build and possibly fiddling with some modules. PAR is likely to produce
results faster.
 
Ok, PAR never has worked for me out of the box, and for some people,
F<staticperl> does work out of the box, as they don't count "fiddling with
module use lists" against it, but nevertheless, F<staticperl> is certainly
a bit more difficult to use.
 
=back
 
=head1 HOW DOES IT WORK?
 
Simple: F<staticperl> downloads, compile and installs a perl version of
your choice in F<~/.staticperl>. You can add extra modules either by
letting F<staticperl> install them for you automatically, or by using CPAN
and doing it interactively. This usually takes 5-10 minutes, depending on
the speed of your computer and your internet connection.
 
It is possible to do program development at this stage, too.
 
Afterwards, you create a list of files and modules you want to include,
and then either build a new perl binary (that acts just like a normal perl
except everything is compiled in), or you create bundle files (basically C
sources you can use to embed all files into your project).
 
This step is very fast (a few seconds if PPI is not used for stripping, or
the stripped files are in the cache), and can be tweaked and repeated as
often as necessary.
 
=head1 THE F<STATICPERL> SCRIPT
 
This module installs a script called F<staticperl> into your perl
binary directory. The script is fully self-contained, and can be
used without perl (for example, in an uClibc chroot environment). In
fact, it can be extracted from the C<App::Staticperl> distribution
tarball as F<bin/staticperl>, without any installation. The
newest (possibly alpha) version can also be downloaded from
L<http://staticperl.schmorp.de/staticperl>.
 
F<staticperl> interprets the first argument as a command to execute,
optionally followed by any parameters.
 
There are two command categories: the "phase 1" commands which deal with
installing perl and perl modules, and the "phase 2" commands, which deal
with creating binaries and bundle files.
 
=head2 PHASE 1 COMMANDS: INSTALLING PERL
 
The most important command is F<install>, which does basically
everything. The default is to download and install perl 5.12.3 and a few
modules required by F<staticperl> itself, but all this can (and should) be
changed - see L<CONFIGURATION>, below.
 
The command
 
   staticperl install
 
is normally all you need: It installs the perl interpreter in
F<~/.staticperl/perl>. It downloads, configures, builds and installs the
perl interpreter if required.
 
Most of the following F<staticperl> subcommands simply run one or more
steps of this sequence.
 
If it fails, then most commonly because the compiler options I selected
are not supported by your compiler - either edit the F<staticperl> script
yourself or create F<~/.staticperl> shell script where your set working
C<PERL_CCFLAGS> etc. variables.
 
To force recompilation or reinstallation, you need to run F<staticperl
distclean> first.
 
=over 4
 
=item F<staticperl version>
 
Prints some info about the version of the F<staticperl> script you are using.
 
=item F<staticperl fetch>
 
Runs only the download and unpack phase, unless this has already happened.
 
=item F<staticperl configure>
 
Configures the unpacked perl sources, potentially after downloading them first.
 
=item F<staticperl build>
 
Builds the configured perl sources, potentially after automatically
configuring them.
 
=item F<staticperl install>
 
Wipes the perl installation directory (usually F<~/.staticperl/perl>) and
installs the perl distribution, potentially after building it first.
 
=item F<staticperl perl> [args...]
 
Invokes the compiled perl interpreter with the given args. Basically the
same as starting perl directly (usually via F<~/.staticperl/bin/perl>),
but beats typing the path sometimes.
 
Example: check that the Gtk2 module is installed and loadable.
 
   staticperl perl -MGtk2 -e0
 
=item F<staticperl cpan> [args...]
 
Starts an interactive CPAN shell that you can use to install further
modules. Installs the perl first if necessary, but apart from that,
no magic is involved: you could just as well run it manually via
F<~/.staticperl/perl/bin/cpan>, except that F<staticperl> additionally
sets the environment variable C<$PERL> to the path of the perl
interpreter, which is handy in subshells.
 
Any additional arguments are simply passed to the F<cpan> command.
 
=item F<staticperl instcpan> module...
 
Tries to install all the modules given and their dependencies, using CPAN.
 
Example:
 
   staticperl instcpan EV AnyEvent::HTTPD Coro
 
=item F<staticperl instsrc> directory...
 
In the unlikely case that you have unpacked perl modules around and want
to install from these instead of from CPAN, you can do this using this
command by specifying all the directories with modules in them that you
want to have built.
 
=item F<staticperl clean>
 
Deletes the perl source directory (and potentially cleans up other
intermediate files). This can be used to clean up files only needed for
building perl, without removing the installed perl interpreter.
 
At the moment, it doesn't delete downloaded tarballs.
 
The exact semantics of this command will probably change.
 
=item F<staticperl distclean>
 
This wipes your complete F<~/.staticperl> directory. Be careful with this,
it nukes your perl download, perl sources, perl distribution and any
installed modules. It is useful if you wish to start over "from scratch"
or when you want to uninstall F<staticperl>.
 
=back
 
=head2 PHASE 2 COMMANDS: BUILDING PERL BUNDLES
 
Building (linking) a new F<perl> binary is handled by a separate
script. To make it easy to use F<staticperl> from a F<chroot>, the script
is embedded into F<staticperl>, which will write it out and call for you
with any arguments you pass:
 
   staticperl mkbundle mkbundle-args...
 
In the oh so unlikely case of something not working here, you
can run the script manually as well (by default it is written to
F<~/.staticperl/mkbundle>).
 
F<mkbundle> is a more conventional command and expect the argument
syntax commonly used on UNIX clones. For example, this command builds
a new F<perl> binary and includes F<Config.pm> (for F<perl -V>),
F<AnyEvent::HTTPD>, F<URI> and a custom F<httpd> script (from F<eg/httpd>
in this distribution):
 
   # first make sure we have perl and the required modules
   staticperl instcpan AnyEvent::HTTPD
 
   # now build the perl
   staticperl mkperl -MConfig_heavy.pl -MAnyEvent::Impl::Perl \
                     -MAnyEvent::HTTPD -MURI::http \
                     --add 'eg/httpd httpd.pm'
 
   # finally, invoke it
   ./perl -Mhttpd
 
As you can see, things are not quite as trivial: the L<Config> module has
a hidden dependency which is not even a perl module (F<Config_heavy.pl>),
L<AnyEvent> needs at least one event loop backend that we have to
specify manually (here L<AnyEvent::Impl::Perl>), and the F<URI> module
(required by L<AnyEvent::HTTPD>) implements various URI schemes as extra
modules - since L<AnyEvent::HTTPD> only needs C<http> URIs, we only need
to include that module. I found out about these dependencies by carefully
watching any error messages about missing modules...
 
Instead of building a new perl binary, you can also build a standalone
application:
 
   # build the app
   staticperl mkapp app --boot eg/httpd \
                    -MAnyEvent::Impl::Perl -MAnyEvent::HTTPD -MURI::http
 
   # run it
   ./app
 
Here are the three phase 2 commands:
 
=over 4
 
=item F<staticperl mkbundle> args...
 
The "default" bundle command - it interprets the given bundle options and
writes out F<bundle.h>, F<bundle.c>, F<bundle.ccopts> and F<bundle.ldopts>
files, useful for embedding.
 
=item F<staticperl mkperl> args...
 
Creates a bundle just like F<staticperl mkbundle> (in fact, it's the same
as invoking F<staticperl mkbundle --perl> args...), but then compiles and
links a new perl interpreter that embeds the created bundle, then deletes
all intermediate files.
 
=item F<staticperl mkapp> filename args...
 
Does the same as F<staticperl mkbundle> (in fact, it's the same as
invoking F<staticperl mkbundle --app> filename args...), but then compiles
and links a new standalone application that simply initialises the perl
interpreter.
 
The difference to F<staticperl mkperl> is that the standalone application
does not act like a perl interpreter would - in fact, by default it would
just do nothing and exit immediately, so you should specify some code to
be executed via the F<--boot> option.
 
=back
 
=head3 OPTION PROCESSING
 
All options can be given as arguments on the command line (typically
using long (e.g. C<--verbose>) or short option (e.g. C<-v>) style). Since
specifying a lot of options can make the command line very long and
unwieldy, you can put all long options into a "bundle specification file"
(one option per line, with or without C<--> prefix) and specify this
bundle file instead.
 
For example, the command given earlier to link a new F<perl> could also
look like this:
 
   staticperl mkperl httpd.bundle
 
With all options stored in the F<httpd.bundle> file (one option per line,
everything after the option is an argument):
 
   use "Config_heavy.pl"
   use AnyEvent::Impl::Perl
   use AnyEvent::HTTPD
   use URI::http
   add eg/httpd httpd.pm
 
All options that specify modules or files to be added are processed in the
order given on the command line.
 
=head3 BUNDLE CREATION WORKFLOW / STATICPERL MKBUNDLE OPTIONS
 
F<staticperl mkbundle> works by first assembling a list of candidate
files and modules to include, then filtering them by include/exclude
patterns. The remaining modules (together with their direct dependencies,
such as link libraries and L<AutoLoader> files) are then converted into
bundle files suitable for embedding. F<staticperl mkbundle> can then
optionally build a new perl interpreter or a standalone application.
 
=over 4
 
=item Step 0: Generic argument processing.
 
The following options influence F<staticperl mkbundle> itself.
 
=over 4
 
=item C<--verbose> | C<-v>
 
Increases the verbosity level by one (the default is C<1>).
 
=item C<--quiet> | C<-q>
 
Decreases the verbosity level by one.
 
=item any other argument
 
Any other argument is interpreted as a bundle specification file, which
supports all options (without extra quoting), one option per line, in the
format C<option> or C<option argument>. They will effectively be expanded
and processed as if they were directly written on the command line, in
place of the file name.
 
=back
 
=item Step 1: gather candidate files and modules
 
In this step, modules, perl libraries (F<.pl> files) and other files are
selected for inclusion in the bundle. The relevant options are executed
in order (this makes a difference mostly for C<--eval>, which can rely on
earlier C<--use> options to have been executed).
 
=over 4
 
=item C<--use> F<module> | C<-M>F<module>
 
Include the named module or perl library and trace direct
dependencies. This is done by loading the module in a subprocess and
tracing which other modules and files it actually loads.
 
Example: include AnyEvent and AnyEvent::Impl::Perl.
 
   staticperl mkbundle --use AnyEvent --use AnyEvent::Impl::Perl
 
Sometimes you want to load old-style "perl libraries" (F<.pl> files), or
maybe other weirdly named files. To support this, the C<--use> option
actually tries to do what you mean, depending on the string you specify:
 
=over 4
 
=item a possibly valid module name, e.g. F<common::sense>, F<Carp>,
F<Coro::Mysql>.
 
If the string contains no quotes, no F</> and no F<.>, then C<--use>
assumes that it is a normal module name. It will create a new package and
evaluate a C<use module> in it, i.e. it will load the package and do a
default import.
 
The import step is done because many modules trigger more dependencies
when something is imported than without.
 
=item anything that contains F</> or F<.> characters,
e.g. F<utf8_heavy.pl>, F<Module/private/data.pl>.
 
The string will be quoted and passed to require, as if you used C<require
$module>. Nothing will be imported.
 
=item "path" or 'path', e.g. C<"utf8_heavy.pl">.
 
If you enclose the name into single or double quotes, then the quotes will
be removed and the resulting string will be passed to require. This syntax
is form compatibility with older versions of staticperl and should not be
used anymore.
 
=back
 
Example: C<use> AnyEvent::Socket, once using C<use> (importing the
symbols), and once via C<require>, not importing any symbols. The first
form is preferred as many modules load some extra dependencies when asked
to export symbols.
 
   staticperl mkbundle -MAnyEvent::Socket     # use + import
   staticperl mkbundle -MAnyEvent/Socket.pm   # require only
 
Example: include the required files for F<perl -V> to work in all its
glory (F<Config.pm> is included automatically by the dependency tracker).
 
   # shell command
   staticperl mkbundle -MConfig_heavy.pl
 
   # bundle specification file
   use Config_heavy.pl
 
The C<-M>module syntax is included as a convenience that might be easier
to remember than C<--use> - it's the same switch as perl itself uses
to load modules. Or maybe it confuses people. Time will tell. Or maybe
not. Sigh.
 
=item C<--eval> "perl code" | C<-e> "perl code"
 
Sometimes it is easier (or necessary) to specify dependencies using perl
code, or maybe one of the modules you use need a special use statement. In
that case, you can use C<--eval> to execute some perl snippet or set some
variables or whatever you need. All files C<require>'d or C<use>'d while
executing the snippet are included in the final bundle.
 
Keep in mind that F<mkbundle> will not import any symbols from the modules
named by the C<--use> option, so do not expect the symbols from modules
you C<--use>'d earlier on the command line to be available.
 
Example: force L<AnyEvent> to detect a backend and therefore include it
in the final bundle.
 
   staticperl mkbundle --eval 'use AnyEvent; AnyEvent::detect'
 
   # or like this
   staticperl mkbundle -MAnyEvent --eval 'AnyEvent::detect'
 
Example: use a separate "bootstrap" script that C<use>'s lots of modules
and also include this in the final bundle, to be executed automatically
when the interpreter is initialised.
 
   staticperl mkbundle --eval 'do "bootstrap"' --boot bootstrap
 
=item C<--boot> F<filename>
 
Include the given file in the bundle and arrange for it to be
executed (using C<require>) before the main program when the new perl
is initialised. This can be used to modify C<@INC> or do similar
modifications before the perl interpreter executes scripts given on the
command line (or via C<-e>). This works even in an embedded interpreter -
the file will be executed during interpreter initialisation in that case.
 
=item C<--incglob> pattern
 
This goes through all standard library directories and tries to match any
F<.pm> and F<.pl> files against the extended glob pattern (see below). If
a file matches, it is added. The pattern is matched against the full path
of the file (sans the library directory prefix), e.g. F<Sys/Syslog.pm>.
 
This is very useful to include "everything":
 
   --incglob '*'
 
It is also useful for including perl libraries, or trees of those, such as
the unicode database files needed by some perl built-ins, the regex engine
and other modules.
 
   --incglob '/unicore/**.pl'
 
=item C<--add> F<file> | C<--add> "F<file> alias"
 
Adds the given (perl) file into the bundle (and optionally call it
"alias"). The F<file> is either an absolute path or a path relative to the
current directory. If an alias is specified, then this is the name it will
use for C<@INC> searches, otherwise the path F<file> will be used as the
internal name.
 
This switch is used to include extra files into the bundle.
 
Example: embed the file F<httpd> in the current directory as F<httpd.pm>
when creating the bundle.
 
   staticperl mkperl --add "httpd httpd.pm"
 
   # can be accessed via "use httpd"
 
Example: add a file F<initcode> from the current directory.
 
   staticperl mkperl --add 'initcode &initcode'
 
   # can be accessed via "do '&initcode'"
 
Example: add local files as extra modules in the bundle.
 
   # specification file
   add file1 myfiles/file1.pm
   add file2 myfiles/file2.pm
   add file3 myfiles/file3.pl
 
   # then later, in perl, use
   use myfiles::file1;
   require myfiles::file2;
   my $res = do "myfiles/file3.pl";
 
=item C<--addbin> F<file> | C<--addbin> "F<file> alias"
 
Just like C<--add>, except that it treats the file as binary and adds it
without any postprocessing (perl files might get stripped to reduce their
size).
 
If you specify an alias you should probably add a C</> prefix to avoid
clashing with embedded perl files (whose paths never start with C</>),
and/or use a special directory prefix, such as C</res/name>.
 
You can later get a copy of these files by calling C<static::find
"alias">.
 
An alternative way to embed binary files is to convert them to perl and
use C<do> to get the contents - this method is a bit cumbersome, but works
both inside and outside of a staticperl bundle, without extra ado:
 
   # a "binary" file, call it "bindata.pl"
   <<'SOME_MARKER'
   binary data NOT containing SOME_MARKER
   SOME_MARKER
 
   # load the binary
   chomp (my $data = do "bindata.pl");
 
=item C<--allow-dynamic>
 
By default, when F<mkbundle> hits a dynamic perl extension (e.g. a F<.so>
or F<.dll> file), it will stop with a fatal error.
 
When this option is enabled, F<mkbundle> packages the shared
object into the bundle instead, with a prefix of F<!>
(e.g. F<!auto/List/Util/Util.so>). What you do with that is currently up
to you, F<staticperl> has no special support for this at the moment, apart
from working around the lack of availability of F<PerlIO::scalar> while
bootstrapping, at a speed cost.
 
One way to deal with this is to write all files starting with F<!> into
some directory and then C<unshift> that path onto C<@INC>.
 
#TODO: example
 
=back
 
=item Step 2: filter all files using C<--include> and C<--exclude> options.
 
After all candidate files and modules are added, they are I<filtered>
by a combination of C<--include> and C<--exclude> patterns (there is an
implicit C<--include *> at the end, so if no filters are specified, all
files are included).
 
All that this step does is potentially reduce the number of files that are
to be included - no new files are added during this step.
 
=over 4
 
=item C<--include> pattern | C<-i> pattern | C<--exclude> pattern | C<-x> pattern
 
These specify an include or exclude pattern to be applied to the candidate
file list. An include makes sure that the given files will be part of the
resulting file set, an exclude will exclude remaining files. The patterns
are "extended glob patterns" (see below).
 
The patterns are applied "in order" - files included via earlier
C<--include> specifications cannot be removed by any following
C<--exclude>, and likewise, and file excluded by an earlier C<--exclude>
cannot be added by any following C<--include>.
 
For example, to include everything except C<Devel> modules, but still
include F<Devel::PPPort>, you could use this:
 
   --incglob '*' -i '/Devel/PPPort.pm' -x '/Devel/**'
 
=back
 
=item Step 3: add any extra or "hidden" dependencies.
 
F<staticperl> currently knows about three extra types of depdendencies
that are added automatically. Only one (F<.packlist> files) is currently
optional and can be influenced, the others are always included:
 
=over 4
 
=item C<--usepacklists>
 
Read F<.packlist> files for each distribution that happens to match a
module name you specified. Sounds weird, and it is, so expect semantics to
change somehow in the future.
 
The idea is that most CPAN distributions have a F<.pm> file that matches
the name of the distribution (which is rather reasonable after all).
 
If this switch is enabled, then if any of the F<.pm> files that have been
selected match an install distribution, then all F<.pm>, F<.pl>, F<.al>
and F<.ix> files installed by this distribution are also included.
 
For example, using this switch, when the L<URI> module is specified, then
all L<URI> submodules that have been installed via the CPAN distribution
are included as well, so you don't have to manually specify them.
 
=item L<AutoLoader> splitfiles
 
Some modules use L<AutoLoader> - less commonly (hopefully) used functions
are split into separate F<.al> files, and an index (F<.ix>) file contains
the prototypes.
 
Both F<.ix> and F<.al> files will be detected automatically and added to
the bundle.
 
=item link libraries (F<.a> files)
 
Modules using XS (or any other non-perl language extension compiled at
installation time) will have a static archive (typically F<.a>). These
will automatically be added to the linker options in F<bundle.ldopts>.
 
Should F<staticperl> find a dynamic link library (typically F<.so>) it
will warn about it - obviously this shouldn't happen unless you use
F<staticperl> on the wrong perl, or one (probably wrongly) configured to
use dynamic loading.
 
=item extra libraries (F<extralibs.ld>)
 
Some modules need linking against external libraries - these are found in
F<extralibs.ld> and added to F<bundle.ldopts>.
 
=back
 
=item Step 4: write bundle files and optionally link a program
 
At this point, the select files will be read, processed (stripped) and
finally the bundle files get written to disk, and F<staticperl mkbundle>
is normally finished. Optionally, it can go a step further and either link
a new F<perl> binary with all selected modules and files inside, or build
a standalone application.
 
Both the contents of the bundle files and any extra linking is controlled
by these options:
 
=over 4
 
=item C<--strip> C<none>|C<pod>|C<ppi>
 
Specify the stripping method applied to reduce the file of the perl
sources included.
 
The default is C<pod>, which uses the L<Pod::Strip> module to remove all
pod documentation, which is very fast and reduces file size a lot.
 
The C<ppi> method uses L<PPI> to parse and condense the perl sources. This
saves a lot more than just L<Pod::Strip>, and is generally safer,
but is also a lot slower (some files take almost a minute to strip -
F<staticperl> maintains a cache of stripped files to speed up subsequent
runs for this reason). Note that this method doesn't optimise for raw file
size, but for best compression (that means that the uncompressed file size
is a bit larger, but the files compress better, e.g. with F<upx>).
 
Last not least, if you need accurate line numbers in error messages,
or in the unlikely case where C<pod> is too slow, or some module gets
mistreated, you can specify C<none> to not mangle included perl sources in
any way.
 
=item C<--perl>
 
After writing out the bundle files, try to link a new perl interpreter. It
will be called F<perl> and will be left in the current working
directory. The bundle files will be removed.
 
This switch is automatically used when F<staticperl> is invoked with the
C<mkperl> command instead of C<mkbundle>.
 
Example: build a new F<./perl> binary with only L<common::sense> inside -
it will be even smaller than the standard perl interpreter as none of the
modules of the base distribution (such as L<Fcntl>) will be included.
 
   staticperl mkperl -Mcommon::sense
 
=item C<--app> F<name>
 
After writing out the bundle files, try to link a new standalone
program. It will be called C<name>, and the bundle files get removed after
linking it.
 
This switch is automatically used when F<staticperl> is invoked with the
C<mkapp> command instead of C<mkbundle>.
 
The difference to the (mutually exclusive) C<--perl> option is that the
binary created by this option will not try to act as a perl interpreter -
instead it will simply initialise the perl interpreter, clean it up and
exit.
 
This means that, by default, it will do nothing but burn a few CPU cycles
- for it to do something useful you I<must> add some boot code, e.g. with
the C<--boot> option.
 
Example: create a standalone perl binary called F<./myexe> that will
execute F<appfile> when it is started.
 
   staticperl mkbundle --app myexe --boot appfile
 
=item C<--ignore-env>
 
Generates extra code to unset some environment variables before
initialising/running perl. Perl supports a lot of environment variables
that might alter execution in ways that might be undesirablre for
standalone applications, and this option removes those known to cause
trouble.
 
Specifically, these are removed:
 
C<PERL_HASH_SEED_DEBUG> and C<PERL_DEBUG_MSTATS> can cause undesirable
output, C<PERL5OPT>, C<PERL_DESTRUCT_LEVEL>, C<PERL_HASH_SEED> and
C<PERL_SIGNALS> can alter execution significantly, and C<PERL_UNICODE>,
C<PERLIO_DEBUG> and C<PERLIO> can affect input and output.
 
The variables C<PERL_LIB> and C<PERL5_LIB> are always ignored because the
startup code used by F<staticperl> overrides C<@INC> in all cases.
 
This option will not make your program more secure (unless you are
running with elevated privileges), but it will reduce the surprise effect
when a user has these environment variables set and doesn't expect your
standalone program to act like a perl interpreter.
 
=item C<--static>
 
Add C<-static> to F<bundle.ldopts>, which means a fully static (if
supported by the OS) executable will be created. This is not immensely
useful when just creating the bundle files, but is most useful when
linking a binary with the C<--perl> or C<--app> options.
 
The default is to link the new binary dynamically (that means all perl
modules are linked statically, but all external libraries are still
referenced dynamically).
 
Keep in mind that Solaris doesn't support static linking at all, and
systems based on GNU libc don't really support it in a very usable
fashion either. Try uClibc if you want to create fully statically linked
executables, or try the C<--staticlib> option to link only some libraries
statically.
 
=item C<--staticlib> libname
 
When not linking fully statically, this option allows you to link specific
libraries statically. What it does is simply replace all occurrences of
C<-llibname> with the GCC-specific C<-Wl,-Bstatic -llibname -Wl,-Bdynamic>
option.
 
This will have no effect unless the library is actually linked against,
specifically, C<--staticlib> will not link against the named library
unless it would be linked against anyway.
 
Example: link libcrypt statically into the final binary.
 
   staticperl mkperl -MIO::AIO --staticlib crypt
 
   # ldopts might now contain:
   # -lm -Wl,-Bstatic -lcrypt -Wl,-Bdynamic -lpthread
 
=item C<--extra-cflags> string
 
Specifies extra compiler flags, used when compiling the bundle file. The
flags are appended to all the existing flags, so can be sued to override
settings.
 
=item C<--extra-ldflags> string
 
Specifies extra linker flags, used when linking the bundle.
 
=item C<--extra-libs> string
 
Extra linker flags, appended at the end when linking. The difference to
C<--extra-ldflags> is that the ldflags are appended to the flags, before
the objects and libraries, and the extra libs are added at the end.
 
=back
 
=back
 
=head3 EXTENDED GLOB PATTERNS
 
Some options of F<staticperl mkbundle> expect an I<extended glob
pattern>. This is neither a normal shell glob nor a regex, but something
in between. The idea has been copied from rsync, and there are the current
matching rules:
 
=over 4
 
=item Patterns starting with F</> will be a anchored at the root of the library tree.
 
That is, F</unicore> will match the F<unicore> directory in C<@INC>, but
nothing inside, and neither any other file or directory called F<unicore>
anywhere else in the hierarchy.
 
=item Patterns not starting with F</> will be anchored at the end of the path.
 
That is, F<idna.pl> will match any file called F<idna.pl> anywhere in the
hierarchy, but not any directories of the same name.
 
=item A F<*> matches anything within a single path component.
 
That is, F</unicore/*.pl> would match all F<.pl> files directly inside
C</unicore>, not any deeper level F<.pl> files. Or in other words, F<*>
will not match slashes.
 
=item A F<**> matches anything.
 
That is, F</unicore/**.pl> would match all F<.pl> files under F</unicore>,
no matter how deeply nested they are inside subdirectories.
 
=item A F<?> matches a single character within a component.
 
That is, F</Encode/??.pm> matches F</Encode/JP.pm>, but not the
hypothetical F</Encode/J/.pm>, as F<?> does not match F</>.
 
=back
 
=head2 F<STATICPERL> CONFIGURATION AND HOOKS
 
During (each) startup, F<staticperl> tries to source some shell files to
allow you to fine-tune/override configuration settings.
 
In them you can override shell variables, or define shell functions
("hooks") to be called at specific phases during installation. For
example, you could define a C<postinstall> hook to install additional
modules from CPAN each time you start from scratch.
 
If the env variable C<$STATICPERLRC> is set, then F<staticperl> will try
to source the file named with it only. Otherwise, it tries the following
shell files in order:
 
   /etc/staticperlrc
   ~/.staticperlrc
   $STATICPERL/rc
 
Note that the last file is erased during F<staticperl distclean>, so
generally should not be used.
 
=head3 CONFIGURATION VARIABLES
 
=head4 Variables you I<should> override
 
=over 4
 
=item C<EMAIL>
 
The e-mail address of the person who built this binary. Has no good
default, so should be specified by you.
 
=item C<CPAN>
 
The URL of the CPAN mirror to use (e.g. L<http://mirror.netcologne.de/cpan/>).
 
=item C<EXTRA_MODULES>
 
Additional modules installed during F<staticperl install>. Here you can
set which modules you want have to installed from CPAN.
 
Example: I really really need EV, AnyEvent, Coro and AnyEvent::AIO.
 
   EXTRA_MODULES="EV AnyEvent Coro AnyEvent::AIO"
 
Note that you can also use a C<postinstall> hook to achieve this, and
more.
 
=back
 
=head4 Variables you might I<want> to override
 
=over 4
 
=item C<STATICPERL>
 
The directory where staticperl stores all its files
(default: F<~/.staticperl>).
 
=item C<DLCACHE>
 
The path to a directory (will be created if it doesn't exist) where
downloaded perl sources are being cached, to avoid downloading them
again. The default is empty, which means there is no cache.
 
=item C<PERL_VERSION>
 
The perl version to install - C<5.12.5> is a good choice for small builds,
but C<5.8.9> is also a good choice (5.8.9 is much smaller than 5.12.5), if
it builds on your system.
 
You can also set this variable to the absolute URL of a tarball (F<.tar>,
F<.tar.gz>, F<.tar.bz2>, F<.tar.lzma> or F<.tar.xz>), or to the absolute
path of an unpacked perl source tree, which will be copied.
 
The default is currently
F<http://stableperl.schmorp.de/dist/latest.tar.gz>, i.e. the latest
stableperl release.
 
=item C<PERL_MM_USE_DEFAULT>, C<EV_EXTRA_DEFS>, ...
 
Usually set to C<1> to make modules "less inquisitive" during their
installation. You can set (and export!) any environment variable you want
- some modules (such as L<Coro> or L<EV>) use environment variables for
further tweaking.
 
=item C<PERL_PREFIX>
 
The directory where perl gets installed (default: F<$STATICPERL/perl>),
i.e. where the F<bin> and F<lib> subdirectories will end up. Previous
contents will be removed on installation.
 
=item C<PERL_CONFIGURE>
 
Additional Configure options - these are simply passed to the perl
Configure script. For example, if you wanted to enable dynamic loading,
you could pass C<-Dusedl>. To enable ithreads (Why would you want that
insanity? Don't! Use L<forks> instead!) you would pass C<-Duseithreads>
and so on.
 
More commonly, you would either activate 64 bit integer support
(C<-Duse64bitint>), or disable large files support (-Uuselargefiles), to
reduce filesize further.
 
=item C<PERL_CC>, C<PERL_CCFLAGS>, C<PERL_OPTIMIZE>, C<PERL_LDFLAGS>, C<PERL_LIBS>
 
These flags are passed to perl's F<Configure> script, and are generally
optimised for small size (at the cost of performance). Since they also
contain subtle workarounds around various build issues, changing these
usually requires understanding their default values - best look at
the top of the F<staticperl> script for more info on these, and use a
F<~/.staticperlrc> to override them.
 
Most of the variables override (or modify) the corresponding F<Configure>
variable, except C<PERL_CCFLAGS>, which gets appended.
 
The default for C<PERL_OPTIMIZE> is C<-Os> (assuming gcc), and for
C<PERL_LIBS> is C<-lm -lcrypt>, which should be good for most (but not
all) systems.
 
For other compilers or more customised optimisation settings, you need to
adjust these, e.g. in your F<~/.staticperlrc>.
 
With gcc on x86 and amd64, you can get more space-savings by using:
 
   -Os -ffunction-sections -fdata-sections -finline-limit=8 -mpush-args
   -mno-inline-stringops-dynamically -mno-align-stringops
 
And on x86 and pentium3 and newer (basically everything you might ever
want to run on), adding these is even better for space-savings (use
-mtune=core2 or something newer for much faster code, too):
 
   -fomit-frame-pointer -march=pentium3 -mtune=i386
 
=back
 
=head4 Variables you probably I<do not want> to override
 
=over 4
 
=item C<MAKE>
 
The make command to use - default is C<make>.
 
=item C<MKBUNDLE>
 
Where F<staticperl> writes the C<mkbundle> command to
(default: F<$STATICPERL/mkbundle>).
 
=item C<STATICPERL_MODULES>
 
Additional modules needed by C<mkbundle> - should therefore not be changed
unless you know what you are doing.
 
=back
 
=head3 OVERRIDABLE HOOKS
 
In addition to environment variables, it is possible to provide some
shell functions that are called at specific times. To provide your own
commands, just define the corresponding function.
 
The actual order in which hooks are invoked during a full install
from scratch is C<preconfigure>, C<patchconfig>, C<postconfigure>,
C<postbuild>, C<postinstall>.
 
Example: install extra modules from CPAN and from some directories
at F<staticperl install> time.
 
   postinstall() {
      rm -rf lib/threads* # weg mit Schaden
      instcpan IO::AIO EV
      instsrc ~/src/AnyEvent
      instsrc ~/src/XML-Sablotron-1.0100001
      instcpan Anyevent::AIO AnyEvent::HTTPD
   }
 
=over 4
 
=item preconfigure
 
Called just before running F<./Configure> in the perl source
directory. Current working directory is the perl source directory.
 
This can be used to set any C<PERL_xxx> variables, which might be costly
to compute.
 
=item patchconfig
 
Called after running F<./Configure> in the perl source directory to create
F<./config.sh>, but before running F<./Configure -S> to actually apply the
config. Current working directory is the perl source directory.
 
Can be used to tailor/patch F<config.sh> or do any other modifications.
 
=item postconfigure
 
Called after configuring, but before building perl. Current working
directory is the perl source directory.
 
=item postbuild
 
Called after building, but before installing perl. Current working
directory is the perl source directory.
 
I have no clue what this could be used for - tell me.
 
=item postinstall
 
Called after perl and any extra modules have been installed in C<$PREFIX>,
but before setting the "installation O.K." flag.
 
The current working directory is C<$PREFIX>, but maybe you should not rely
on that.
 
This hook is most useful to customise the installation, by deleting files,
or installing extra modules using the C<instcpan> or C<instsrc> functions.
 
The script must return with a zero exit status, or the installation will
fail.
 
=back
 
=head1 ANATOMY OF A BUNDLE
 
When not building a new perl binary, C<mkbundle> will leave a number of
files in the current working directory, which can be used to embed a perl
interpreter in your program.
 
Intimate knowledge of L<perlembed> and preferably some experience with
embedding perl is highly recommended.
 
C<mkperl> (or the C<--perl> option) basically does this to link the new
interpreter (it also adds a main program to F<bundle.>):
 
   $Config{cc} $(cat bundle.ccopts) -o perl bundle.c $(cat bundle.ldopts)
 
=over 4
 
=item bundle.h
 
A header file that contains the prototypes of the few symbols "exported"
by bundle.c, and also exposes the perl headers to the application.
 
=over 4
 
=item staticperl_init (xs_init = 0)
 
Initialises the perl interpreter. You can use the normal perl functions
after calling this function, for example, to define extra functions or
to load a .pm file that contains some initialisation code, or the main
program function:
 
   XS (xsfunction)
   {
     dXSARGS;
 
     // now we have items, ST(i) etc.
   }
 
   static void
   run_myapp(void)
   {
      staticperl_init (0);
      newXSproto ("myapp::xsfunction", xsfunction, __FILE__, "$$;$");
      eval_pv ("require myapp::main", 1); // executes "myapp/main.pm"
   }
 
When your bootcode already wants to access some XS functions at
compiletime, then you need to supply an C<xs_init> function pointer that
is called as soon as perl is initialised enough to define XS functions,
but before the preamble code is executed:
 
   static void
   xs_init (pTHX)
   {
     newXSproto ("myapp::xsfunction", xsfunction, __FILE__, "$$;$");
   }
 
   static void
   run_myapp(void)
   {
      staticperl_init (xs_init);
   }
 
=item staticperl_cleanup ()
 
In the unlikely case that you want to destroy the perl interpreter, here
is the corresponding function.
 
=item staticperl_xs_init (pTHX)
 
Sometimes you need direct control over C<perl_parse> and C<perl_run>, in
which case you do not want to use C<staticperl_init> but call them on your
own.
 
Then you need this function - either pass it directly as the C<xs_init>
function to C<perl_parse>, or call it as one of the first things from your
own C<xs_init> function.
 
=item PerlInterpreter *staticperl
 
The perl interpreter pointer used by staticperl. Not normally so useful,
but there it is.
 
=back
 
=item bundle.ccopts
 
Contains the compiler options required to compile at least F<bundle.c> and
any file that includes F<bundle.h> - you should probably use it in your
C<CFLAGS>.
 
=item bundle.ldopts
 
The linker options needed to link the final program.
 
=back
 
=head1 RUNTIME FUNCTIONALITY
 
Binaries created with C<mkbundle>/C<mkperl> contain extra functionality,
mostly related to the extra files bundled in the binary (the virtual
filesystem). All of this data is statically compiled into the binary, and
accessing means copying it from a read-only section of your binary. Data
pages in this way are usually freed by the operating system, as they aren't
used more then once.
 
=head2 VIRTUAL FILESYSTEM
 
Every bundle has a virtual filesystem. The only information stored in it
is the path and contents of each file that was bundled.
 
=head3 LAYOUT
 
Any paths starting with an ampersand (F<&>) or exclamation mark (F<!>) are
reserved by F<staticperl>. They must only be used as described in this
section.
 
=over 4
 
=item !
 
All files that typically cannot be loaded from memory (such as dynamic
objects or shared libraries), but have to reside in the filesystem, are
prefixed with F<!>. Typically these files get written out to some
(semi-)temporary directory shortly after program startup, or before being
used.
 
=item !boot
 
The bootstrap file, if specified during bundling.
 
=item !auto/
 
Shared objects or dlls corresponding to dynamically-linked perl extensions
are stored with an F<!auto/> prefix.
 
=item !lib/
 
External shared libraries are stored in this directory.
 
=item any letter
 
Any path starting with a letter is a perl library file. For example,
F<Coro/AIO.pm> corresponds to the file loaded by C<use Coro::AIO>, and
F<Coro/jit.pl> corresponds to C<require "Coro/jit.pl">. 
 
Obviously, module names shouldn't start with any other characters than
letters :)
 
=back
 
=head3 FUNCTIONS
 
=over 4
 
=item $file = static::find $path
 
Returns the data associated with the given C<$path>
(e.g. C<Digest/MD5.pm>, C<auto/POSIX/autosplit.ix>).
 
Returns C<undef> if the file isn't embedded.
 
=item @paths = static::list
 
Returns the list of all paths embedded in this binary.
 
=back
 
=head2 EXTRA FEATURES
 
In addition, for the embedded loading of perl files to work, F<staticperl>
overrides the C<@INC> array.
 
=head1 FULLY STATIC BINARIES - ALPINE LINUX
 
This section once contained a way to build fully static (including
uClibc) binaries with buildroot. Unfortunately, buildroot no longer
supports a compiler, so I recommend using alpine linux instead
(L<http://alpinelinux.org/>). Get yourself a VM (e.g. with qemu), run an
older alpine linux verison in it (e.g. 2.4), copy staticperl inside and
use it.
 
The reason you might want an older alpine linux is that uClibc can be
quite dependent on kernel versions, so the newest version of alpine linux
might need a newer kernel then you might want for, if you plan to run your
binaries on on other kernels.
 
=head1 RECIPES / SPECIFIC MODULES
 
This section contains some common(?) recipes and information about
problems with some common modules or perl constructs that require extra
files to be included.
 
=head2 MODULES
 
=over 4
 
=item utf8
 
Some functionality in the utf8 module, such as swash handling (used
for unicode character ranges in regexes) is implemented in the
C<"utf8_heavy.pl"> library:
 
   -Mutf8_heavy.pl
 
Many Unicode properties in turn are defined in separate modules,
such as C<"unicore/Heavy.pl"> and more specific data tables such as
C<"unicore/To/Digit.pl"> or C<"unicore/lib/Perl/Word.pl">. These tables
are big (7MB uncompressed, although F<staticperl> contains special
handling for those files), so including them only on demand in your
application might pay off.
 
To simply include the whole unicode database, use:
 
   --incglob '/unicore/**.pl'
 
=item AnyEvent
 
AnyEvent needs a backend implementation that it will load in a delayed
fashion. The L<AnyEvent::Impl::Perl> backend is the default choice
for AnyEvent if it can't find anything else, and is usually a safe
fallback. If you plan to use e.g. L<EV> (L<POE>...), then you need to
include the L<AnyEvent::Impl::EV> (L<AnyEvent::Impl::POE>...) backend as
well.
 
If you want to handle IRIs or IDNs (L<AnyEvent::Util> punycode and idn
functions), you also need to include C<"AnyEvent/Util/idna.pl"> and
C<"AnyEvent/Util/uts46data.pl">.
 
Or you can use C<--usepacklists> and specify C<-MAnyEvent> to include
everything.
 
=item Cairo
 
See Glib, same problem, same solution.
 
=item Carp
 
Carp had (in older versions of perl) a dependency on L<Carp::Heavy>. As of
perl 5.12.2 (maybe earlier), this dependency no longer exists.
 
=item Config
 
The F<perl -V> switch (as well as many modules) needs L<Config>, which in
turn might need L<"Config_heavy.pl">. Including the latter gives you
both.
 
=item Glib
 
Glib literally requires Glib to be installed already to build - it tries
to fake this by running Glib out of the build directory before being
built. F<staticperl> tries to work around this by forcing C<MAN1PODS> and
C<MAN3PODS> to be empty via the C<PERL_MM_OPT> environment variable.
 
=item Gtk2
 
See Pango, same problems, same solution.
 
=item Net::SSLeay
 
This module hasn't been significantly updated since OpenSSL is called
OpenSSL, and fails to properly link against dependent libraries, most
commonly, it forgets to specify -ldl when linking.
 
On GNU/Linux systems this usually goes undetected, as perl usually links
against -ldl itself and OpenSSL just happens to pick it up that way, by
chance.
 
For static builds, you either have to configure -ldl manually, or you
cna use the following snippet in your C<postinstall> hook which patches
Net::SSLeay after installation, which happens to work most of the time:
 
   postinstall() {
      # first install it
      instcpan Net::SSLeay
      # then add -ldl for future linking
      chmod u+w "$PERL_PREFIX"/lib/auto/Net/SSLeay/extralibs.ld
      echo " -ldl" >>"$PERL_PREFIX"/lib/auto/Net/SSLeay/extralibs.ld
   }
 
=item Pango
 
In addition to the C<MAN3PODS> problem in Glib, Pango also routes around
L<ExtUtils::MakeMaker> by compiling its files on its own. F<staticperl>
tries to patch L<ExtUtils::MM_Unix> to route around Pango.
 
=item Term::ReadLine::Perl
 
Also needs L<Term::ReadLine::readline>, or C<--usepacklists>.
 
=item URI
 
URI implements schemes as separate modules - the generic URL scheme is
implemented in L<URI::_generic>, HTTP is implemented in L<URI::http>. If
you need to use any of these schemes, you should include these manually,
or use C<--usepacklists>.
 
=back
 
=head2 RECIPES
 
=over 4
 
=item Just link everything in
 
To link just about everything installed in the perl library into a new
perl, try this (the first time this runs it will take a long time, as a
lot of files need to be parsed):
 
   staticperl mkperl -v --strip ppi --incglob '*'
 
If you don't mind the extra megabytes, this can be a very effective way of
creating bundles without having to worry about forgetting any modules.
 
You get even more useful variants of this method by first selecting
everything, and then excluding stuff you are reasonable sure not to need -
L<bigperl|http://staticperl.schmorp.de/bigperl.html> uses this approach.
 
=item Getting rid of netdb functions
 
The perl core has lots of netdb functions (C<getnetbyname>, C<getgrent>
and so on) that few applications use. You can avoid compiling them in by
putting the following fragment into a C<preconfigure> hook:
 
   preconfigure() {
      for sym in \
         d_getgrnam_r d_endgrent d_endgrent_r d_endhent \
         d_endhostent_r d_endnent d_endnetent_r d_endpent \
         d_endprotoent_r d_endpwent d_endpwent_r d_endsent \
         d_endservent_r d_getgrent d_getgrent_r d_getgrgid_r \
         d_getgrnam_r d_gethbyaddr d_gethent d_getsbyport \
         d_gethostbyaddr_r d_gethostbyname_r d_gethostent_r \
         d_getlogin_r d_getnbyaddr d_getnbyname d_getnent \
         d_getnetbyaddr_r d_getnetbyname_r d_getnetent_r \
         d_getpent d_getpbyname d_getpbynumber d_getprotobyname_r \
         d_getprotobynumber_r d_getprotoent_r d_getpwent \
         d_getpwent_r d_getpwnam_r d_getpwuid_r d_getsent \
         d_getservbyname_r d_getservbyport_r d_getservent_r \
         d_getspnam_r d_getsbyname
         # d_gethbyname
      do
         PERL_CONFIGURE="$PERL_CONFIGURE -U$sym"
      done
   }
 
This mostly gains space when linking statically, as the functions will
likely not be linked in. The gain for dynamically-linked binaries is
smaller.
 
Also, this leaves C<gethostbyname> in - not only is it actually used
often, the L<Socket> module also exposes it, so leaving it out usually
gains little. Why Socket exposes a C function that is in the core already
is anybody's guess.
 
=back
 
=head1 ADDITIONAL RESOURCES
 
Some guy has made a repository on github
(L<https://github.com/gh0stwizard/staticperl-modules>) with some modules
patched to build with staticperl.
 
=head1 AUTHOR
 
 Marc Lehmann <schmorp@schmorp.de>
 http://software.schmorp.de/pkg/staticperl.html

MetaCPAN
About
Sponsor
grep::cpan
Recent
FAQ
Tools
API
Perl.org
Bytemark logo
liquidweb logo
Deriv logo
Geocode logo
OpenCage logo
