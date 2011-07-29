package Sub::Spec::GetArgs::Argv;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Object::BlankStr;
use Sub::Spec::GetArgs::Array qw(get_args_from_array);
use Sub::Spec::Utils; # temp, for _parse_schema

use Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw(get_args_from_argv);

# VERSION

our %SPEC;

sub _parse_schema {
    Sub::Spec::Utils::_parse_schema(@_);
}

$SPEC{get_args_from_argv} = {
    summary => 'Get subroutine arguments (%args) from command-line arguments '.
        '(@ARGV)',
    description_fmt => 'org',
    description => <<'_',

Using information in sub spec's ~args~ clause, parse command line arguments
~@argv~ into hash ~%args~, suitable for passing into subs.

Uses Getopt::Long's GetOptions to parse the result.

As with GetOptions, this function modifies its ~argv~ argument.

Why would one use this function instead of using Getopt::Long directly? Among
other reasons, we want YAML parsing (ability to pass data structures via command
line) and parsing of arg_pos and arg_greedy.

* How this routine translates the args spec clause

Bool types can be specified using

: --argname

or

: --noargname

All the other types can be specified using

: --argname VALUE

or

: --argname=VALUE

VALUE will be parsed as YAML for nonscalar types.

Argument aliases (~arg_aliases~) clause for each argument is also parsed. For
example:

: args => {
:     argname => [bool => {
:         summary => '...',
:         arg_aliases => {
:             a => {},
:             arg => {},
:         },
:     }
: }

Then -a and --arg are also available in addition to --argname.

This function also (using [[cpanmod:Sub::Spec::GetArgs::Array]]) groks ~arg_pos~
and ~arg_greedy~ type clause, for example:

: $SPEC{multiply2} = {
:     summary => 'Multiply 2 numbers (a & b)',
:     args => {
:         a => ['num*' => {arg_pos=>0}],
:         b => ['num*' => {arg_pos=>1}],
:     }
: }

then on the command-line any of below is valid:

: % multiply2 --a 2 --b 3
: % multiply2 2 --b 3; # first non-option argument is fed into a (arg_pos=0)
: % multiply2 2 3;     # first argument is fed into a, second into b (arg_pos=1)

_
    args => {
        argv => ['array*' => {
            of => 'str*',
        }],
        spec => ['hash*' => {
        }],
        strict => ['bool' => {
            summary => 'Strict mode',
            description => <<'_',

If set to 0, will still return parsed argv even if there are parsing errors. If
set to 1 (the default), will die upon error.

Normally you would want to use strict mode, for more error checking. Setting off
strict is used by, for example, Sub::Spec::BashComplete.

_
            default => 1,
        }],
        extra_getopts => ['hash' => {
            summary => 'Specify extra Getopt::Long specification',
            description => <<'_',

If specified, add extra Getopt::Long specification (as long as it doesn't clash
with spec arg). This is used, for example, by Sub::Spec::CmdLine::run() to add
general options --help, --version, --list, etc so it can mixed with spec arg
options, for convenience.

_
        }]
    },
    result_naked => 1,
};
our $_pa_skip_check_required_args;
sub get_args_from_argv {
    require Getopt::Long;
    require YAML::Syck; $YAML::Syck::ImplicitTyping = 1;

    my %input_args = @_;
    my $argv      = $input_args{argv} or return [400, "Please specify argv"];
    my $sub_spec  = $input_args{spec} or return [400, "Please specify spec"];
    my $args_spec = $sub_spec->{args} // {};
    $args_spec    = { map { $_ => _parse_schema($args_spec->{$_}) }
                          keys %$args_spec };
    my $strict    = $input_args{strict} // 1;
    my $extra_go  = $input_args{extra_getopts} // {};
    $log->tracef("-> get_args_from_argv(), argv=%s", $argv);

    my %go_spec;

    $_pa_skip_check_required_args = 0;

    my $args = {};
    while (my ($name, $schema) = each %$args_spec) {
        my $opt;
        my @name = ($name);
        push @name, $name if $name =~ s/_/-/g; # allow --foo_bar and --foo-bar
        for (@name) {
            if ($schema->{type} eq 'bool') {
                $opt = "$_!";
            } else {
                $opt = "$_=s";
            }
            #$go_spec{$opt} = sub { $args->{$name[0]} = $_[0] };
            $go_spec{$opt} = \$args->{$name[0]};
        }
        my $aliases = $schema->{attr_hashes}[0]{arg_aliases};
        if ($aliases) {
            while (my ($alias, $alinfo) = each %$aliases) {
                my $opt;
                if ($schema->{type} eq 'bool') {
                    $opt = "$alias!";
                } else {
                    $opt = "$alias=s";
                }
                if ($alinfo->{code}) {
                    $go_spec{$opt} = sub {
                        $alinfo->{code}->(
                            args    => $args,
                            arg_ref => \$args->{$name[0]},
                        );
                    };
                } else {
                    $go_spec{$opt} = \$args->{$name[0]};
                }
            }
        }
    }

    while (my ($k, $v) = each %$extra_go) {
        my $k_ = $k; $k_ =~ s/-/_/g;
        next if $go_spec{$k} || $go_spec{"--$k"} || $args_spec->{$k_};
        $go_spec{$k} = $v;
    }

    $log->tracef("GetOptions rule: %s", \%go_spec);
    my $old_go_opts = Getopt::Long::Configure(
        $strict ? "no_pass_through" : "pass_through",
        "no_ignore_case", "permute");
    my $result = Getopt::Long::GetOptionsFromArray($argv, %go_spec);
    Getopt::Long::Configure($old_go_opts);
    unless ($result) {
        die Object::BlankStr->new if $strict;
    }

    # process arg_pos
    if (@$argv) {
        my $res = get_args_from_array(
            array=>$argv, _args_spec=>$args_spec,
        );
        if ($res->[0] != 200 && $strict) {
            die "Error: $res->[0] - $res->[1]\n";
        } elsif ($res->[0] == 200) {
            my $pos_args = $res->[2];
            for my $name (keys %$pos_args) {
                # should've been exists(), but currently uses defined()
                # because our Getopt::Long assigns undef to all opts
                if (defined $args->{$name}) {
                    die "You specified option --$name but also argument #".
                        $args_spec->{$name}{attr_hashes}[0]{arg_pos}
                            if $strict;
                }
                $args->{$name} = $pos_args->{$name};
            }
        }
    }

    # check required args & parse yaml/etc
    unless ($_pa_skip_check_required_args) {
        while (my ($name, $schema) = each %$args_spec) {
            if ($schema->{attr_hashes}[0]{required} &&
                    !defined($args->{$name})) {
                die "Missing required argument: $name\n" if $strict;
            }
            my $parse_yaml;
            my $type = $schema->{type};
            # XXX more proper checking, e.g. check any/all recursively for
            # nonscalar types. check base type.
            $log->tracef("name=%s, arg=%s, parse_yaml=%s",
                         $name, $args->{$name}, $parse_yaml);
            $parse_yaml++ unless $type =~ /^(str|num|int|float|bool)$/;
            if ($parse_yaml && defined($args->{$name})) {
                if (ref($args->{$name}) eq 'ARRAY') {
                    # XXX check whether each element needs to be YAML or not
                    $args->{$name} = [
                        map { YAML::Syck::Load($_) } @{$args->{$name}}
                    ];
                } elsif (!ref($args->{$name})) {
                    $args->{$name} = YAML::Syck::Load($args->{$name});
                } else {
                    die "BUG: Why is \$args->{$name} ".
                        ref($args->{$name})."?";
                }
            }

            # XXX special parsing of type = date, accept
        }
    }

    # cleanup undefined args
    for (keys %$args) {
        delete $args->{$_} unless defined($args->{$_});
    }

    $log->tracef("<- get_args_from_argv(), args=%s, remaining argv=%s",
                 $args, $argv);
    $args;
}

1;
#ABSTRACT: Get subroutine arguments from command line arguments (@ARGV)
__END__

=head1 SYNOPSIS

 use Sub::Spec::GetArgs::Argv;

 my $res = get_args_from_argv(argv=>\@ARGV, spec=>$spec, ...);


=head1 DESCRIPTION

This module provides get_args_from_argv(), which parses command line arguments
(@ARGV) into subroutine arguments (%args). This module is used by
L<Sub::Spec::CmdLine>.

This module uses L<Log::Any> for logging framework.

This module's functions has L<Sub::Spec> specs.


=head1 FUNCTIONS

None are exported by default, but they are exportable.


=head1 FAQ


=head1 SEE ALSO

L<Sub::Spec>

L<Sub::Spec::CmdLine>

=cut
