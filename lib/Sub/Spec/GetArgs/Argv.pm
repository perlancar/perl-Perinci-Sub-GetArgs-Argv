package Sub::Spec::GetArgs::Argv;

use 5.010;
use strict;
use warnings;
use Log::Any '$log';

use Object::BlankStr;
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

This function also takes ~arg_pos~ and ~arg_greedy~ type clause in schema into
account, for example:

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

If set to 0, will still return parsed argv even if there are parsing errors.

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
my $_pa_skip_check_required_args;
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

    $_pa_skip_check_required_args = 0;

    $log->tracef("GetOptions rule: %s", \%go_spec);
    Getopt::Long::Configure(
        $strict ? "no_pass_through" : "pass_through",
        "no_ignore_case", "permute");
    my $result = Getopt::Long::GetOptionsFromArray($argv, %go_spec);
    unless ($result) {
        die Object::BlankStr->new if $strict;
    }

    # process arg_pos
  ARGV:
    for my $i (reverse 0..@$argv-1) {
        while (my ($name, $schema) = each %$args_spec) {
            my $ah0 = $schema->{attr_hashes}[0];
            my $o = $ah0->{arg_pos};
            if (defined($o) && $o == $i) {
                if (defined($args->{$name})) {
                    die "You specified option --$name but also argument #".
                        ($i+1)."\n" if $strict;
                }
                if ($ah0->{arg_greedy}) {
                    $args->{$name} = [splice(@$argv, $i)];
                    my $j = $i;
                    last ARGV;
                } else {
                    $args->{$name} = splice(@$argv, $i, 1);
                }
            }
        }
    }

    #$log->tracef("tmp args result (after arg_pos processing): %s, argv: %s",
    #             $args, $argv);
    if (@$argv) {
        die "Error: extra argument(s): ".join(", ", @$argv)."\n"
            if $strict;
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
