#!perl

use 5.010;
use strict;
use warnings;
use Log::Any '$log';
use Test::More 0.96;

use Data::Clone qw(clone);
use Sub::Spec::GetArgs::Argv qw(get_args_from_argv);

my $spec = {
    args => {
        arg1 => ['str*' => {arg_pos=>0}],
        arg2 => ['str*' => {arg_pos=>1}],
        arg3 => 'str',
        arg4 => 'array',
        arg5 => 'hash',
    },
};

# XXX check bool arg

test_getargs(spec=>$spec, argv=>[qw/--arg1 1 --arg2 2/],
           args=>{arg1=>1, arg2=>2},
           name=>"optional missing = ok");
test_getargs(spec=>$spec, argv=>[qw/--arg1 1 --arg2 2/],
           args=>{arg1=>1, arg2=>2},
           name=>"optional given = ok");
test_getargs(spec=>$spec, argv=>[qw/1 2/],
           args=>{arg1=>1, arg2=>2},
           name=>"arg_pos");
test_getargs(spec=>$spec, argv=>[qw/1 2 --arg3 3/],
           args=>{arg1=>1, arg2=>2, arg3=>3},
           name=>"mixed arg_pos with opts (1)");
test_getargs(spec=>$spec, argv=>[qw/1 --arg2 2/],
           args=>{arg1=>1, arg2=>2},
           name=>"mixed arg_pos with opts (2)");
test_getargs(spec=>$spec, argv=>[qw/--arg1 1 2/], error=>1,
           name=>"mixed arg_pos with opts (clash)");
test_getargs(spec=>$spec, argv=>[qw/--arg1 1 --arg2 2 3/], error=>1,
           name=>"extra args given = fails (1)");
test_getargs(spec=>$spec, argv=>[qw/1 2 3/], error=>1,
           name=>"extra args given = fails (2)");

test_getargs(spec=>$spec, argv=>[qw/arg1/], error=>1,
           name=>"required missing = fails");
test_getargs(spec=>$spec, argv=>[qw/--foo bar/], error=>1,
           name=>"unknown args given = fails");
test_getargs(spec=>$spec, argv=>[qw/--arg1 1 --arg2 2/],
           args=>{arg1=>1, arg2=>2},
           name=>"optional missing = ok");

test_getargs(spec=>$spec, argv=>['--arg1', '{foo: false}',
                               '--arg2', '',
                               '--arg5', '{foo: false}'],
           args=>{arg1=>'{foo: false}', arg2=>'', arg5=>{foo=>""}},
           name=>"yaml parsing, done on nonscalars");
test_getargs(spec=>$spec, argv=>['--arg1', '{foo: false}',
                               '--arg2', '',
                               '--arg5', '{foo: false'],
           error=>1,
           name=>"yaml syntax error");

{
    my $extra;
    test_getargs(spec=>$spec, argv=>[qw/--arg1 1 --arg2 2 --extra/],
                 extra_getopts=>{extra=>sub{$extra=5}},
                 args=>{arg1=>1, arg2=>2},
                 post_test=>sub {
                     is($extra, 5, "extra getopt is executed");
                 },
                 name=>"opt: extra_getopts",
             );
}

test_getargs(spec=>{args=>{arg1=>'str*'}}, argv=>[qw/--arg1 1 --arg2 2/],
             strict=>1, # the default
             error=>1,
             name=>"opt: strict=1",
         );
test_getargs(spec=>{args=>{arg1=>'str*'}}, argv=>[qw/--arg1 1 --arg2 2/],
             strict=>0,
             args=>{arg1=>1},
             name=>"opt: strict=0",
       );

$spec = {
    args => {
        foo_bar_baz => 'int',
    },
};
test_getargs(name=>"dash alias for underscore (1)",
             spec=>$spec, argv=>[qw/--foo_bar_baz 2/],
             args=>{foo_bar_baz=>2},
       );
test_getargs(name=>"dash alias for underscore (2)",
             spec=>$spec, argv=>[qw/--foo-bar-baz 2/],
             args=>{foo_bar_baz=>2},
       );
test_getargs(name=>"dash alias for underscore (3)",
             spec=>$spec, argv=>[qw/--foo-bar_baz 2/],
             error=>1,
         );

DONE_TESTING:
done_testing();

sub test_getargs {
    my (%args) = @_;

    subtest $args{name} => sub {
        my $argv = clone($args{argv});
        my $res;
        my %input_args = (argv=>$argv, spec=>$args{spec});
        $input_args{strict} = $args{strict} if defined $args{strict};
        $input_args{extra_getopts} = $args{extra_getopts}
            if defined $args{extra_getopts};
        eval {
            $res = get_args_from_argv(%input_args);
        };
        my $eval_err = $@;
        diag "eval_err = $eval_err" if "$eval_err";
        if ($args{error}) {
            ok($eval_err, "dies");
        } else {
            is_deeply($res, $args{args}, "result")
                or diag explain $res;
        }

        if ($args{post_test}) {
            $args{post_test}->();
        }
    }
}

