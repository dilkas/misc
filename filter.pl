#!/usr/bin/perl
use strict;

my @files = <keybinds/*>;
for my $file (@files) {
    open FILE, $file;
    my $content = join("", <FILE>);
    close FILE;
    my $result = index($content, "has not set any keybinds.");
    if ($result != -1) {
        unlink $file;
    }
}
