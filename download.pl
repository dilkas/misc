#!/usr/bin/perl
use strict;
use LWP::UserAgent;

my $url = "http://www.skill-capped.com/gladiators/ajax/keybinds.php?character_id=";
my $dir = "keybinds";

mkdir($dir);
my $ua = LWP::UserAgent->new();
for my $i (1 .. 500) {
    my $req = new HTTP::Request GET => $url . $i;
    my $res = $ua->request($req);
    my $content = $res->content;
    if ($content eq "") {
        next;
    }
    open(my $file, '>', $dir . "/" . $i . ".html");
    print $file $content;
    close $file;
}
