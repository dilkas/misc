#!/usr/bin/perl

# pagrinde sita programa pasidariau kad nebutu
# mano muzikos dir`e tokiu pavadinimu: 81-andrius_mamontavas-tyli32_juoda_naktis384.mp3

print "\/\/ RENAMER\n";
opendir( KAT, "./" );
if ( $ARGV[0] eq "0" ) {
    while ( $a = readdir(KAT) ) {
        chomp($a);
        $old = $a;
        $a =~ s/[0-9]//;
        #while ( $a =~ /[0-9]/ ) {
        #    $a =~ s/[0-9]//;
        #}
        print $a. "\n";
        if ( $a =~ /.mp/ ) { $a =~ s/.mp//; rename( $old, $a . ".mp3" ); }
    }
}
elsif ( $ARGV[0] eq "3" ) {
    while ( $a = readdir(KAT) ) {
        chomp($a);
        $old = $a;
        $del = $ARGV[1];
        $a =~ s/$del//;
        while ( $a =~ /$del/ ) {
            $a =~ s/$del//;
        }
        print $a. "\n";
        rename( $old, $a );
    }
}
elsif ( $ARGV[0] eq "1" ) {
    while ( $a = readdir(KAT) ) {
        chomp($a);
        $old = $a;
        $a =~ s/-/ - /;
        print $a. "\n";
        rename( $old, $a );
    }
}
elsif ( $ARGV[0] eq "2" ) {
    while ( $old = readdir(KAT) ) {
        chomp($old);
        $smth = $ARGV[1];
        $smth =~ s/'//;
        $a = $smth . $old;
        print $a. "\n";
        rename( $old, $a );
    }
}
elsif ( $ARGV[0] eq "4" ) {
    while ( $old = readdir(KAT) ) {
        chomp($old);
        $smth = $ARGV[1];
        $a = $old . $smth;
        print $a. "\n";
        rename( $old, $a );
    }
}
else {
    print "./program.pl 3 smth   \| to delete all \"smth\"\n";
    print "./program.pl 0        \| to delete all numbers\n";
    print "./program.pl 1        \| to change '-' to ' - '\n";
    print "./program.pl 2 'smth' \| to add smth to the beginning\n";
    print "./program.pl 4 smth   \| to add smth to the end\n";
}
