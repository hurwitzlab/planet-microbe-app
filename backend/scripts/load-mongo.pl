#!/usr/bin/env perl

use strict;
use DateTime::Format::ISO8601;
use feature 'say';
use Data::Dump 'dump';
use DateTime;
use File::Find::Rule;
use File::Spec::Functions;
use MongoDB;
use Text::RecordParser::Tab;
use Regexp::Common;

$MongoDB::BSON::looks_like_number = 1;

my $file  = shift(@ARGV) or die "No input file\n";
my $mongo = MongoDB::MongoClient->new(
    host  => "localhost", #'lipothrix.hpc.arizona.edu',
    port  => 27017,
) or die 'No Mongo conneciton';
my $db    = $mongo->get_database('pm')    or die 'No pm db';
my $coll  = $db->get_collection('sample') or die 'No sample collection';

#
# This was supposed to ensure that a "2dsphere" index exists on the 
# "location" but it doesn't seem to work on Mongo 4
#
#if (my $indexes = $coll->indexes) {
#    $indexes->create_one(
#        ['location' => 1], 
#        { options => { '2dsphere' => 1 } },
#    );
#}

my %fld_alias           = (
    alkalinity          => 'alkalin',
    chlorophyll_a       => 'chl_a',
    chlorophyll_b       => 'chl_b',
    collection_date     => '',
    depth_max           => 'depth',
    latitude            => 'lat',
    longitude           => 'lon',
    nitrate_and_nitrite => 'no2_no3',
    oxygen              => '',
    phosphate           => 'phspht',
    pres_mas            => 'pressure',
    salinity            => '',
    salinity_ctd        => 'ctdsal',
    sigma               => '',
    temperature_ctd     => 'ctdtmp',
);

my $p = Text::RecordParser::Tab->new($file);
$p->header_filter( sub { $_ = shift; s/\s+/_/g; lc $_ } );

my $i = 0;
while (my $rec = $p->fetchrow_hashref) {
    #say dump($rec);
    my $cruise_num  = $rec->{'cruise_name'} or next;
    my %sample = (
        project => 'HOT',
        sample  => join('-', 
            "HOT$cruise_num", 
            $rec->{'stnnbr'}, 
            $rec->{'castno'},
            $rec->{'rosette'},
        ),
    );

    my $lat = $rec->{'lat'};
    my $lon = $rec->{'lon'};

    if ($lat =~ $RE{'num'}{'real'} && $lon =~ $RE{'num'}{'real'}) {
        $sample{'location'} = {
              type => 'Point',
              coordinates => [$lon + 0, $lat + 0]
        };
    }

    if (my $iso = $rec->{'iso_datetime'}) {
        $iso .= 'Z' unless $iso =~ /Z/;
        if (my $dt = DateTime::Format::ISO8601->parse_datetime($iso)) {
            $sample{'collected'} = $dt;
        }
    }
    elsif ($rec->{'year'} && $rec->{'month'} && $rec->{'day'}) {
        my $dt = DateTime->new(
            year  => $rec->{'year'}, 
            month => $rec->{'month'}, 
            day   => $rec->{'day'}
        );

        if ($dt) {
            $sample{'collected'} = $dt;
        }
    }

    if (my $depth = $rec->{'depth_max'}) {
        if ($depth =~ $RE{'num'}{'int'} || $depth =~ $RE{'num'}{'real'}) {
            $sample{'depth'} = $depth + 0.0; # force number
        }
    }

    printf "%6d: %s\n", ++$i, $sample{'sample'};
    $coll->replace_one(
        { sample => $sample{'sample'} }, 
        \%sample,
        { upsert => 1 }
    );
}

say "Done, processed $i samples.";
