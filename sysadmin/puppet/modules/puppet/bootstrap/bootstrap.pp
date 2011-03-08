if $prefix {
    $dest = $prefix
} elsif $id == "root" {
    $dest = "/etc/puppet"
} elsif $home {
    $dest = "$home/puppet"
} else {
    $dest = "/home/$id/puppet"
}

notice("Target top directory: $dest")

if ! $modules {
    err("Please set FACTER_MODULES to absolute path of your modules!")
    err("For example: FACTER_MODULES=`pwd`/modules puppet modules/puppet/bootstrap/bootstrap.pp")
}


file { [ $dest, "$dest/modules" ]:
    ensure  => directory,
    mode    => "644",
}

File {
    ignore => [ ".*", "CVS", "bootstrap", "bootstrap.pp", "pkg", "tests" ],
}

define sync($source = undef, $purge = false) {
    if $source {
        $source2 = $source
        $dest2 = "$dest/$name"
    } else {
        $source2 = "$modules/$name"
        $dest2 = "$dest/modules/$name"
    }

    notice("Sync from $source2 to $dest2...")

    if $id == "root" {
        $owner = "puppet"
        $group = "puppet"
    } else {
        $owner = undef
        $group = undef
    }

    file { $dest2:
        force   => true,
        group   => $group,
        owner   => $owner,
        purge   => $purge,
        recurse => true,
        source  => $source2,
    }
}

sync { "manifests":
    source => "$modules/puppet/bootstrap",
}

sync { [ "generic", "ntp", "apache", "puppet" ]:
    purge  => true,
}

