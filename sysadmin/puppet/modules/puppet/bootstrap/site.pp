/*  vi: set et sw=4 ts=4:   */

# Default file parameters
File {
    ignore => [ '.svn', '.git', '.hg', '.bzr', 'CVS' ],
    owner  => "root",
    group  => "root",
    mode   => "644",
}

# Default exec parameters
Exec {
    path => ["/bin", "/sbin", "/usr/bin", "/usr/sbin"],
}

# By default when we talk about a package we want to install it
Package {
    ensure => installed,
}

# Default service parameters
Service {
    enable     => true,
    ensure     => running,
    hasrestart => true,
    hasstatus  => true,
}

node default {
    include generic
}

import "*.node"

