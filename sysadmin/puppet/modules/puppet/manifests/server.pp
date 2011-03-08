/*  vi: set et sw=4 ts=4:   */

class puppet::server inherits puppet {
    include apache

    package { "puppetmaster":
        ensure => installed,
    }

    apache::module {
        "passenger":
            ensure          => true,
            require_package => "libapache2-mod-passenger";
        "ssl":
            ensure => true;
        "headers":
            ensure => true;
    }
            
    apache::site { "puppetmasterd":
        content => template("puppet/puppetmasterd.site.erb"),
        ensure  => true,
        require => Package["puppetmaster", "libapache2-mod-passenger"],
    }

    file {
        "/etc/puppet/fileserver.conf":
            before  => Service["apache2"],
            group   => "puppet",
            mode    => "640",
            owner   => "puppet",
            require => Package["puppetmaster"],
            source  => "puppet:///modules/puppet/fileserver.conf";

        "/usr/share/puppet/rack/puppetmasterd/config.ru":
            before  => Service["apache2"],
            group  => "puppet",
            owner  => "puppet",
            require => Package["puppetmaster"];

        "/etc/puppet/auth.conf":
            before  => Service["apache2"],
            group   => "puppet",
            mode    => "640",
            owner   => "puppet",
            require => Package["puppetmaster"],
            source  => "puppet:///modules/puppet/auth.conf";
    }

    File["/etc/puppet/puppet.conf"] {
        source => "puppet:///modules/puppet/master-puppet.conf",
    }

    service { "puppetmaster":
        enable    => false,
        ensure    => stopped,
        hasstatus => true,
        require   => Package["puppetmaster"],
    }

    exec { "sed -i -e 's/^\\s*START\\s*=\\s*['\\''\"]\\?yes.*/START=no/i' /etc/default/puppetmaster":
        before  => Service["apache2"],
        onlyif  => "grep -q '^\\s*START\\s*=\\s*['\\''\"]\\?yes' /etc/default/puppetmaster",
        require => Package["puppetmaster"],
    }
}

