/*  vi: set et sw=4 ts=4:   */

class puppet {
    package { "puppet":
        ensure => installed,
        name   => [ "puppet", "libaugeas-ruby1.8", "libselinux-ruby1.8" ],
    }

    file {
        "/etc/puppet/puppet.conf":
            group   => "puppet",
            mode    => "640",
            owner   => "puppet",
            require => Package["puppet"],
            source  => "puppet:///modules/puppet/puppet.conf";

        "/etc/puppet/namespaceauth.conf":
            group   => "puppet",
            mode    => "640",
            owner   => "puppet",
            require => Package["puppet"],
            source  => "puppet:///modules/puppet/namespaceauth.conf";
    }

    service { "puppet":
        enable     => true,
        ensure     => running,
        hasrestart => true,
        hasstatus  => true,
        require    => File["/etc/puppet/puppet.conf", "/etc/puppet/namespaceauth.conf"],
        subscribe  => File["/etc/puppet/puppet.conf", "/etc/puppet/namespaceauth.conf"],
    }

    exec { "sed -i -e 's/^\\s*START\\s*=\\s*['\\''\"]\\?no.*/START=yes/i' /etc/default/puppet":
        before  => Service["puppet"],
        onlyif  => "grep -q '^\\s*START\\s*=\\s*['\\''\"]\\?no' /etc/default/puppet",
        require => Package["puppet"],
    }
}

