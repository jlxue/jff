/*  vi: set et sw=4 ts=4:   */

class ntp {
    package { "ntp":
        ensure => installed,
    }

    file { "/etc/ntp.conf":
        require => Package["ntp"],
    }

    service { "ntp":
        enable     => true,
        ensure     => running,
        hasrestart => true,
        hasstatus  => true,
        require    => Package["ntp"],
        subscribe  => File["/etc/ntp.conf"],
    }
}
