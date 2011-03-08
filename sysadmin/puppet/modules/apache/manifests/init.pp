/*  vi: set et sw=4 ts=4:   */

class apache {
    package { "apache2":
        ensure => installed,
    }

    service { "apache2":
        enable     => true,
        ensure     => running,
        hasrestart => true,
        hasstatus  => true,
        require    => Package["apache2"],
    }

    define module($ensure = true, $require_package = "apache2") {
        if $require_package != "apache2" {
            package { $require_package:
                before  => Service["apache2"],
                ensure  => installed,
                require => Package["apache2"],
            }
        }

        $load_file = "/etc/apache2/mods-enabled/${name}.load"

        # a2enmod/a2dismod/a2ensite/a2dissite will reload or restart apache2
        # so don't require "notify" attribute
        case $ensure {
            true,
            present: {
                exec { "/usr/sbin/a2enmod $name":
                    before  => Service["apache2"],
                    creates => $load_file,
                    group   => "root",
                    require => [ Package["apache2"], Package[$require_package] ],
                    user    => "root",
                }
            }

            default: {
                exec { "/usr/sbin/a2dismod $name":
                    before  => Service["apache2"],
                    group   => "root",
                    onlyif  => "test -e '$load_file'",
                    require => [ Package["apache2"], Package[$require_package] ],
                    user    => "root",
                }
            }
        } # end case
    } # end define

    define site($ensure = true,  $content = undef, $source = undef) {
        $site_file = "/etc/apache2/sites-enabled/$name"
        $avail_site = "/etc/apache2/sites-available/$name"

        if $content or $source {
            file { $avail_site:
                content => $content,
                group   => "root",
                #notify  => Service["apache2"],
                owner   => "root",
                source  => $source,
            }
        }

        # a2enmod/a2dismod/a2ensite/a2dissite will reload or restart apache2
        # so don't require "notify" attribute
        case $ensure {
            true,
            present: {
                exec { "/usr/sbin/a2ensite $name":
                    before  => Service["apache2"],
                    creates => $site_file,
                    group   => "root",
                    require => [ Package["apache2"], File[$avail_site] ],
                    user    => "root",
                }

                File[$avail_site] {
                    notify +> Service["apache2"],
                }
            }

            default: {
                exec { "/usr/sbin/a2dissite $name":
                    before  => Service["apache2"],
                    group   => "root",
                    onlyif  => "test -e '$site_file'",
                    require => Package["apache2"],
                    user    => "root",
                }
            }
        } # end case
    } # end define
                
}

