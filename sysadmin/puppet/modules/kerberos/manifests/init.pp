/*  vi: set et sw=4 ts=4:   */

class kerberos {
    package { "kerberos-client":
        name => [ "krb5-user", "krb5-clients" ],
        ensure => installed,
    }

    # TODO: setup default realm
}

