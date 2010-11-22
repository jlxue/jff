# On Debian Squeeze, run with root privilege
aptitude install apache2 apache2-mpm-prefork apache2-suexec \
    libapache2-mod-fcgid libapache2-mod-perl2
a2enmod userdir
a2enmod suexec
