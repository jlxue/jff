Tested on Windows XP with:
    apache_2.2.11-win32-x86-openssl-0.9.8i.msi
    Setup-Subversion-1.6.2.msi
    TortoiseSVN-1.6.3.16613-win32-svn-1.6.3.msi

Append next line to Apache2.2\conf\httpd.conf:
Include conf/extra/httpd-svn.conf

See comments in httpd-svn.conf for more directives.

