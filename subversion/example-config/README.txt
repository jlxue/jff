Tested on Windows XP with:
    apache_2.2.11-win32-x86-openssl-0.9.8i.msi
    Setup-Subversion-1.6.2.msi
    TortoiseSVN-1.6.3.16613-win32-svn-1.6.3.msi

Steps:
    * install apache, subversion
    * make sure PATH in system environment variables
      contains Subversion\bin
    * copy mod_*.so in Subversion\bin to Apache2.2\modules
    * restart Windows! (Because the PATH environment
      variable takes effect after reboot, mod_*.so requires
      some .dll files in Subversion\bin)
    * append next line to Apache2.2\conf\httpd.conf:

    Include conf/extra/httpd-svn.conf

See comments in httpd-svn.conf for more directives.

