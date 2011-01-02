#!/bin/sh
#
#   http://www.gnu.org/software/gettext/manual/gettext.html#sh
#
# Generate .pot:
#   xgettext -d i18n-example -L Shell --from-code=UTF-8 i18n-example.sh
#   mv i18n-example.po i18n-example.pot
#
# Generate .po:
#   msginit -l zh_CN.UTF-8 -i i18n-example.pot
#
# Generate .mo:
#   msgfmt -o i18n-example.mo zh_CN.po
#   mkdir -p zh_CN/LC_MESSAGES
#   mv i18n-example.mo zh_CN/LC_MESSAGES
#
# Use .mo:
#   (unset LANGUAGE; LC_MESSAGES=zh_CN ./i18n-example.sh)
#   LANGUAGE=zh_CN:zh:en_US:en ./i18n-example.sh

. gettext.sh

error() {
    echo "$@" >&2
}

TEXTDOMAIN=i18n-example
TEXTDOMAINDIR="`pwd`"
export TEXTDOMAIN TEXTDOMAINDIR

#echo "OK"
gettext "OK"; echo

#echo "3 files"
echo "3 `ngettext \"file\" \"files\" 3`"

program_name=$0
#echo "Usage: $program_name [option] FILE..."
eval_gettext "Usage: \$program_name [option] FILE..."; echo

filecount="`ls | wc -l`"
#[ $filecount gt 1 ] && echo "Remaining files: $filecount" || echo "Remaining file: $filecount"
eval_ngettext "Remaining file: \$filecount" "Remaining files: \$filecount" $filecount; echo

filename=xxx
#error "file not found: $filename"
#error "`echo \"file not found: \$filename\"`"
error "`eval_gettext \"file not found: \\\$filename\"`"

