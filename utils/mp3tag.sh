#!/bin/bash
find . -iname "*.mp3" -exec dir mid3iconv -e GBK {} +

