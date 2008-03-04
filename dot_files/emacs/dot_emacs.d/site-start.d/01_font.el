(if (and (boundp 'window-system) window-system)
  (if (not (string= emacs-version "23.0.60.1"))
    ;;; font setting for Emacs 22
    (progn (create-fontset-from-fontset-spec
             (concat
               "-bitstream-bitstream vera sans mono-medium-r-normal-*-14-*-*-*-*-*-fontset-my,"
               "chinese-gb2312:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-139-iso10646-1,"
               "chinese-big5-1:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-139-iso10646-1,"
               "chinese-big5-2:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-139-iso10646-1,"
               "mule-unicode-2500-33ff:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-139-iso10646-1,"
               "mule-unicode-e000-ffff:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-139-iso10646-1,"
               "mule-unicode-0100-24ff:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-cvs/139-iso10646-1,"
               "chinese-cns11643-1:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-139-iso10646-1,"
               "chinese-cns11643-2:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-139-iso10646-1,"
               "chinese-cns11643-3:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-139-iso10646-1,"
               "chinese-cns11643-4:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-139-iso10646-1,"
               "chinese-cns11643-5:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-139-iso10646-1,"
               "chinese-cns11643-6:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-139-iso10646-1,"
               "chinese-cns11643-7:-wenquanyi-wenquanyi bitmap song-medium-r-normal--14-100-100-100-p-139-iso10646-1"))

           ;; 当前frame使用此字体集
           (set-default-font "fontset-my")

           ;; 使C-x 5 2新开的frame也使用此字体集
           (setq default-frame-alist
                 (append 
                   '((font . "fontset-my")) default-frame-alist)))

    ;;; font setting for Emacs 23
    (progn (create-fontset-from-fontset-spec
             "-*-bitstream vera sans mono-medium-r-normal-*-14-*-*-*-*-*-fontset-my")

           (set-fontset-font
             "fontset-my" nil
             "-*-simsun-*-*-*-*-17-*-*-*-*-*-iso10646-1" nil 'prepend)
           (set-fontset-font
             "fontset-my" 'kana
             "-*-simsun-*-*-*-*-17-*-*-*-*-*-iso10646-1" nil 'prepend)
           (set-fontset-font
             "fontset-my" 'han
             "-*-simsun-*-*-*-*-17-*-*-*-*-*-iso10646-1" nil 'prepend)
           (set-fontset-font
             "fontset-my" 'cjk-misc
             "-*-simsun-*-*-*-*-17-*-*-*-*-*-iso10646-1" nil 'prepend)

           (set-default-font "fontset-my")
           (setq default-frame-alist
                 (append
                   '((font . "fontset-my")) default-frame-alist))

           ;(set-language-environment "chinese-gb18030")
           ;(prefer-coding-system 'chinese-gb18030)
           ;(prefer-coding-system 'utf-8)
           )))


