;; 新闻组服务器
(setq gnus-select-method '(nntp "news.cn99.com"))
;(setq gnus-select-method '(nntp "news.yaako.com"))

;; 设置邮件存放地
(setq gnus-secondary-select-methods
      '((nnmaildir ""
                   (directory "~/maildirs"))))

;; 默认的订阅组
; (setq gnus-default-subscribed-newsgroups
;      '("gnu.emacs.help"
;        "cn.comp.os.linux"
;        "cn.bbs.comp.emacs"))

;; 将发往新闻组的信和其它信件分开存放
(setq gnus-message-archive-group
      '((if (message-news-p)
          "nnml:sent.news"
          "nnml:sent.mail")))

;; 保存感兴趣的文章到对应的nnml组
;; 有时看到好帖子，想保存下来，这里有个好法子。
(defun my-archive-article (&optional n)
  "Copies one or more article(s) to a corresponding `nnml:' group, e.g.
  `gnus.ding' goes to `nnml:1.gnus.ding'. And `nnml:List-gnus.ding' goes
  to `nnml:1.List-gnus-ding'.
  Use process marks or mark a region in the summary buffer to archive
  more then one article."
  (interactive "P")
  (let ((archive-name
          (format
            "nnfolder:collection.%s"
            (if (featurep 'xemacs)
              (replace-in-string gnus-newsgroup-name "^.*:" "")
              (replace-regexp-in-string "^.*:" "" gnus-newsgroup-name)))))
    (gnus-summary-copy-article n archive-name)))
;; add it to summary-mode-map
(define-key gnus-summary-mode-map [?\C-c ?\M-s] 'my-archive-article)
;; 在好文章标题上按C-c M-s，就会自动保存到本地的对应文件夹。
;; 这里用的是nnfolder方法，可以在Group Buffer中按^订阅你的收藏夹

;; 设置信件自动删除的时间为一个月
(setq nnmail-expiry-wait 30)

;; 中文环境
(set-language-environment 'Chinese-GB)
(setq gnus-default-charset 'chinese-iso-8bit
      gnus-group-name-charset-group-alist '((".*" . chinese-iso-8bit))
      gnus-summary-show-article-charset-alist
      '((1 . chinese-iso-8bit)
        (2 . gbk)
        (3 . big5)
        (4 . utf-8))
      gnus-newsgroup-ignored-charsets
      '(unkown-8bit x-unkown iso-8859-1))

;; 只显示 html 文件的文本部分
(eval-after-load "mm-decode"
                 '(progn
                    (add-to-list 'mm-discouraged-alternatives "text/html")
                    (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;; 使用 * 键保存有价值的信件
(setq gnus-use-cache 'passive)

;; Unconditionally read the dribble file.
(setq gnus-always-read-dribble-file t)

;; 使用 bbdb
(require 'bbdb)
(bbdb-initialize 'gnus 'message)
;; bbdb自己检查你填写的电话是否符合北美标准，
;; 如果你不是生活在北美，应该取消这种检查
(setq bbdb-north-american-phone-numbers-p nil)
;; 把你的 email 地址告诉bbdb
(setq bbdb-user-mail-names
      (regexp-opt '("yubao.liu@gmail.com"
                    "dieken@126.com")))
;; 补全 email 地址的时候循环往复
;(setq bbdb-complete-name-allow-cycling t)
;; No popup-buffers
;(setq bbdb-use-pop-up nil)

;;设置标题行样式
(setq gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)%l %O\n")
(setq gnus-summary-line-format
      ":%U%R %B %s %-100=|%3L|%-20,20n|%&user-date;\n")

;; 不取回已读的新闻标题
;; 这样想看老文章的时候可以用A T或者^，前者取回当前话题的
;; 所有文章，后者取回相关话题的前一篇。
;; fetch old，no.
;(setq gnus-fetch-old-headers nil)

;; 给文章排序
;; 默认新文章在最下面，不合有些人的习惯，改改好了。
;; summary buffer sorting
(setq gnus-thread-sort-functions
      '(
        (not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)
        ))

;; 防止在新闻组中将 f 按成 r
(setq gnus-confirm-mail-reply-to-news t)

;; 发表文章时开启 autofill mode
(add-hook 'message-mode-hook
          (function (lambda ()
                      (setq fill-column 70)
                      (auto-fill-mode))))

;; 保留 pop3 服务器上备份
(setq pop3-leave-mail-on-server t)

;; 使用 msmtp 发信
(setq sendmail-program "/usr/bin/msmtp")

;; 用 fortune 做动态签名档
(defun my-fortune-signature ()
  (concat "Hello world:\n\n"
          (shell-command-to-string "fortune")))

(setq gnus-posting-styles
      '((".*"
         (name "Liu Yubao")
         (address "yubao.liu@gmail.com")
         (signature my-fortune-signature )
         (eval (setq message-sendmail-extra-arguments '("-a" "gmail"))))
        ("126.*"
         (name "dieken")
         (address "dieken@126.com")
         (eval (setq message-sendmail-extra-arguments '("-a" "126"))))
        ("cn.bbs.*"
         (name "Dieken")
         (address "Dieken@newsmth.net"))))

;; 发信时确认，防止发送没写完的信
(defadvice message-send (around my-confirm-message-send)
           (if (yes-or-no-p "Really send message?")
             ad-do-it))
(ad-activate 'message-send)

;; 自动折行收到的信件
; (add-hook 'gnus-article-prepare-hook
;           (lambda ()
;             (setq fill-column 70)
;             (gnus-article-fill-long-lines)))

;; 将邮件的发出时间转换成本地时间
(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)

;; 去掉重复的邮件
(setq gnus-summary-ignore-duplicates t)
(setq gnus-suppress-duplicates t)

;; Use a second connection to grab the next article when I read one, so
;; I don't have to wait for it be downloaded.  
(setq gnus-asynchronous t)

(defun gnus-demon-update-group-buffer ()
  "Scan for new mail/news and update the *Group* buffer"
  (when (gnus-alive-p)  
    (save-window-excursion
      (save-excursion
        (set-buffer gnus-group-buffer)
        (gnus-group-get-new-news)))))

;; Initialize the Gnus daemon, check new mail every two minutes.
;; 每隔 20 分钟查一下信
(setq gnus-use-demon t)
(gnus-demon-init)
(gnus-compile)
(gnus-demon-add-handler 'gnus-demon-update-group-buffer 2 nil)
(gnus-demon-add-handler 'gnus-demon-scan-mail 20 t)
(gnus-demon-add-handler 'gnus-demon-scan-news 20 t)

;; see email's encoding and reader.
(add-hook 'gnus-startup-hook
          '(lambda () 
             (setq gnus-visible-headers 
                   (concat "^User-Agent:\\|^Content-Type:\\|"
                           "Content-Transfer-Encoding:\\|"
                           "^X-mailer:\\|^X-Newsreader:\\|^X-Sender:\\|" 
                           gnus-visible-headers))))

;; from /usr/share/doc/spamassassin/examples/gnus
;; F10 pipe the message as spam
(global-set-key [(f10)]
  '(lambda () (interactive) (shell-command "sa-learn --spam --single ")))

;; F9 pipe the message as ham
(global-set-key [(f9)]
  '(lambda () (interactive) (shell-command "sa-learn --ham --single ")))


