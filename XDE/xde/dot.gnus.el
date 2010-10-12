;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 收信的配置

(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.googlemail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)     ; ssl requires openssl, tls requires gnutls-cli
               ))

(setq gnus-secondary-select-methods
      '(
        (nnimap "yahoo-corp"
                (nnimap-address "kr1-webmail.corp.yahoo.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl))
        ))

;; 自动登录
;; 在 ~/.authinfo 写入此行，替换 USERNAME 和 PASSWORD:
;; machine imap.googlemail.com login USERNAME@gmail.com password PASSWORD port 993

;; 允许 [Gmail]/... 这样的邮箱名字
;(setq gnus-ignored-newsgroup "...")

;; 下载邮件到本地再阅读
;(setq mail-source-primary-source)
;(setq mail-sources)

;; 保留 pop3 服务器上备份
(setq pop3-leave-mail-on-server t)

;; 异步预取邮件
(setq gnus-asynchronous t)

;; 定时检查邮件
(defun gnus-demon-update-group-buffer ()
  "Scan for new mail/news and update the *Group* buffer"
  (when (gnus-alive-p)
    (save-window-excursion
      (save-excursion
        (set-buffer gnus-group-buffer)
        (gnus-group-get-new-news)))))

(setq gnus-use-demon t)
(gnus-demon-init)
(gnus-compile)
(gnus-demon-add-handler 'gnus-demon-update-group-buffer 2 nil)
(gnus-demon-add-handler 'gnus-demon-scan-mail 20 t)
(gnus-demon-add-handler 'gnus-demon-scan-news 20 t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 发信的配置

(setq message-send-mail-function 'message-smtpmail-send-it)

(setq smtpmail-auth-credentials
      '(
        ("smtp.googlemail.com" 465 "Yubao.Liu@gmail.com" nil)
        ("smtp.126.com"        25  "dieken" nil)
        ("smarthost.yahoo.com" 25  "liuyb" nil)
        )

      smtpmail-starttls-credentials
      '(
        ("smtp.googlemail.com" 465 nil nil)
        ))

(setq gnus-posting-styles
      '(
        (".*"
         (name "Yubao Liu")
         (address "Yubao.Liu@gmail.com")
         (signature (lambda () (shell-command-to-string "fortune")))
         (eval (setq smtpmail-smtp-server "smtp.googlemail.com"
                     smtpmail-smtp-service 465)
               (message "Use %s:%d to send mail." smtpmail-smtp-server smtpmail-smtp-service)))

        ("^nnmaildir+126"
         (name "dieken")
         (address "dieken@126.com")
         (eval (setq smtpmail-smtp-server "smtp.126.com"
                     smtpmail-smtp-service 25)
               (message "Use %s:%d to send mail." smtpmail-smtp-server smtpmail-smtp-service)))

        ("^nnimap+yahoo-corp"
         (address "liuyb@yahoo-inc.com")
         (eval (setq smtpmail-smtp-server "smarthost.yahoo.com"
                     smtpmail-smtp-service 25)
               (message "Use %s:%d to send mail." smtpmail-smtp-server smtpmail-smtp-service)))
        ))

;; 拼写检查
(add-hook 'message-send-hook 'ispell-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 地址簿

(require 'bbdb)     ; 需要安装 bbdb 软件包，否则加载会失败！
(bbdb-initialize 'gnus 'message)

;; 不检查是否符合美国电话号码标准
(setq bbdb-north-american-phone-numbers-p nil)

(setq bbdb-user-mail-names
      (regexp-opt '("Yubao.Liu@gmail.com"
                    "dieken@126.com")))

;; 自动添加联系人
(setq bbdb/news-auto-create-p t)


;; 补全 email 地址的时候循环往复
;(setq bbdb-complete-name-allow-cycling t)
;; No popup-buffers
;(setq bbdb-use-pop-up nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 杂项

;;设置标题行样式
;(setq gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)%l %O\n")
;(setq gnus-summary-line-format ":%U%R %B %s %-100=|%3L|%-20,20n|%&user-date;\n")

;; 让邮件列表中的新邮件显示在上面
(setq gnus-thread-sort-functions
      '(
        (not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)
        ))

;; 发信时确认，防止发送没写完的邮件
(setq message-confirm-send t)

;; 防止在新闻组中将 f 按成 r，对邮件列表也要求确认
(setq gnus-confirm-mail-reply-to-news t
      gnus-confirm-treat-mail-like-news t)

;; 将邮件的发出时间转换成本地时间
(add-hook 'gnus-article-prepare-hook 'gnus-article-date-local)

;; 只让 * 键使用 cache
(setq gnus-use-cache 'passive)

;; 查看邮件的编码以及发送者的邮件客户端
(add-hook 'gnus-startup-hook
          '(lambda ()
             (setq gnus-visible-headers
                   (concat "^User-Agent:\\|^Content-Type:\\|"
                           "Content-Transfer-Encoding:\\|"
                           "^X-mailer:\\|^X-Newsreader:\\|^X-Sender:\\|"
                           gnus-visible-headers))))

;; 设置信件自动删除的时间为一个月
(setq nnmail-expiry-wait 30)

