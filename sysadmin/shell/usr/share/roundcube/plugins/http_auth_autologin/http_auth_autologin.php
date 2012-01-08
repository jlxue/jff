<?php

/**
 * Use HTTP authentication especially SPNEGO to decide
 * real user name and automatically login.
 *
 * Reference:
 *  autologon plugin
 *  http_authentication plugin
 *  https://github.com/aimxhaisse/roundcube-mod-auth-openid
 *  http://trac.roundcube.net/ticket/1486689
 *
 * @version 0.1
 * @author Yubao Liu <yubao.liu@gmail.com>
 */
class http_auth_autologin extends rcube_plugin
{
    public $task = 'login';

    function init()
    {
        if (isset($_SERVER['REMOTE_USER']) && isset($_SERVER['AUTH_TYPE'])) {
            $rcmail = rcmail::get_instance();

            $this->load_config();

            $this->add_hook('startup', array($this, 'startup'));
            $this->add_hook('authenticate', array($this, 'authenticate'));

            if ($rcmail->config->get('imap_auth_master_user_separator')) {
                $this->add_hook('user_create', array($this, 'user_create'));
            }
        }
    }

    function startup($args)
    {
        # For SPNEGO, AUTH_TYPE is "Negotiate".
        if (empty($args['action']) && empty($_SESSION['user_id']) &&
                !empty($_SERVER['REMOTE_USER']) &&
                !empty($_SERVER['AUTH_TYPE'])) {
            $args['action'] = 'login';
        }

        return $args;
    }

    function authenticate($args)
    {
        if (!empty($_SERVER['REMOTE_USER']) && !empty($_SERVER['AUTH_TYPE'])) {
            $rcmail = rcmail::get_instance();

            # convert user@REALM to user
            $pos = strpos($_SERVER['REMOTE_USER'], '@');
            if ($pos === false) {
                $args['user'] = $_SERVER['REMOTE_USER'];
            } else {
                $args['user'] = substr($_SERVER['REMOTE_USER'], 0, $pos);
            }

            # Dovecot's master user feature
            $separator = $rcmail->config->get('imap_auth_master_user_separator');
            if ($separator) {
                $master_user = $rcmail->config->get('imap_auth_master_username');
                if ($master_user) {
                    # For dovecot master user
                    $args['user'] .= $separator . $master_user;
                }
            }

            $args['pass'] = $rcmail->config->get('imap_auth_master_password');

            $args['cookiecheck'] = false;
            $args['valid'] = true;
        }

        return $args;
    }

    function user_create($args)
    {
        $rcmail = rcmail::get_instance();
        $mail_domain = $rcmail->config->get('mail_domain');
        $separator = $rcmail->config->get('imap_auth_master_user_separator');

        $pos = strpos($args['user'], $separator);
        if ($pos === false) {
            $user = $args['user'];
        } else {
            $user = substr($args['user'], 0, $pos);
        }

        $args['user_name'] = $user;

        if ($mail_domain) {
            $args['user_email'] = $user . '@' . $mail_domain;
        }

        return $args;
    }
}

