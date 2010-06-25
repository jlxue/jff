(function() {
    function elem(id) {
        return document.getElementById(id);
    }

    function tags(name) {
        return document.getElementsByTagName(name);
    }

    function attr(node, name, value) {
        if (value) {
            node.setAttribute(name, value)
            return node;
        } else {
            return node.getAttribute(name);
        }
    }

    var dialog = elem('markit-dialog');
    if (dialog) {
        // markit.js must load jQuery and jQuery UI first, then create dialog
        jQuery(dialog).toggle();
        return;
    }

    var id = 'markit-script';
    var msg = 'Loading MarkIt dialog, please try again later.';
    var markit_root     = '##MARKIT_ROOT##';
    var markit_url      = markit_root + '##MARKIT_URL##';
    var jquery_url      = markit_root + '##JQUERY_URL##';
    var jquery_ui_url   = markit_root + '##JQUERY_UI_URL##';
    var ckeditor_url    = markit_root + '##CKEDITOR_URL##';

    if (elem(id)) {
        alert(msg);
        return;
    } else {
        var scripts = tags('script');

        for (var i = 0; i < scripts.length; ++i) {
            if (markit_url == attr(scripts[i], 'src')) {
                alert(msg);
                return;
            }
        }
    }

    var s = document.createElement('script');
    attr(attr(attr(attr(attr(attr(s,
                'src', markit_url),
                'j', jquery_url),
                'u', jquery_ui_url),
                'c', ckeditor_url),
                'charset', 'UTF-8'),
                'id', id);
    s.ontimeout = s.onerror = function() {
        s.parentNode.removeChild(s);
        alert('Can\'t load ' + markit_url);
    }

    tags('body')[0].appendChild(s);
})()

