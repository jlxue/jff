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

    var dialog = elem('markit-dialog'),
        id = 'markit-script',
        msg = 'Loading MarkIt dialog, please try again later.',
        markit_root     = '##MARKIT_ROOT##',
        markit_url      = markit_root + '##MARKIT_URL##',
        jquery_url      = markit_root + '##JQUERY_URL##',
        jquery_ui_url   = markit_root + '##JQUERY_UI_URL##',
        ckeditor_url    = markit_root + '##CKEDITOR_URL##',
        script;

    if (dialog) {
        // markit.js must load jQuery and jQuery UI first, then create dialog
        jQuery(dialog).toggle();
        return;
    }


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

    script = document.createElement('script');
    attr(attr(attr(attr(attr(attr(script,
                'src', markit_url),
                'j', jquery_url),
                'u', jquery_ui_url),
                'c', ckeditor_url),
                'charset', 'UTF-8'),
                'id', id);
    script.ontimeout = script.onerror = function() {
        script.parentNode.removeChild(script);
        alert('Can\'t load ' + markit_url);
    }

    tags('body')[0].appendChild(script);
})()

