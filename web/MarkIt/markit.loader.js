(function() {
    var dialog = document.getElementById('markit-dialog');
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

    if (document.getElementById(id)) {
        alert(msg);
        return;
    } else {
        var scripts = document.getElementsByTagName('script');

        for (var i = 0; i < scripts.length; ++i) {
            if (markit_url == scripts[i].getAttribute('src')) {
                alert(msg);
                return;
            }
        }
    }

    var s = document.createElement('script');
    s.setAttribute('src', markit_url);
    s.setAttribute('j', jquery_url);
    s.setAttribute('u', jquery_ui_url);
    s.setAttribute('c', ckeditor_url);
    s.setAttribute('charset', 'UTF-8');
    s.setAttribute('id', id);
    s.ontimeout = s.onerror = function() {
        this.parentNode.removeChild(this);
        alert("Can't load " + markit_url);
    }
    document.getElementsByTagName('body')[0].appendChild(s);
})()

