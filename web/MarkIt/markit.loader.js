(function() {
    var dialog = document.getElementById('markit-dialog');
    if (dialog) {
        // markit.js must load jQuery and jQuery UI first, then create dialog
        jQuery(dialog).toggle();
        return;
    }

    var id = 'markit-script';
    var msg = 'Loading MarkIt dialog, please try again later.';
    var url = '##MARKIT_URL##';

    if (document.getElementById(id)) {
        alert(msg);
        return;
    } else {
        var scripts = document.getElementsByTagName('script');

        for (var i = 0; i < scripts.length; ++i) {
            if (url == scripts[i].getAttribute('src')) {
                alert(msg);
                return;
            }
        }
    }

    var s = document.createElement('script');
    s.setAttribute('src', url);
    s.setAttribute('charset', 'UTF-8');
    s.setAttribute('id', id);
    document.getElementsByTagName('body')[0].appendChild(s);
})()

