(function($) {
    $(function(){
        $("#timeline").sortable({ cursor: "crosshair" , items: "td", update: function(event, ui) {
            update_timeline(0);
        }});


        $("#timeline td").live("click",
            {
                parent: "#timeline",
                children: "#timeline td",
                selectedClass: "ui-selected",
            }, onSelect);


        $(document).keydown(function(event) {
            if (event.which == 16) {    // shift key
                $("#timeline .last-clicked-item").addClass("last-clicked");
            }
        });

        $(document).keyup(function(event) {
            if (event.which == 16) {    // shift key
                $("#timeline .last-clicked-item").removeClass("last-clicked");
            }
        });


        $("#timeline td input").live("change", function() {
            var td = $(this).parent("td")[0];

            if (this.value != formatTime(parseTime(this.value))) {
                $(this).addClass("error-time-format");
                alert("时间格式必须是 HH:MM:SS！");
            } else {
                $(this).removeClass("error-time-format");
                update_timeline(find_td_index(td));
            }
        });


        var timeline_item = '<td>持续时间：<input type="text" size="8" value="00:05:00"/><br/>' +
            '开始时间：<span>00:00:00</span><br/>' + '<img src="empty.png"/></td>';

        $("#btn_add").click(function() {
            $("#timeline").append(timeline_item);
            update_timeline($("#timeline span").length - 1);
        });

        $("#timeline").append(timeline_item);


        $("#btn_remove").click(function() {
            var tds = $("#timeline .ui-selected");

            if (tds.length > 0) {
                tds.remove();
                update_timeline(0);
            } else {
                alert("请先选择场景！");
            }
        });
    });


    function find_td_index(td) {
        var tds = $("#timeline td");

        for (var i = 0; i < tds.length; ++i) {
            if (td === tds[i]) {
                return i;
            }
        }

        return -1;
    }


    function update_timeline(begin) {
        var inputs = $("#timeline input");
        var spans = $("#timeline span");
        var time = 0;

        if (begin < 0 || begin >= spans.length)
            return;

        if (begin > 0) {
            time = parseTime(inputs[begin - 1].value) + parseTime(spans[begin - 1].textContent);
        }

        while (begin < spans.length) {
            spans[begin].textContent = formatTime(time);

            time += parseTime(inputs[begin].value);

            ++begin;
        }
    }


    function formatTime(time) {
        var hour = Math.floor(time / 60 / 60);
        var minute;
        var second;

        time -= hour * 60 * 60;
        minute = Math.floor(time / 60);
        second = time - minute * 60;

        return (formatTwoDigits(hour) + ":" +
            formatTwoDigits(minute) + ":" +
            formatTwoDigits(second));
    }


    function formatTwoDigits(d) {
        if (d >= 10)
            return d;
        else
            return "0" + d;
    }


    function parseTime(str) {
        var a = str.match(/^\s*(\d\d?):(\d\d?):(\d\d?)\s*$/);

        if (a) {
            return a[1] * 60 * 60 + a[2] * 60 + a[3] * 1;
        } else {
            return 0;
        }
    }


    function onSelect(event) {
        var td = $(this);
        var last_clicked = $(event.data.parent).find(".last-clicked-item");
        if (last_clicked.length > 0)
            last_clicked_td = last_clicked[0];
        else
            last_clicked_td = null;


        if (event.ctrlKey) {
            // ctrl key pressed
            if (event.shiftKey) {
                // ctrl and shift keys pressed, unselect items between current one and last clicked one
                if (last_clicked_td) {
                    var selected_tds = select_range($(event.data.children), last_clicked_td, this);

                    for (var i = 0; i < selected_tds.length; ++i) {
                        $(selected_tds[i]).removeClass(event.data.selectedClass);
                    }
                } else {
                    td.removeClass(event.data.selectedClass);
                }

                $(event.data.parent).find(".last-clicked-item").removeClass("last-clicked");

            } else {
                // ctrl key pressed, not shift key, toggle current item
                td.toggleClass(event.data.selectedClass);
            }

        } else if (event.shiftKey) {
            // shift key pressed, not ctrl key, select items between current one and last clicked one
            if (last_clicked_td) {
                var selected_tds = select_range($(event.data.children), last_clicked_td, this);

                for (var i = 0; i < selected_tds.length; ++i) {
                    $(selected_tds[i]).addClass(event.data.selectedClass);
                }
            } else {
                td.addClass(event.data.selectedClass);
            }

            $(event.data.parent).find(".last-clicked-item").removeClass("last-clicked");

        } else {
            // no ctrl or shift key pressed, select current item, unselect others
            $(event.data.children).removeClass(event.data.selectedClass);
            td.addClass(event.data.selectedClass);
        }


        last_clicked.removeClass("last-clicked-item");
        td.addClass("last-clicked-item");
    }


    function select_range(children, begin_child, end_child) {
        var begin = -1;
        var end = -1;

        for (var i = 0; i < children.length; ++i) {
            if (children[i] === begin_child) {
                begin = i;
                break;
            }
        }

        for (var i = 0; i < children.length; ++i) {
            if (children[i] === end_child) {
                end = i;
                break;
            }
        }

        if (begin < 0 || end < 0)
            return [];

        if (begin > end) {
            var temp = end;
            end = begin;
            begin = temp;
        }

        var selected = [];
        for (var i = begin; i <= end; ++i) {
            selected.push(children[i]);
        }

        return selected;
    }
})(jQuery);

