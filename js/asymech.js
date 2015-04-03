function load() {
    $('nav a').parent().removeClass("active");
    request = "home.html";
    if (location.hash) {
        request = location.hash.substring(1) + ".html";
        $('nav a[href="' + location.hash + '"]').parent().addClass("active");
    }
    $.ajax({
        method: "GET",
        url: request.toString(),
        contentType: "text/html; charset=utf-8",
        dataType: "html",
        success: function(content, status, jqXHR) {
            $("#content").html(content);
        },
        error: function(jqXHR, status, error) {
            notify("warning", "Seite '" + request.toString() + "' konnte nicht korrekt geladen werden. Lade Homepage...");
            if (request.toString() != "home.html") {
                load('home.html');
            }
        }
    });
}

function notify(type, msg) {
    d=document.createElement('div');
    $(d).addClass("alert alert-" + type)
        .text(msg)
        .appendTo($("#notify"))
        .click(function(){$(this).remove();})
        .hide()
        .slideToggle(300)
        .delay(20000)
        .slideToggle(300)
        .queue(function() {$(this).remove();});
}

$(document).ready(load());
$(window).on("hashchange", function(event) { load(); });
