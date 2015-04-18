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
            notify("warning", "Seite '" + request.toString() + "' konnte nicht korrekt geladen werden.");
            notify("warning", "Entweder existiert die Datei nicht auf dem Server nicht oder die Internetverbindung fehlt.");
        }
    });
}

function sendNewBook() {
    var dataToBeSent = {};
    dataToBeSent.name = $('#bname>input').val();
    if (dataToBeSent.name == "") {
        notify("warning", "Es fehlt der Name des Buches. Bitte eingeben!");
        return;
    }
    dataToBeSent.autor = $('#bautor>input').val();
    if (dataToBeSent.autor == "") {
        notify("warning", "Es fehlt der Autor des Buches. Bitte eingeben!");
        return;
    }
    for (var i = 1; i <= 13; i++) {
        dataToBeSent["c" + i] = $('#c' + i + '>input').val();
        if (dataToBeSent["c" + i] == "") {
            notify("warning", "Es fehlt ein Charakterisierungswert. Bitte eingeben!");
            return;
        }
    }
    $.ajax({
        method: "POST",
        url: "cgi-bin/addBook",
        data: dataToBeSent,
        contentType: "text/plain",
        success: function(content, status, jqXHR) {
            switch(content) {
                case "S": notify("success", "Das Buch wurde hinzugefügt!"); break;
                case "R": notify("success", "Deine Eingabedaten wurden mit den bereits vorhandenen gemischt!"); break;
                case "P": notify("warning", "Es entstand ein Fehler beim Verarbeiten der Daten, wahrscheinlich weil einige Zahlen nicht richtig eingegeben wurden."); break;
                case "E": notify("danger", "Das Buch konnte aus serverseitigen Gründen nicht hinzugefügt werden."); break;
            }
        },
        error: function(jqXHR, status, error) {
            notify("danger", "Die Daten konnten aus (wahrscheinlich) verbindungstechnischen Gründen nicht hinzugefügt.");
        }
    });
    return;
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
