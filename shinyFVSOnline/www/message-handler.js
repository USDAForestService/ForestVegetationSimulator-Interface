(function() {

// This recieves messages of type "dialogContentUpdate" from the server.
Shiny.addCustomMessageHandler("dialogContentUpdate",
  function(data) {
    $('#' + data.id).find(".modal-body").html(data.message);
  }
);

// This recieves messages of type "infomessage" from the server.
Shiny.addCustomMessageHandler("infomessage", 
  function(message) { alert(message); }
);


})();
