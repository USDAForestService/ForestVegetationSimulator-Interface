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

// Refocus "eltid"
Shiny.addCustomMessageHandler("refocus",
  function(eltid) { document.getElementById(eltid).focus(); }
);


// This gets the cursor postion from eltid
Shiny.addCustomMessageHandler("getStart",
  function(eltid) 
  {
    if (document.getElementById(eltid)) 
    {
      document.getElementById(eltid).onmouseout = function() 
      { 
        Shiny.onInputChange("selectionStart",  document.getElementById(eltid).selectionStart); 
        Shiny.onInputChange("selectionEnd",  document.getElementById(eltid).selectionEnd); 
      }
    }
  }
);

})();


