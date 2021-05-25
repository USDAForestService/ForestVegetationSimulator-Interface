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
Shiny.addCustomMessageHandler("getStartEnd",
  function(eltid) 
  {
    if (document.getElementById(eltid)) 
    {
      document.getElementById(eltid).onmouseout = function() 
      { 
        Shiny.onInputChange("selectionStart", document.getElementById(eltid).selectionStart); 
        Shiny.onInputChange("selectionEnd",   document.getElementById(eltid).selectionEnd); 
      }
    }
  }
);

// This will attempt to open a new tab with the provided URL
// add: session$sendCustomMessage(type = "openURL",url) anywhere in the server code.
Shiny.addCustomMessageHandler("openURL",
  function (url) { window.open(url); }                                                 
);

// This will close the window, it causes onSessionEnded to be called as well.
// add: session$sendCustomMessage(type = "closeWindow"," ") anywhere in the server code.
Shiny.addCustomMessageHandler("closeWindow",
  function (dummy) { window.close(); }
);

// this function load causes a shiny variable "signalClosing" to be set to 1 if the
// browser is being closed for any reason. NB: return null will suppress the "do you
// really want to exit" dialog automatically created by the browser.
window.onbeforeunload = function(e)                                      
{
  Shiny.onInputChange("signalClosing", 1);
  return null;
};


// copy an items content to the clipboard.
Shiny.addCustomMessageHandler("copyEltToClipboard",
  function(eltid) 
  {
    const elt = document.getElementById(eltid);
    const selection = window.getSelection();                     
    selection.removeAllRanges();
    range = document.createRange();
    range.selectNodeContents(elt);  
    selection.addRange(range);
    document.execCommand('copy');                                              
    selection.removeAllRanges();
  }
);



// should work but doesn't because of a bug in javascript.  
//Shiny.addCustomMessageHandler("copyWebGLShapshotToClipboard",
//  function(eltid) 
//  {                                                                            
//    const elt = document.getElementById(eltid);
//    const myImageData = elt.firstChild.toDataURL();
//    const imgBlob = new Blob([myImageData], { type: "image/png;base64" }); 
//    const item = new ClipboardItem({[imgBlob.type]: imgBlob});
//    navigator.clipboard.write([item]);                                          
//  }
//);

Shiny.addCustomMessageHandler("copyWebGLSnapshotToClipboard",
  function(eltid)                      
  {                                                                       
    const elt = document.getElementById(eltid);
    const imageData = elt.firstChild.toDataURL('image/png', 1.0);
    var imageElt = document.createElement("IMG");
    imageElt.setAttribute("src", imageData);
    imageElt.setAttribute("alt", eltid);
    document.body.appendChild(imageElt);
    const selection = window.getSelection();   
    selection.removeAllRanges();
    range = document.createRange();
    range.selectNode(imageElt);
    selection.addRange(range);
    document.execCommand('copy');              
    selection.removeAllRanges();
    document.body.removeChild(imageElt);        
  }
);


Shiny.addCustomMessageHandler("makeTopSideImages",
  function(eltIds)                      
  { 
    var eltid=eltIds[0];            
    var topid=eltIds[1];                                                                     
    var sidid=eltIds[2];
    const elt  = document.getElementById(eltid);                        
    const top  = document.getElementById(topid);  
    const ssid = document.getElementById(sidid); 
    var rglinst = elt.rglinstance;
    if (rglinst == null) return; 
    var sid = rglinst.scene.rootSubscene;
    if (sid == null) return;
    let p3d = rglinst.getObj(sid).par3d;
    if (p3d == null) return;
    let cpy3d = JSON.parse(JSON.stringify(p3d));
    cpy3d.userMatrix.m11=1;
    cpy3d.userMatrix.m12=0;                                                                                               
    cpy3d.userMatrix.m13=0;
    cpy3d.userMatrix.m14=0;
    cpy3d.userMatrix.m21=0;                                              
    cpy3d.userMatrix.m22=0;
    cpy3d.userMatrix.m23=1;                                                                                               
    cpy3d.userMatrix.m24=0;                                                                                              
    cpy3d.userMatrix.m31=0;                                              
    cpy3d.userMatrix.m32=1;
    cpy3d.userMatrix.m33=0;                                             
    cpy3d.userMatrix.m34=0;                                             
    cpy3d.userMatrix.m41=0;
    cpy3d.userMatrix.m42=0;
    cpy3d.userMatrix.m43=0;
    cpy3d.userMatrix.m44=1;
    rglinst.getObj(sid).par3d = cpy3d;
    rglinst.drawScene();                                                   
    var imageData = elt.firstChild.toDataURL('image/png', 1.0);            
    top.setAttribute("src", imageData);                                     
    cpy3d.userMatrix.m22=1;
    cpy3d.userMatrix.m23=0;
    cpy3d.userMatrix.m32=0;                                             
    cpy3d.userMatrix.m33=1;                                                    
    rglinst.getObj(sid).par3d = cpy3d;
    rglinst.drawScene();                                               
    imageData = elt.firstChild.toDataURL('image/png', 1.0);
    ssid.setAttribute("src", imageData);
    rglinst.getObj(sid).par3d = p3d;
    rglinst.drawScene(); 
  } 
);


})();


                                                      