(function(){
   $('#upload').change(upload);
  function upload(){
  var req=ocpu.call("upload",{
    path:$("#upload")[0].files[0]
  },function(session){
    console.log(session)
  }).fail(function() {alert("Error: " + req.responseText);})
}


})()
