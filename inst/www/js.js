$(document).ready(function(){

  $('#upload').change(upload);
  function upload(){
    var req=ocpu.call("upload",{
      path:$("#upload")[0].files[0]
    },function(session){
      session.getObject(function(obj){

      })
    }).fail(function() {alert("Error: " + req.responseText);})
  }
})


