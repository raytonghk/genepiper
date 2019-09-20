$(document).ready(function() {

  $('.desc').each(function() {
    var content = $(this).html();
    
    html = '<div class = "showdesc">Show Description</div>' + '<div class = "desccontent">' + content + '<div class = "hidedesc">Hide Description</div></div><br>';

    $(this).html(html);
  });

  $('.showdesc').click(function(){
    $(this).next().toggle();
    
    html = ($(this).html() == "Show Description") ? "Hide Description" : "Show Description";
    $(this).html(html);
    return false;
  });
  
  $('.hidedesc').click(function(){
    $(this).parent().prev().click();
    return false;
  });
  
  $('.innernote').each(function(){
    var content = $(this).html();
    
    html = '<div class = "shownote">&nbsp; notes...</div>' + '<div class = "notecontent">' + content + '</div>';
    $(this).html(html);
  });
  
  $('.shownote').click(function(){
    $(this).next().toggle();
    return false;
  });
  
  $('.ref').each(function() {
    var content = $(this).html();
    
    html = '<div class = "showref">Show References</div>' + '<div class = "refcontent">' + content + '<div class = "hideref">Hide References</div></div><br>';
    
    $(this).html(html);
  });
  
  $('.showref').click(function(){
    $(this).next().toggle();
    
    html = ($(this).html() == "Show References") ? "Hide References" : "Show References";
    $(this).html(html);
    return false;
  });
  
  $('.hideref').click(function(){
    $(this).parent().prev().click();
    return false;
  });
})