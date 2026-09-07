$(function () {
  Shiny.addCustomMessageHandler('resetOptionalFiles', function (ids) {
    ids.forEach(function (id) {
      var el = document.getElementById(id);
      if (!el) return;
      el.value = '';
      var container = $(el).closest('.shiny-input-container');
      container.find('input[type=text]').val('');
      container.find('.progress').css('visibility', 'hidden');
    });
  });
});
